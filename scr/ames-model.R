# Machine Learning dengan Dataset Ames Housing
# Implementasi beberapa model untuk prediksi harga rumah

library(tidyverse)
library(performance)
library(tidymodels)
library(finetune)
library(skimr)

# Memuat dataset Ames Housing
ames <- read_csv("data/ames.csv")

# 1. Preprocessing Data
# Melihat struktur data
glimpse(ames)

# Skimming Data
skim_without_charts(ames)

# 2. Membagi data menjadi training dan testing

# Filter outlier
ames <- ames %>% 
  mutate(Log10Sale_Price = log10(Sale_Price)) %>% 
  filter(Gr_Liv_Area <= 4000 | Sale_Condition != "Partial") %>% 
  filter(Log10Sale_Price >= 4.5) %>% 
  select(-Longitude, -Latitude, 
         -Sale_Price, -Overall_Cond, 
         -MS_SubClass, -Pool_QC, -Pool_Area, 
         -Heating)

# Membuat Kategori Modernitas dan Kualitas Rumah
# Fungsi untuk modernitas
get_modernity <- function(x) {
  dplyr::case_when(
    x %in% c("VinylSd", "CemntBd", "MetalSd", "PreCast") ~ "Modern",
    x %in% c("Wd Sdng", "WdShing", "HdBoard", "Plywood", "Stucco", "ImStucc", "AsphShn") ~ "Traditional",
    TRUE ~ "Unknown"
  )
}

# Fungsi untuk kualitas
get_quality <- function(x) {
  dplyr::case_when(
    x %in% c("Stone", "BrkFace", "BrkComm") ~ "Premium",
    x %in% c("AsbShng", "CBlock", "Other") ~ "Low",
    TRUE ~ "Standard"
  )
}

combine_priority <- function(cat1, cat2, order) {
  pmin <- function(x, y) ifelse(match(x, order) < match(y, order), x, y)
  mapply(pmin, cat1, cat2)
}

modernity_order <- c("Modern", "Traditional", "Unknown")
quality_order <- c("Premium", "Standard", "Low", "Unknown")


set.seed(123)
ames_split <- initial_split(ames, prop = 0.8)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

# Data preprocessing dengan recipes
# Membuat recipe untuk preprocessing data
ames_recipe <- ames_train %>% 
  recipe(Log10Sale_Price ~ .) %>%
 
  # Menangani nilai yang hilang dalam prediktor numerik dengan median
  step_impute_median(all_numeric_predictors()) %>%
  # Menangani nilai yang hilang dalam prediktor kategori dengan modus
  step_impute_mode(all_nominal_predictors()) %>%
  step_mutate(Age = Year_Sold - Year_Built,
              After_Remod_Add = Year_Sold - Year_Remod_Add, 
              skip = FALSE) %>% 
  
  # Recode variable kategorik
  step_mutate(
    Neighborhood_Category = 
      case_when(Neighborhood %in% c("Stone_Brook", "Northridge_Heights", "Northridge", 
                                    "Green_Hills", "Veenker", "Timberland", "Somerset") ~ "High-end", 
                Neighborhood %in% c("Crawford", "College_Creek", "Greens", "Clear_Creek", 
                                    "Bloomington_Heights", "Gilbert", "Northwest_Ames", "Sawyer_West") ~ "Mid-range", 
                Neighborhood %in% c("Meadow_Village", "Briardale", "Iowa_DOT_and_Rail_Road", 
                                    "Old_Town", "Edwards", "Brookside", "Blueste", "Sawyer",  
                                    "South_and_West_of_Iowa_State_University", "Landmark", 
                                    "North_Ames", "Northpark_Villa", "Mitchell") ~ "Affordable", 
                TRUE ~ Neighborhood), 
    Neighborhood_Category = factor(Neighborhood_Category, 
                                   levels = c("Affordable", "Mid-range", "High-end")), 
    skip = FALSE) %>% 
  step_mutate(
    MS_Zoning = case_when(MS_Zoning %in% c("A_agr", "C_all", "I_all") ~ "Others", 
                          TRUE ~ MS_Zoning), 
    skip = FALSE) %>% 
  step_mutate(
    Lot_Shape = case_when(Lot_Shape %in% c("Irregular", "Moderately_Irregular") ~ "Irregular", 
                          TRUE ~ Lot_Shape), 
    skip = FALSE) %>% 
  step_mutate(
    Sale_Type = case_when(Sale_Type %in% c("WD", "CWD", "VWD") ~ "Warranty", 
                          Sale_Type %in% c("Con", "ConLI", "ConLw", "ConLD") ~ "Contract", 
                          Sale_Type %in% c("COD", "Oth") ~ "COD_Oth", 
                          Sale_Type == "New" ~ "New", 
                          TRUE ~ "Unknown"), 
    skip = FALSE) %>% 
  step_mutate(Total_Full_Bath = Full_Bath + Bsmt_Full_Bath, skip = FALSE) %>% 
  step_mutate(Total_Half_Bath = Half_Bath + Bsmt_Half_Bath, skip = FALSE) %>%  
  step_mutate(
    Modernity1 = get_modernity(Exterior_1st), 
    Modernity2 = get_modernity(Exterior_2nd), 
    Quality1 = get_quality(Exterior_1st), 
    Quality2 = get_quality(Exterior_2nd), 
    skip = FALSE) %>% 
  step_mutate(
    Modernity = combine_priority(Modernity1, Modernity2, order = modernity_order), 
    Quality = combine_priority(Quality1, Quality2, order = quality_order), 
    Quality = factor(Quality, levels = rev(quality_order)), 
    skip = FALSE) %>% 
  step_mutate(
    Has_Masonry = case_when(Mas_Vnr_Type == "None" | Mas_Vnr_Area == 0 ~ 0, 
                            TRUE ~ 1), 
    skip = FALSE) %>% 
  step_mutate(
    Fence = case_when(Fence == "No_Fence" ~ "No", 
                      str_detect(Fence, "^Good") ~ "Good", 
                      str_detect(Fence, "^Minimum") ~ "Minimum", 
                      TRUE ~ "unknown"), 
    skip = FALSE) %>% 
  step_mutate(
    Heating_QC = case_when(Heating_QC %in% c("Fair", "Poor") ~ "Poor_Fair", 
                            TRUE ~ Heating_QC) %>% 
      fct_relevel(c("Poor_Fair", "Typical", "Good", "Excellent")), 
    skip = FALSE
    ) %>% 
  
  
  # Menghapus variabel kategorik yang sudah tidak digunakan
  step_rm(Modernity1, Modernity2, Quality1, Quality2, Mas_Vnr_Area, 
          Exterior_1st, Exterior_2nd, 
          Mas_Vnr_Type, Full_Bath, Bsmt_Full_Bath, Half_Bath, Bsmt_Half_Bath, 
          Neighborhood, Year_Built, Year_Sold, Year_Remod_Add) %>% 
  # Mengubah variabel kategori menjadi dummy variables
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  # Menghapus prediktor dengan near zero variance
  step_nzv(all_predictors()) %>%
  # Normalisasi prediktor numerik
  step_normalize(all_numeric_predictors()) %>%
  # Korelasi yang tinggi dapat menyebabkan masalah dalam beberapa model
  step_corr(all_numeric_predictors(), threshold = 0.7)

ames_recipe

# Cross-validation data
ames_cv <- ames_train %>% 
  vfold_cv(v = 5)

# Mempersiapkan data dengan recipe
ames_prep <- prep(ames_recipe, training = ames_train)
ames_train_processed <- bake(ames_prep, new_data = ames_train)

glimpse(ames_train_processed)

# 3. Implementasi berbagai model machine learning

# Fungsi untuk evaluasi model
evaluate_model <- metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)

# A. Linear Regression
cat("\n--- Model Regresi Linear ---\n")
lm_spec <- linear_reg() %>% 
  set_engine("lm")

wf <- workflow(preprocessor = ames_recipe, 
               spec = lm_spec)

lm_model <- wf %>% 
  fit(data = ames_train)

lm_model %>% 
  extract_fit_engine() %>% 
  summary()

lm_model %>% 
  tidy() %>% 
  arrange(p.value) %>% 
  print(n = Inf)

# Plot Asumsi
lm_model %>% 
  extract_fit_engine() %>% 
  performance::check_model()
lm_model %>% 
  extract_fit_engine() %>% 
  performance::check_collinearity()
lm_model %>% 
  extract_fit_engine() %>% 
  performance::check_normality()
lm_model %>% 
  extract_fit_engine() %>% 
  performance::check_heteroscedasticity()
# lm_model %>% 
#   extract_fit_engine() %>% 
#   performance::check_homogeneity()

# Prediksi dengan model linear regression
lm_pred <- lm_model %>% 
  augment(new_data = ames_test) %>% 
  mutate(Sale_Price = 10^Log10Sale_Price, 
         .pred_price = 10^.pred, 
         .before = MS_Zoning)
lm_pred

lm_eval_log <- evaluate_model(lm_pred, truth = Log10Sale_Price, estimate = .pred)
lm_eval_log

lm_eval <- evaluate_model(lm_pred, truth = Sale_Price, estimate = .pred_price)
lm_eval
