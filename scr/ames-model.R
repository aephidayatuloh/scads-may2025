# Machine Learning dengan Dataset Ames Housing
# Implementasi beberapa model untuk prediksi harga rumah

library(tidyverse)
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
    Heating_QC = fct_recode(Heating_QC, 
                            Poor_Fair = "Fair", 
                            Poor_Fair = "Poor") %>% 
      fct_relevel("Poor_Fair", "Typical", "Good", "Excellent"), 
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
  step_corr(all_numeric_predictors(), threshold = 0.8)

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
evaluate_model <- metric_set(rmse, mae, rsq)

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

# Prediksi dengan model linear regression
lm_pred <- lm_model %>% 
  augment(new_data = ames_test)

lm_eval <- evaluate_model(lm_pred, truth = Sale_Price, estimate = .pred)


# B. Ridge Regression
cat("\n--- Model Ridge Regression ---\n")
# Menyiapkan matriks X dan y untuk glmnet
x_train <- as.matrix(ames_train_processed %>% select(-Sale_Price))
y_train <- ames_train_processed$Sale_Price
x_test <- as.matrix(ames_test_processed %>% select(-Sale_Price))
y_test <- ames_test_processed$Sale_Price

# Cross-validation untuk menemukan lambda 
library(glmnet)

cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
best_lambda <- cv_ridge$lambda.min
cat("Lambda optimal untuk Ridge:", best_lambda, "\n")

# Melatih model dengan lambda optimal
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)

# Prediksi dengan Ridge Regression
ridge_pred <- predict(ridge_model, newx = x_test)
ridge_eval <- evaluate_model(ridge_pred, y_test)
results <- rbind(results, data.frame(Model = "Ridge Regression", 
                                     RMSE = ridge_eval$RMSE, 
                                     MAE = ridge_eval$MAE, 
                                     R_squared = ridge_eval$R_squared))

# C. LASSO Regression
cat("\n--- Model LASSO Regression ---\n")
# Cross-validation untuk menemukan lambda optimal
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
best_lambda <- cv_lasso$lambda.min
cat("Lambda optimal untuk LASSO:", best_lambda, "\n")

# Melatih model dengan lambda optimal
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Fitur penting yang dipilih oleh LASSO
lasso_coef <- coef(lasso_model)
important_features <- rownames(lasso_coef)[which(lasso_coef != 0)]
cat("Jumlah fitur yang dipilih oleh LASSO:", length(important_features) - 1, "\n")
cat("Beberapa fitur penting yang dipilih oleh LASSO:", head(important_features[-1]), "...\n")

# Prediksi dengan LASSO Regression
lasso_pred <- predict(lasso_model, newx = x_test)
lasso_eval <- evaluate_model(lasso_pred, y_test)
results <- rbind(results, data.frame(Model = "LASSO Regression", 
                                     RMSE = lasso_eval$RMSE, 
                                     MAE = lasso_eval$MAE, 
                                     R_squared = lasso_eval$R_squared))

# D. Random Forest
cat("\n--- Model Random Forest ---\n")
# Mengatur cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Melatih model Random Forest dengan caret
rf_model <- train(
  Sale_Price ~ .,
  data = ames_train_processed,
  method = "rf",
  trControl = ctrl,
  importance = TRUE,
  ntree = 100
)

print(rf_model)

# Fitur penting menurut Random Forest
rf_importance <- varImp(rf_model)
print(rf_importance, top = 10)

# Prediksi dengan Random Forest
rf_pred <- predict(rf_model, newdata = ames_test_processed)
rf_eval <- evaluate_model(rf_pred, ames_test_processed$Sale_Price)
results <- rbind(results, data.frame(Model = "Random Forest", 
                                     RMSE = rf_eval$RMSE, 
                                     MAE = rf_eval$MAE, 
                                     R_squared = rf_eval$R_squared))

# E. XGBoost
cat("\n--- Model XGBoost ---\n")
# Menyiapkan data untuk XGBoost
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Parameter untuk XGBoost
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Melatih model XGBoost
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 0
)

# Fitur penting menurut XGBoost
xgb_importance <- xgb.importance(feature_names = colnames(x_train), model = xgb_model)
print(head(xgb_importance, 10))

# Prediksi dengan XGBoost
xgb_pred <- predict(xgb_model, dtest)
xgb_eval <- evaluate_model(xgb_pred, y_test)
results <- rbind(results, data.frame(Model = "XGBoost", 
                                     RMSE = xgb_eval$RMSE, 
                                     MAE = xgb_eval$MAE, 
                                     R_squared = xgb_eval$R_squared))

# 4. Perbandingan kinerja model
cat("\n--- Perbandingan Kinerja Model ---\n")
print(results)

# Visualisasi perbandingan model
ggplot(results, aes(x = reorder(Model, -R_squared), y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Perbandingan R-squared antar Model",
       x = "Model",
       y = "R-squared") +
  theme_minimal() +
  coord_flip()

ggplot(results, aes(x = reorder(Model, RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Perbandingan RMSE antar Model",
       x = "Model",
       y = "RMSE") +
  theme_minimal() +
  coord_flip()

# 5. Stacked Ensemble Model (Meta-learner)
cat("\n--- Stacked Ensemble Model ---\n")
# Membuat dataset untuk meta-learner
meta_features <- data.frame(
  lm_pred = predict(lm_model, newdata = ames_train_processed),
  ridge_pred = predict(ridge_model, newx = x_train),
  lasso_pred = predict(lasso_model, newx = x_train),
  rf_pred = predict(rf_model, newdata = ames_train_processed),
  xgb_pred = predict(xgb_model, dtrain),
  actual = ames_train_processed$Sale_Price
)

# Melatih meta-model (lm)
meta_model <- lm(actual ~ ., data = meta_features)
summary(meta_model)

# Membuat prediksi dari model-model dasar pada test set
meta_test_features <- data.frame(
  lm_pred = predict(lm_model, newdata = ames_test_processed),
  ridge_pred = predict(ridge_model, newx = x_test),
  lasso_pred = predict(lasso_model, newx = x_test),
  rf_pred = predict(rf_model, newdata = ames_test_processed),
  xgb_pred = predict(xgb_model, dtest)
)

# Prediksi final dengan meta-model
ensemble_pred <- predict(meta_model, newdata = meta_test_features)
ensemble_eval <- evaluate_model(ensemble_pred, y_test)
results <- rbind(results, data.frame(Model = "Stacked Ensemble", 
                                     RMSE = ensemble_eval$RMSE, 
                                     MAE = ensemble_eval$MAE, 
                                     R_squared = ensemble_eval$R_squared))

# Update perbandingan kinerja model
cat("\n--- Perbandingan Kinerja Model (dengan Ensemble) ---\n")
print(results)

# Visualisasi perbandingan model final
ggplot(results, aes(x = reorder(Model, -R_squared), y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Perbandingan R-squared antar Model",
       x = "Model",
       y = "R-squared") +
  theme_minimal() +
  coord_flip()

# 6. Visualisasi prediksi vs. aktual
# Mengambil hasil prediksi terbaik
best_model <- results$Model[which.max(results$R_squared)]
cat("\nModel terbaik berdasarkan R-squared:", best_model, "\n")

# Menggunakan hasil prediksi dari model terbaik
if(best_model == "Stacked Ensemble") {
  best_pred <- 10^ensemble_pred
} else if(best_model == "XGBoost") {
  best_pred <- 10^xgb_pred
} else if(best_model == "Random Forest") {
  best_pred <- 10^rf_pred
} else if(best_model == "LASSO Regression") {
  best_pred <- 10^as.vector(lasso_pred)
} else if(best_model == "Ridge Regression") {
  best_pred <- 10^as.vector(ridge_pred)
} else {
  best_pred <- 10^lm_pred
}

# Data aktual
actual_values <- 10^y_test

# Membuat dataframe untuk visualisasi
pred_vs_actual <- data.frame(
  Predicted = best_pred,
  Actual = actual_values
)

# Visualisasi prediksi vs aktual
ggplot(pred_vs_actual, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = paste("Harga Aktual vs. Prediksi dengan", best_model),
       x = "Harga Aktual ($)",
       y = "Harga Prediksi ($)") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# 7. Analisis residual
residuals <- best_pred - actual_values
residual_data <- data.frame(
  Predicted = best_pred,
  Residual = residuals
)

# Visualisasi residual
ggplot(residual_data, aes(x = Predicted, y = Residual)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = paste("Analisis Residual untuk", best_model),
       x = "Harga Prediksi ($)",
       y = "Residual ($)") +
  theme_minimal()

# Histogram residual
ggplot(residual_data, aes(x = Residual)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = paste("Distribusi Residual untuk", best_model),
       x = "Residual ($)",
       y = "Frekuensi") +
  theme_minimal()

# 8. Kesimpulan
cat("\n--- Kesimpulan ---\n")
cat("1. Model terbaik untuk prediksi harga rumah adalah:", best_model, "\n")
cat("2. Metrik evaluasi model terbaik: RMSE =", results$RMSE[which.max(results$R_squared)],
    ", MAE =", results$MAE[which.max(results$R_squared)],
    ", R-squared =", results$R_squared[which.max(results$R_squared)], "\n")
cat("3. Fitur-fitur penting yang mempengaruhi harga rumah dapat diidentifikasi dari model LASSO dan algoritma tree-based.\n")
cat("4. Transformasi log pada harga rumah membantu model untuk menangani skewness dan meningkatkan performa prediksi.\n")
cat("5. Teknik ensemble dapat meningkatkan performa prediksi dengan menggabungkan kekuatan dari beberapa model dasar.\n")