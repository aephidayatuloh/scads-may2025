# Machine Learning dengan Dataset Bank Marketing
# Implementasi beberapa model untuk prediksi y

library(tidyverse)
library(tidymodels)
library(finetune)
library(skimr)
library(themis)

# Memuat dataset Bank Marketing
bank_full <- read_delim("data/bank-additional-full.csv", 
                        delim = ";", 
                        locale = locale(decimal_mark = "."))

# 1. Preprocessing Data
# Melihat struktur data
glimpse(bank_full)

# Skimming Data
skim_without_charts(bank_full)

# 2. Mengubah target variabel menjadi factor
bank_full <- bank_full %>% 
  mutate(y = factor(y, levels = c("yes", "no"))) %>% 
  select(-duration)


# 3. Membagi data menjadi training dan testing
set.seed(123)
bank_split <- initial_split(bank_full, prop = 0.8, strata = y)
bank_train <- training(bank_split)
bank_test <- testing(bank_split)

bank_cv <- bank_train %>% 
  vfold_cv(v = 5, strata = y)

# Fungsi untuk evaluasi model
evaluate_model <- metric_set(accuracy, roc_auc, f_meas)

# Data preprocessing dengan recipes
# Membuat recipe untuk preprocessing data
bank_recipe <- bank_train %>% 
  recipe(y ~ .) %>% 
  step_mutate(
    pdays_bin = case_when(
      pdays == 999 ~ "never",
      pdays <= 5 ~ "recently",
      pdays <= 30 ~ "in_last_month",
      TRUE ~ "more_than_month"), 
    skip = FALSE
  ) %>% 
  step_mutate(
    job = case_when(job %in% c("admin.", "blue-collar", "technician") ~ job, 
                    TRUE ~ "other"), 
    skip = FALSE) %>% 
  step_mutate(
    education_level = case_when(education %in% c("illiterate", "basic.4y", "basic.6y") ~ "low", 
                                education %in% c("basic.9y", "high.school") ~ "mid", 
                                education %in% c("professional.course", "university.degree") ~ "high", 
                                TRUE ~ "unknown") %>% 
      factor(levels = c("unknown", "low", "mid", "high")), 
    skip = FALSE
  ) %>% 
  step_mutate(default = if_else(default == "yes", "unknown", default), skip = FALSE) %>% 
  step_mutate(month = factor(month, levels = str_to_lower(month.abb))) %>% 
  step_mutate(day_of_week = factor(day_of_week, levels = c("mon", "tue", "wed", "thu", "fri"))) %>% 
  step_rm(education, pdays) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.7) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_mutate(
    job = factor(job, levels = c("admin.", "blue-collar", "technician", "other")), 
    default = factor(default, levels = c("unknown", "no")), 
    pdays_bin = factor(pdays_bin, levels = c("never", "more_than_month", "in_last_month", "recently"))
  ) %>% 
  step_dummy(all_factor_predictors()) %>% 
  step_smote(y, over_ratio = 0.5, skip = TRUE)

bank_recipe
  
bank_prep <- bank_recipe %>% 
  prep()

bank_train_processed <- bank_prep %>% 
  bake(new_data = bank_train)

bank_train_processed

bank_train_processed %>% 
  count(y) %>% 
  mutate(pct = n/sum(n))

# A. Logistic Regression
glm_spec <- logistic_reg() %>% 
  set_engine("glm")

glm_wf <- workflow(preprocessor = bank_recipe, 
               spec = glm_spec)

glm_model <- glm_wf %>% 
  fit(data = bank_train)

glm_model %>% 
  extract_fit_engine() %>% 
  summary()

glm_model %>% 
  tidy() %>% 
  arrange(p.value) %>% 
  print(n = Inf)

# B. Decision Tree
dtree_spec <- decision_tree() %>% 
  set_engine("rpart", model = TRUE) %>% 
  set_mode("classification")

dtree_wf <- workflow(preprocessor = bank_recipe, 
                   spec = dtree_spec)

dtree_model <- dtree_wf %>% 
  fit(data = bank_train)

library(rpart.plot)
dtree_model %>% 
  extract_fit_engine() %>% 
  rpart.plot()

dtree_pred <- dtree_model %>% 
  augment(new_data = bank_test)

dtree_pred %>% 
  evaluate_model(truth = y, .pred_clas)

# C. Random Forest
rf_spec <- rand_forest() %>% 
  set_engine("ranger", importance = 'impurity') %>% 
  set_mode("classification")

rf_wf <- workflow(preprocessor = bank_recipe, 
                  spec = rf_spec)

rf_model <- rf_wf %>% 
  fit(data = bank_train)

rf_model

library(vip)
vip(rf_model)

rf_pred <- rf_model %>% 
  augment(new_data = bank_test)

rf_pred %>% 
  evaluate_model(truth = y, .pred_clas)


# Hyperparameter Tuning

dtree_spec <- decision_tree(cost_complexity = tune(), 
                            tree_depth = tune(), 
                            min_n = tune()) %>% 
  set_engine("rpart", model = TRUE) %>% 
  set_mode("classification")

rf_spec <- rand_forest(mtry = tune(), 
                       min_n = tune(), 
                       trees = 500) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

models_wf <- workflow_set(
  preproc = list(bank = bank_recipe), 
  models = list(dtree = dtree_spec, 
                rf = rf_spec), 
  cross = TRUE
  )

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_results <- models_wf %>%
  workflow_map(
    "tune_race_anova", 
    seed = 123,
    resamples = bank_cv,
    grid = 10,
    control = race_ctrl, 
    metrics = metric_set(f_meas), 
    verbose = TRUE
  )

race_results


# Performance --------------------------------------------------------

race_results %>% 
  autoplot()

race_results %>% 
  autoplot(
    rank_metric = "roc_auc",  
    metric = "roc_auc",
    select_best = TRUE) +
  geom_text(aes(y = mean - 0.005, label = wflow_id), 
            angle = 90, hjust = 1) +
  lims(y = c(0.65, 0.85)) +
  theme(legend.position = "none")


# Finalizing Model

best_param <- race_results %>% 
  extract_workflow_set_result("bank_dtree") %>% 
  select_best(metric = "roc_auc")

best_param


final_result <- race_results %>% 
  extract_workflow("basic_rf") %>% 
  finalize_workflow(best_param) %>% 
  last_fit(split = bank_split)

collect_metrics(final_result)

final_model <- final_result %>% 
  extract_workflow()


best_model_pred <- final_model %>% 
  augment(new_data = bank_test)

best_model_pred %>% 
  count(y)

best_model_pred %>% 
  count(.pred_class)

best_model_pred %>% 
  conf_mat(truth = y, estimate = .pred_class) 

best_model_pred %>% 
  conf_mat(truth = y, estimate = .pred_class) %>% 
  autoplot()

best_model_pred %>% 
  conf_mat(truth = y, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

best_model_pred %>% 
  conf_mat(truth = y, estimate = .pred_class) %>% 
  tidy() %>% 
  mutate(pct = value/nrow(best_model_pred))

best_model_pred %>% 
  accuracy(truth = y, estimate = .pred_class)

best_model_pred %>% 
  evaluate_model(truth = y, estimate = .pred_class)

best_model_pred %>% 
  roc_curve(truth = y, .pred_yes) %>% 
  autoplot()

best_model_pred %>% 
  roc_auc(truth = y, .pred_yes)

# Compare ------------------------------------------------------------

compare_roc <- bind_rows(
  dtree_pred %>% 
    roc_curve(truth = y, .pred_Yes) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred %>% 
    roc_curve(truth = y, .pred_Yes) %>% 
    mutate(Algorithm = "RF")
)

compare_roc %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = Algorithm)) + 
  geom_line() + 
  coord_fixed()

# Model Interpetation -----------------------------------------------

library(DALEXtra)

explainer <- 
  explain_tidymodels(
    model = final_model, 
    data = bank_train, 
    y = bank_train$y == "yes",
    verbose = TRUE, 
    label = "Bank Marketing Deposit Campaign Prediction"
  )

set.seed(123)
x <- bank_test %>% 
  slice_sample(n = 3, by = y)

new_obs <- x %>% 
  mutate(row_id = row.names(.)) %>% 
  column_to_rownames("row_id")

final_model %>% 
  predict(new_obs)

library(modelStudio)
modelStudio(
  explainer = explainer, 
  new_observation = new_obs, 
  new_observation_y = new_obs$y, 
  max_features = 15, 
  facet_dim = c(2, 3)
)











