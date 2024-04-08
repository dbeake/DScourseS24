library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)
library(yardstick)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% pull(medv)
housing_test_y  <- housing_test_prepped  %>% pull(medv)

cat("Dimension of the training data (housing_train):", dim(housing_train), "\n")
cat("Number of additional X variables compared to the original housing data:", ncol(housing_train_x) - ncol(housing), "\n")

# LASSO model
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lasso_grid <- grid_regular(penalty(), levels = 50)
lasso_folds <- vfold_cv(housing_train_prepped, v = 6)

lasso_wf <- workflow() %>%
  add_model(lasso_spec) %>%
  add_formula(medv ~ .)

lasso_res <- lasso_wf %>%
  tune_grid(resamples = lasso_folds, grid = lasso_grid)

best_lasso <- lasso_res %>% select_best(metric = "rmse")
final_lasso <- finalize_workflow(lasso_wf, best_lasso)
lasso_fit <- fit(final_lasso, data = housing_train_prepped)

lasso_train_pred <- predict(lasso_fit, new_data = housing_train_prepped) %>% 
  bind_cols(housing_train_prepped %>% select(medv))
lasso_test_pred <- predict(lasso_fit, new_data = housing_test_prepped) %>% 
  bind_cols(housing_test_prepped %>% select(medv))

lasso_metrics <- metric_set(rmse)
lasso_in_rmse <- lasso_metrics(lasso_train_pred, truth = medv, estimate = .pred)
lasso_out_rmse <- lasso_metrics(lasso_test_pred, truth = medv, estimate = .pred)

cat("LASSO Results:\n")
cat("Optimal lambda:", best_lasso$penalty, "\n")
cat("In-sample RMSE:", lasso_in_rmse$.estimate, "\n")
cat("Out-of-sample RMSE:", lasso_out_rmse$.estimate, "\n\n")

# Ridge regression model
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

ridge_grid <- grid_regular(penalty(), levels = 50)
ridge_folds <- vfold_cv(housing_train_prepped, v = 6)

ridge_wf <- workflow() %>%
  add_model(ridge_spec) %>%
  add_formula(medv ~ .)

ridge_res <- ridge_wf %>%
  tune_grid(resamples = ridge_folds, grid = ridge_grid)

best_ridge <- ridge_res %>% select_best(metric = "rmse")
final_ridge <- finalize_workflow(ridge_wf, best_ridge)
ridge_fit <- fit(final_ridge, data = housing_train_prepped)

ridge_test_pred <- predict(ridge_fit, new_data = housing_test_prepped) %>% 
  bind_cols(housing_test_prepped %>% select(medv))

ridge_metrics <- metric_set(rmse)
ridge_out_rmse <- ridge_metrics(ridge_test_pred, truth = medv, estimate = .pred)

cat("Ridge Regression Results:\n")
cat("Optimal lambda:", best_ridge$penalty, "\n")
cat("Out-of-sample RMSE:", ridge_out_rmse$.estimate, "\n")