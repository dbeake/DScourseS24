library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)
library(kableExtra)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# Clean up the data
income %<>% select(-native.country, -fnlwgt, education.num)
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)

income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)

# logistic regression

print('Starting LOGIT')
tune_logit_spec <- logistic_reg(
  penalty = tune(), 
  mixture = 1       
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

lambda_grid <- grid_regular(penalty(), levels = 50)
rec_folds <- vfold_cv(income_train, v = 3)

rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf,
                                       best_acc
)
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "logit",
         best_params = paste("lambda =", logit_ans[["penalty"]])) %>% 
  select(-starts_with(".config"))

# tree model

print('Starting TREE')
tune_tree_spec <- decision_tree(
  min_n = tune(), 
  tree_depth = tune(), 
  cost_complexity = tune(), 
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

rec_folds <- vfold_cv(income_train, v = 3)

rec_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df
  )

top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_tree <- finalize_workflow(rec_wf,
                                best_acc
)
print('*********** DECISION TREE **************')
tree_test <- last_fit(final_tree,income_split) %>%
  collect_metrics()

tree_test %>% print(n = 1)
top_acc %>% print(n = 1)

tree_ans <- top_acc %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "tree",
         best_params = paste("min_n =", tree_ans[["min_n"]], ", tree_depth =", tree_ans[["tree_depth"]], ", cost_complexity =", tree_ans[["cost_complexity"]])) %>% 
  select(-starts_with(".config"))


# neural net

print('Starting NNET')
tune_nnet_spec <- mlp(
  hidden_units = tune(),
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

rec_folds <- vfold_cv(income_train, v = 3)

rec_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = nnet_parm_df
  )

top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_nnet <- finalize_workflow(rec_wf,
                                best_acc
)
print('*********** NEURAL NETWORK **************')
nnet_test <- last_fit(final_nnet,income_split) %>%
  collect_metrics()

nnet_test %>% print(n = 1)
top_acc %>% print(n = 1)

nnet_ans <- top_acc %>% slice(1)
nnet_ans %<>% left_join(nnet_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "nnet",
         best_params = paste("hidden_units =", nnet_ans[["hidden_units"]], ", lambda =", nnet_ans[["penalty"]])) %>% 
  select(-starts_with(".config"))


# knn

print('Starting KNN')
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() 
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

knn_parm_df <- tibble(neighbors = seq(1,30))

rec_folds <- vfold_cv(income_train, v = 3)

rec_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = knn_parm_df
  )

top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_knn <- finalize_workflow(rec_wf,
                               best_acc
)
print('*********** k-NEAREST NEIGHBORS **************')
knn_test <- last_fit(final_knn,income_split) %>%
  collect_metrics()

knn_test %>% print(n = 1)
top_acc %>% print(n = 1)

knn_ans <- top_acc %>% slice(1)
knn_ans %<>% left_join(knn_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "knn",
         best_params = paste("neighbors =", knn_ans[["neighbors"]])) %>% 
  select(-starts_with(".config"))


# SVM

print('Starting SVM')
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

svm_grid <- expand.grid(
  cost = 10^seq(-2, 2, length.out = 3),  
  rbf_sigma = 10^seq(-2, 2, length.out = 3)  
)

rec_folds <- vfold_cv(income_train, v = 3)

rec_wf <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res_grid <- tune_grid(
  rec_wf,
  resamples = rec_folds,
  grid = svm_grid,
  metrics = metric_set(accuracy)
)

rec_res_bayes <- tune_bayes(
  rec_wf,
  resamples = rec_folds,
  initial = rec_res_grid,
  iter = 20,  
  control = control_bayes(no_improve = 5, verbose = FALSE),  # Disabled verbose output
  metrics = metric_set(accuracy)
)

top_acc  <- show_best(rec_res_bayes, metric = "accuracy")
best_acc <- select_best(rec_res_bayes, metric = "accuracy")
final_svm <- finalize_workflow(rec_wf,
                               best_acc
)
print('*********** SUPPORT VECTOR MACHINE **************')
svm_test <- last_fit(final_svm, income_split) %>%
  collect_metrics()

svm_test %>% print(n = 1)
top_acc %>% print(n = 1)

svm_ans <- top_acc %>% slice(1)
svm_ans %<>% left_join(svm_test %>% slice(1), by = c(".metric", ".estimator")) %>%
  mutate(alg = "svm",
         best_params = paste("cost =", svm_ans[["cost"]], ", rbf_sigma =", svm_ans[["rbf_sigma"]])) %>% 
  select(-starts_with(".config"))

# combine answers

all_ans <- bind_rows(logit_ans, tree_ans, nnet_ans, knn_ans, svm_ans)

# Include optimal hyperparameter values in the results
all_ans %<>% mutate(
  best_params = case_when(
    alg == "logit" ~ paste("lambda =", penalty),
    alg == "tree" ~ paste("min_n =", min_n, ", tree_depth =", tree_depth, ", cost_complexity =", cost_complexity),
    alg == "nnet" ~ paste("hidden_units =", hidden_units, ", lambda =", penalty),
    alg == "knn" ~ paste("neighbors =", neighbors),
    alg == "svm" ~ paste("cost =", cost, ", rbf_sigma =", rbf_sigma)
  )
)

# Print the summary of results as a LaTeX table
kable(all_ans %>% select(alg, .estimate, best_params),
      col.names = c("alg", "accuracy", "best_params"),
      format = "latex", booktabs = TRUE, caption = "Model Performance Summary", label = "tab:model_summary")
