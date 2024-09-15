library(shiny)
library(rvest)
library(tidyverse)
library(tidymodels)
library(httr)
library(dlookr)
library(sjPlot)
library(caret)
library(snow)
library(doSNOW)
library(plotly)
library(embed)

# Load data and helpful functions
theme_set(ggthemes::theme_clean(base_size = 16))

load("Preprocessed_data.RData")
load("Int_formula.RData")
load("ga_best_vars.RData")

unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
my_cores <- as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2
my_cluster <- snow::makeCluster(my_cores, type = 'SOCK')
registerDoSNOW(my_cluster)
snow::clusterExport(cl = my_cluster, "int_formula")

# Create recipe with best variables from feature selection processes
imdb_train3 <- imdb_train2 %>%
  select(-primaryTitle) %>%
  mutate(across(everything(), as.numeric))

tuning_rec <- recipe(Worldwide_log ~ ., data = imdb_train3) %>%
  step_interact(int_formula) %>%
  step_nzv(all_predictors()) %>%
  step_select(all_outcomes(), all_of(ga_best_vars), skip = TRUE)

tuning_data <- tuning_rec %>%
  prep() %>%
  bake(new_data = NULL)

tuning_split <- initial_split(tuning_data, prop = 0.8)
tuning_train <- training(tuning_split)
tuning_test <- testing(tuning_split)
tuning_folds <- bootstraps(tuning_train, times = 25)

tuning_rec2 <- recipe(Worldwide_log ~ ., data = tuning_train)

# Linear model
lm_tune_model <- linear_reg() %>%
  set_engine("lm")

lm_tune_wf <- workflow() %>%
  add_recipe(tuning_rec2) %>%
  add_model(lm_tune_model)

lm_tune_control <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

system.time({
  set.seed(8584)
  lm_tune_fit <- fit_resamples(lm_tune_wf, tuning_folds, control = lm_tune_control)
})

lm_fit <- finalize_workflow(lm_tune_wf, select_best(lm_tune_fit, metric = "rmse")) %>%
  fit(tuning_train)

lm_pred <- lm_fit %>%
  predict(tuning_test) %>%
  bind_cols(., tuning_test) %>%
  mutate(Worldwide = expm1(Worldwide_log),
         Worldwide_est = expm1(.pred))

lm_pred %>%
  ggplot(aes(x = Worldwide, y = Worldwide_est)) +
  geom_point()

lm_pred %>% metrics(Worldwide, Worldwide_est)

# Glmnet model
glmnet_tune_model <- linear_reg(mixture = tune(), penalty = tune()) %>%
  set_engine("glmnet", family = "gaussian", verbose = TRUE)

glmnet_tune_wf <- lm_tune_wf %>%
  update_model(glmnet_tune_model)

my_rmse <- metric_set(rmse)
glmnet_ctrl <- control_grid(save_pred = TRUE, save_workflow = TRUE)
glmnet_grid <- expand.grid(penalty = 0.1, mixture = seq(0, 1, 0.1))

system.time({
  set.seed(8584)
  glmnet_tune_fit <- glmnet_tune_wf %>%
    tune_grid(object = ., resamples = tuning_folds, metrics = my_rmse, control = glmnet_ctrl, grid = glmnet_grid)
})

show_best(glmnet_tune_fit, metric = "rmse", n = 20) %>%
  ggplot(aes(x = mixture, y = mean)) +
  geom_line() +
  geom_point() +
  labs(title = "Final tune results", x = "Mixture", y = "Resample rmse")

glmnet_fit <- finalize_workflow(glmnet_tune_wf, select_best(glmnet_tune_fit, metric = "rmse")) %>%
  fit(tuning_train)

glmnet_pred <- glmnet_fit %>%
  predict(tuning_test) %>%
  bind_cols(., tuning_test) %>%
  mutate(Worldwide = expm1(Worldwide_log),
         Worldwide_est = expm1(.pred))

glmnet_pred %>%
  ggplot(aes(x = Worldwide, y = Worldwide_est)) +
  geom_point()

glmnet_pred %>% metrics(Worldwide, Worldwide_est)
