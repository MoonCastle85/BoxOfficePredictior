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

# Load some helpful functions and themes
theme_set(ggthemes::theme_clean(base_size = 16))

load("Preprocessed_data.RData")

unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
my_cores <- as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2
my_cluster <- snow::makeCluster(my_cores, type = 'SOCK')
registerDoSNOW(my_cluster)

# Penalized regression
imdb_train3 <- imdb_train3 %>%
  mutate(across(everything(), as.numeric))

imdb_train_folds <- vfold_cv(imdb_train3, v = 10, repeats = 5)

pen_rec <- recipe(Worldwide_log ~ ., data = imdb_train3) %>%
  step_zv(all_predictors())

my_rmse <- metric_set(rmse)
pen_ctrl <- control_grid(verbose = TRUE, save_pred = TRUE, save_workflow = TRUE)
pen_grid <- expand.grid(mixture = c(0.2, 0.6, 1), penalty = seq(1e-05, 1e-01, 5e-05))

glmnet_mod <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

system.time({
  set.seed(8584)
  pen_tune <- glmnet_mod %>%
    tune_grid(pen_rec, resamples = imdb_train_folds, metrics = my_rmse, control = pen_ctrl, grid = pen_grid)
})

pen_best <- fit_best(pen_tune)
pen_coef <- pen_best %>%
  tidy() %>%
  filter(estimate != 0) %>%
  filter(term != "(Intercept)") %>%
  pull(term)

show_best(pen_tune, metric = "rmse", n = 100) %>%
  mutate(mixture = as.factor(round(mixture, 2))) %>%
  ggplot(aes(x = penalty, y = mean, label = mixture, colour = mixture)) +
  geom_line() +
  geom_point() +
  labs(title = "Tune results without interactions", x = "Lambda penalty", y = "Resample rmse", colour = "Alpha")

select_best(pen_tune, metric = "rmse")

# Penalization with interactions
int_formula <- map2_chr(.x = variable1, .y = variable2, 
                        .f = \(v1, v2) paste0("starts_with('", v1, "'):starts_with('", v2, "')")) %>%
  str_flatten(., collapse = "+") %>%
  paste("~", .) %>%
  as.formula(.)

clusterExport(cl = my_cluster, "int_formula")

pen_rec_int <- recipe(Worldwide_log ~ ., data = imdb_train3) %>%
  step_interact(int_formula) %>%
  step_nzv(all_predictors())

system.time({
  set.seed(8584)
  pen_int_tune <- glmnet_mod %>%
    tune_grid(pen_rec_int, resamples = imdb_train_folds, metrics = my_rmse, control = pen_ctrl, grid = pen_grid)
})

show_best(pen_int_tune, metric = "rmse", n = 500) %>%
  mutate(mixture = as.factor(round(mixture, 2))) %>%
  ggplot(aes(x = penalty, y = mean, label = mixture, colour = mixture)) +
  geom_line() +
  geom_point() +
  labs(title = "Tune results with interactions", x = "Lambda penalty", y = "Resample rmse", colour = "Alpha")

select_best(pen_int_tune, metric = "rmse")

pen_int_best <- fit_best(pen_int_tune)
pen_int_coef <- pen_int_best %>%
  tidy()

pen_int_coef %>%
  select(-penalty) %>%
  filter(estimate != 0 & term != "(Intercept)") %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:20) %>%
  ggplot(., aes(x = abs(estimate), y = reorder(term, abs(estimate)))) +
  geom_col() +
  geom_vline(aes(xintercept = 0.50), colour = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0.25), colour = "blue", linetype = "dashed") +
  labs(x = "Regression coefficient (abs)", y = "Variable") +
  theme(legend.position = "none")

snow::stopCluster(my_cluster)
unregister()

save(int_formula, file = "Int_formula.RData")
