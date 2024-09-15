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
load("Int_formula.RData")

unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
my_cores <- as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2
my_cluster <- snow::makeCluster(my_cores, type = 'SOCK')
registerDoSNOW(my_cluster)
snow::clusterExport(cl = my_cluster, "int_formula")

# Genetic algorithm
ga_funcs <- caretGA
ga_funcs$fitness_extern <- defaultSummary

ga_rec <- recipe(Worldwide_log ~ ., data = imdb_train3) %>%
  step_interact(int_formula) %>%
  step_nzv(all_predictors())

# Inner control
ga_ctrl_inner <- trainControl(method = "boot", p = 0.90, number = 1, summaryFunction = defaultSummary, allowParallel = FALSE)

# Outer control for SA
ga_ctrl_outer <- gafsControl(method = "cv", metric = c(internal = "RMSE", external = "RMSE"),
                             maximize = c(internal = FALSE, external = FALSE), functions = ga_funcs, returnResamp = "all",
                             verbose = FALSE, allowParallel = TRUE)

system.time({
  set.seed(8584)
  ga_rmse <- gafs(ga_rec, data = imdb_train3, iters = 50, gafsControl = ga_ctrl_outer, method = "lm",
                  trControl = ga_ctrl_inner, metric = "RMSE")
})

ga_rmse_int <- ga_rmse$internal
ga_rmse_int2 <- ga_rmse_int %>%
  group_by(Iter) %>%
  summarise(RMSE = sum(RMSE) / length(unique(ga_rmse_int$Resample))) %>%
  ungroup() %>%
  mutate(Resample = "Averaged") %>%
  bind_rows(ga_rmse_int, .) %>%
  mutate(colour_grp = if_else(Resample == "Averaged", "yes", "no"))

ga_avg_int <- ga_rmse_int2 %>% filter(Resample == "Averaged") %>% select(Iter, RMSE)

ggplot(ga_rmse_int2, aes(x = Iter, y = RMSE, colour = colour_grp)) +
  geom_point() +
  facet_wrap(~Resample) +
  theme(legend.position = "none")

ga_rmse_ext <- ga_rmse$external
ga_rmse_ext2 <- ga_rmse_ext %>%
  group_by(Iter) %>%
  summarise(RMSE = sum(RMSE) / length(unique(ga_rmse_ext$Resample))) %>%
  ungroup() %>%
  mutate(Resample = "Averaged") %>%
  bind_rows(ga_rmse_ext, .) %>%
  mutate(colour_grp = if_else(Resample == "Averaged", "yes", "no"))

ga_avg_ext <- ga_rmse_ext2 %>% filter(Resample == "Averaged") %>% select(Iter, RMSE)
ga_ext_int_corr <- round(cor(ga_avg_int$RMSE, ga_avg_ext$RMSE), 2)

ggplot(mapping = aes(x = Iter, y = RMSE)) +
  geom_point(data = ga_avg_int, aes(colour = "Internal")) +
  geom_point(data = ga_avg_ext, aes(colour = "External")) +
  geom_label(data = ga_avg_ext, x = 5, y = 0.803, label = str_c("Corr: ", ga_ext_int_corr)) +
  labs(colour = "Estimate") +
  scale_colour_manual(values = c("Internal" = "red", "External" = "green"))

ga_best_vars <- ga_rmse[["optVariables"]]
ga_best_vars

save(ga_best_vars, file = "ga_best_vars.RData")

snow::stopCluster(my_cluster)
unregister()
