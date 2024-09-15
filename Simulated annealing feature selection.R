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
my_cores <- as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2

theme_set(ggthemes::theme_clean(base_size = 16))

load("Preprocessed_data.RData")
load("Int_formula.RData")

unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
my_cluster <- snow::makeCluster(my_cores, type = 'SOCK')
registerDoSNOW(my_cluster)
snow::clusterExport(cl = my_cluster, "int_formula")

# Simulated annealing
sa_funcs <- caretSA
sa_funcs$initial <- function(vars, prob = 0.30, ...) { # Change the prob to begin from a lower or higher subset
  sort(sample.int(vars, size = floor(vars * prob) + 1))
}

SA_rec <- recipe(Worldwide_log ~ ., data = imdb_train3) %>%
  step_interact(int_formula) %>%
  step_nzv(all_predictors())

# Inner control
sa_ctrl_inner <- trainControl(method = "boot", p = 0.90, number = 1, summaryFunction = defaultSummary, allowParallel = FALSE)

# Outer control for SA
sa_ctrl_outer <- safsControl(method = "cv", metric = c(internal = "RMSE", external = "RMSE"), 
                             maximize = c(internal = FALSE, external = FALSE), functions = sa_funcs, improve = 20, 
                             returnResamp = "all", verbose = FALSE, allowParallel = TRUE)

system.time({
  set.seed(8485)
  sa_30pct_init <- safs(SA_rec, data = imdb_train3, iters = 500, safsControl = sa_ctrl_outer, method = "lm",
                        trControl = sa_ctrl_inner, metric = "RMSE")
})

sa_rmse_int <- sa_30pct_init$internal
sa_rmse_int2 <- sa_rmse_int %>%
  group_by(Iter) %>%
  summarise(RMSE = sum(RMSE) / length(unique(sa_rmse_int$Resample))) %>%
  ungroup() %>%
  mutate(Resample = "Averaged") %>%
  bind_rows(sa_rmse_int, .) %>%
  mutate(colour_grp = if_else(Resample == "Averaged", "yes", "no"))

sa_avg_int <- sa_rmse_int2 %>% filter(Resample == "Averaged") %>% select(Iter, RMSE)

ggplot(sa_rmse_int2, aes(x = Iter, y = RMSE, colour = colour_grp)) +
  geom_point() +
  facet_wrap(~Resample) +
  theme(legend.position = "none")

sa_rmse_ext <- sa_30pct_init$external
sa_rmse_ext2 <- sa_rmse_ext %>%
  group_by(Iter) %>%
  summarise(RMSE = sum(RMSE) / length(unique(sa_rmse_ext$Resample))) %>%
  ungroup() %>%
  mutate(Resample = "Averaged") %>%
  bind_rows(sa_rmse_ext, .) %>%
  mutate(colour_grp = if_else(Resample == "Averaged", "yes", "no"))

sa_avg_ext <- sa_rmse_ext2 %>% filter(Resample == "Averaged") %>% select(Iter, RMSE)
ext_int_corr <- round(cor(sa_avg_int$RMSE, sa_avg_ext$RMSE), 2)

ggplot(mapping = aes(x = Iter, y = RMSE)) +
  geom_point(data = sa_avg_int, aes(colour = "Internal")) +
  geom_point(data = sa_avg_ext, aes(colour = "External")) +
  geom_label(data = sa_avg_ext, x = 5, y = 0.795, label = str_c("Corr: ", ext_int_corr)) +
  labs(colour = "Estimate") +
  scale_colour_manual(values = c("Internal" = "red", "External" = "green"))

sa_final <- sa_30pct_init$sa
sa_final2 <- data.frame(Iter = sa_final[["internal"]]$Iter, RMSE = sa_final[["internal"]]$RMSE,
                        Subset_Size = unlist(lapply(sa_final[["subsets"]], length))) %>%
  pivot_longer(-Iter)

ggplot(sa_final2, aes(x = Iter, y = value)) +
  geom_point() +
  geom_vline(xintercept = 408, linetype = "dashed", colour = "blue", linewidth = 1, alpha = 0.6) +
  facet_wrap(~name, nrow = 2, ncol = 1, scales = "free_y")

sa_best_vars <- sa_30pct_init[["optVariables"]]
sa_best_vars

save(sa_best_vars, file = "sa_best_vars.RData")

snow::stopCluster(my_cluster)
unregister()