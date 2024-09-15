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
library(rlang)

# Load some helpful functions and themes
theme_set(ggthemes::theme_clean(base_size = 16))

load("Preprocessed_data.RData")

unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
my_cores <- as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2
my_cluster <- makeCluster(my_cores, type = 'SOCK')
registerDoSNOW(my_cluster)

# Interactions pairwise
variable1 <- c(rep("isAnimation", 5), rep("isDocumentary", 4), rep("isMusical", 7), rep("isAction", 6),
               rep("isComedy", 6), rep("isDrama", 6), rep("isSciFi", 4), rep("isFantasy", 5), rep("isRomance", 3),
               rep("isAdventure", 6), rep("isHorror", 1), rep("isThriller", 6), rep("isMystery", 2), rep("isCrime", 4),
               rep("isSport", 7), rep("isBiography", 3), rep("isHistory", 2), "isWar", "isWestern")

variable2 <- c("isMystery", "isFantasy", "isComedy", "isAction", "isHorror",
               "isAdventure", "isRomance", "isDrama", "isComedy", "isHistory", "isBiography", "isCrime", "isHorror", 
               "isAdventure", "isRomance", "isComedy", "isHistory", "isThriller", "isHorror", "isSciFi", "isComedy", 
               "isDocumentary", "isWar", "isBiography", "isMystery", "isThriller", "isSciFi", "isRomance", "isWestern", 
               "isHistory", "isBiography", "isMystery", "isThriller", "isHorror", "isMystery", "isThriller", "isFantasy", 
               "isHorror", "isWar", "isSport", "isMystery", "isThriller", "isMusical", "isSport", "isThriller", "isAdventure", 
               "isWestern", "isHistory", "isBiography", "isCrime", "isThriller", "isHorror", "isMystery", "isWestern", "isWar",
               "isHistory", "isBiography", "isCrime", "isMystery", "isWar", "isBiography", "isBiography", "isSport", "isMystery", 
               "isFantasy", "isHistory", "isBiography", "isThriller", "isAdventure", "isSciFi", "isDrama", "isComedy", "isWar", 
               "isHistory", "isAnimation", "isFantasy", "isAnimation", "isSciFi", "isMystery")

compare_models_1way <- function(a, b, metric = a$metric[1], ...) { # A customized compare_models function from caret that allows 
  mods <- list(a, b)                                             # for a custom t.test adjustment in the diff-function
  rs <- resamples(mods)
  diffs <- diff(rs, metric = metric[1], adjustment = "none", ...)
  res <- diffs$statistics[[1]][[1]]
  return(res)
}

pair_model <- function(df, v1, v2, y) { # Model without interactions with only two variables
  formula <- new_formula(sym(y), quote(.))
  pair_data <- df %>% select(c(v1, v2, y))
  set.seed(8584)
  m <- train(formula, data = pair_data, preProc = NULL, method = "lm", metric = "RMSE", trControl = ctrl)
  return(m)
}

pair_int_model <- function(df, v1, v2, y) { # Model with interactions with only two variables
  formula <- new_formula(sym(y), quote(.^2))
  pair_data <- df %>% select(c(v1, v2, y))
  set.seed(8584)
  outcome <- sym(y)
  m <- train(formula, data = pair_data, preProc = NULL, method = "lm", metric = "RMSE", trControl = ctrl)
  return(m)
}

ctrl <- trainControl(method = "repeatedcv", repeats = 5, summaryFunction = defaultSummary)

no_int_mods <- map2(.x = variable1, .y = variable2, .f = \(v1, v2) pair_model(imdb_train2, v1, v2, "Worldwide_log"))
int_mods <- map2(.x = variable1, .y = variable2, .f = \(v1, v2) pair_int_model(imdb_train2, v1, v2, "Worldwide_log"))
diff_res <- map2(.x = int_mods, .y = no_int_mods, .f = \(m1, m2) compare_models_1way(m1, m2, metric = "RMSE", 
                                                                                     alternative = "less"))
no_int_rmse <- no_int_mods %>%
  map(.x = ., .f = \(m) getTrainPerf(m)[1, "TrainRMSE"]) %>%
  list_c(.)

int_rmse <- int_mods %>%
  map(.x = ., .f = \(m) getTrainPerf(m)[1, "TrainRMSE"]) %>%
  list_c(.)

diff_res2 <-
  data.frame(Improvement = map_dbl(.x = diff_res, .f = \(est) est$estimate), 
             Pvalue = map_dbl(.x = diff_res, .f = \(p) p$p.value)) %>%
  bind_cols(., No_Int_rmse = no_int_rmse, Int_rmse = int_rmse, Variable1 = variable1, Variable2 = variable2)

diff_res2 %>% 
  filter(Pvalue <= 0.05) %>%
  pivot_longer(cols = c(No_Int_rmse, Int_rmse)) %>%
  mutate(Pairs = str_c(Variable1, " and ", Variable2)) %>%
  ggplot(., aes(x = reorder(Pairs, X = value), y = value, fill = name)) +
  geom_col(position = position_dodge2()) +
  coord_flip() +
  labs(title = "Significant pairwise interactions", x = "Variable pairs", y = "RMSE") +
  scale_fill_discrete(name = "Models", labels = c("With interactions", "Without interactions")) +
  theme(legend.position = "top")

# Interactions with all the variables
norm_ctrl <- trainControl(method = "repeatedcv", repeats = 5, summaryFunction = defaultSummary)

imdb_train3 <- imdb_train2 %>%
  select(-primaryTitle)

norm_rec <- recipe(Worldwide_log ~ ., data = imdb_train3)

set.seed(8584)
norm_m <- train(norm_rec, data = imdb_train3, method = "lm", metric = "RMSE", trControl = norm_ctrl)
norm_m_rmse <- getTrainPerf(norm_m)[1, "TrainRMSE"]
int_ctrl <- trainControl(method = "repeatedcv", repeats = 5, summaryFunction = defaultSummary)

int_function <- function(rec, f) {
  ir <- step_interact(recipe = rec, terms = !!f)
  return(ir)
}

# Map over pairs of vars to create int formulas
int_form <- map2(.x = variable1, .y = variable2, 
                 .f = \(v1, v2) formula(paste0("~starts_with('", v1, "'):starts_with('", v2, "')")))
int_rec <- map(.x = int_form, .f = \(form) int_function(norm_rec, f = form))

set.seed(8584)
int_m <- map(.x = int_rec, .f = \(r) train(r, data = imdb_train3, method = "lm", metric = "RMSE", trControl = int_ctrl))
int_m_rmse <- map_dbl(.x = int_m, .f = \(m) getTrainPerf(m)[1, "TrainRMSE"])
diff_all_res <- map2(.x = int_m, .y = list(norm_m), .f = \(m1, m2) compare_models_1way(m1, m2, metric = "RMSE",
                                                                                       alternative = "less"))

diff_all_res2 <-
  data.frame(Improvement = map_dbl(.x = diff_all_res, .f = \(est) est$estimate),
             Resampled_Pvalue = map_dbl(.x = diff_all_res, .f = \(p) p$p.value)) %>%
  bind_cols(., No_Int_rmse = norm_m_rmse, Int_rmse = int_m_rmse, Variable1 = variable1, Variable2 = variable2)

diff_all_res2 %>% 
  filter(Resampled_Pvalue <= 0.05) %>%
  pivot_longer(cols = c(No_Int_rmse, Int_rmse)) %>%
  mutate(Pairs = str_c(Variable1, " and ", Variable2)) %>%
  ggplot(., aes(x = reorder(Pairs, X = value), y = value, fill = name)) +
  geom_col(position = position_dodge2()) +
  coord_flip() +
  labs(title = "Significant pairwise interactions", x = "Variable pairs", y = "RMSE") +
  scale_fill_discrete(name = "Models", labels = c("With interactions", "Without interactions")) +
  theme(legend.position = "top")

diff_all_res2_adj <- diff_all_res2 %>%
  mutate(Resampled_pvalue_bh = p.adjust(Resampled_Pvalue, method = "BH"),
         Resampled_pvalue_bon = p.adjust(Resampled_Pvalue, method = "bonferroni"))

snow::stopCluster(my_cluster)
unregister()
