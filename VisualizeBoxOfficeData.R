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
unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

theme_set(ggthemes::theme_clean(base_size = 16))

# Load preprocessed data
load("imdb_complete5.RData")

# Visualize processed data
imdb_split <- initial_split(imdb_complete5, prop = 3/4)
imdb_train <- training(imdb_split) %>%
  select(-c(Worldwide, Domestic, Domestic_adj, isShort, Inflation_factor, Worldwide_adj)) %>%
  mutate(across(.cols = -c(startYear, runtimeMinutes, Worldwide_log), .fns = as.factor))

my_rec <- recipe(Worldwide_log ~ ., data = imdb_train) %>%
  step_filter(runtimeMinutes > 0) %>%
  step_BoxCox(runtimeMinutes) %>%
  step_other(dirCount, threshold = 500, other = ">1") %>%
  step_other(wriCount, threshold = 300, other = ">4") %>%
  step_lencode_bayes(c(directors, writers), outcome = vars(Worldwide_log), options = list(cores = my_cores, seed = 8584))

imdb_train2 <- my_rec %>%
  prep() %>%
  bake(., new_data = NULL)

my_boxplot <- function(d, column, outcome) {
  g <- ggplot(d, aes(x = !!sym(column), y = !!sym(outcome))) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.5, label = length(x)),
                 geom = "text",
                 aes(label = after_stat(label)))
  print(g)
}

my_boxplot(imdb_train2, "dirCount", "Worldwide_log")
my_boxplot(imdb_train2, "wriCount", "Worldwide_log")
my_boxplot(imdb_train2, "startYear", "Worldwide_log")

my_boxcols <- names(imdb_train2)[c(6:(length(imdb_train2)-1))]
walk(.x = my_boxcols, ~my_boxplot(imdb_train2, .x, "Worldwide_log"))

plot_frq(imdb_train2, directors, type = "histogram")
plot_frq(imdb_train2, writers, type = "histogram")
plot_frq(imdb_train2, primaryTitle, type = "histogram")
plot_frq(imdb_train2, startYear, type = "histogram")

plot_scatter(imdb_train2, x = directors, y = Worldwide_log)
plot_scatter(imdb_train2, x = writers, y = Worldwide_log)
plot_scatter(imdb_train2 %>% filter(str_detect(primaryTitle, "Mission: Impossible")), x = primaryTitle, y = Worldwide_log) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45))

# Visualize interactions
my_interactionplot <- function(d, x_value, y_value, my_group) {
  g <- ggplot(d, aes(x = !!sym(x_value), y = !!sym(y_value), group = !!sym(my_group), colour = !!sym(my_group))) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point")
  print(g)
}

my_intercols <- names(imdb_train2)[c(6:(length(imdb_train2)-1))] %>%
  combn(x = ., m = 2) %>% 
  t() %>%
  as.data.frame(.)

my_cols1 <- my_intercols$V1
my_cols2 <- my_intercols$V2

walk2(.x = my_cols1[1:50], .y = my_cols2[1:50], ~my_interactionplot(d = imdb_train2, .x, "Worldwide_log", my_group = .y))
walk2(.x = my_cols1[51:100], .y = my_cols2[51:100], ~my_interactionplot(d = imdb_train2, .x, "Worldwide_log", my_group = .y))
walk2(.x = my_cols1[101:150], .y = my_cols2[101:150], ~my_interactionplot(d = imdb_train2, .x, "Worldwide_log", my_group = .y))
walk2(.x = my_cols1[151:171], .y = my_cols2[151:171], ~my_interactionplot(d = imdb_train2, .x, "Worldwide_log", my_group = .y))

save.image("Preprocessed_data.RData")
