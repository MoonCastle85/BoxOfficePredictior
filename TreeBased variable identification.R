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

# Tree-based model
system.time({
  rf_mod <- ranger::ranger(Worldwide_log ~ ., data = imdb_train3, num.trees = 300, importance = "permutation",
                           num.threads = my_cores, seed = 8584)
})

rf_imp <- tibble(Predictor = names(rf_mod$variable.importance),
                 Importance = unname(rf_mod$variable.importance))

ggplot(rf_imp, aes(x = reorder(Predictor, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("")
