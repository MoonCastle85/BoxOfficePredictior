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

unregister <- function() { # Helper function to unregister parallel processes that didn't shut down for some reason
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}

#### LOAD AND CLEAN MOVIE DATA #####
# main_url <- "https://www.boxofficemojo.com/year/world/"
# years <- seq(1977, 2023, 1)
# urls <- map2_vec(.x = main_url, .y = years, .f = paste0)
# 
# user_agent <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:120.0) Gecko/20100101 Firefox/120")
# my_session <- session(main_url, user_agent)
# 
# box_office_table_scrap <- function(link) {
#     my_session %>%
#         session_jump_to(link) %>%
#         read_html() %>%
#         html_elements("td") %>%
#         html_text() %>%
#         enframe() %>%
#         bind_cols(., Variable = rep(c("Rank", "Name", "Worldwide", "Domestic", "%", "Foreign", "%2"), times = nrow(.)/7),
#                   my_id = rep(c(1:(nrow(.)/7)), times = 1, each = 7)) %>%
#         pivot_wider(., names_from = Variable, values_from = value, id_cols = my_id) %>%
#         select(Name, Worldwide, Domestic) %>%
#         mutate(Year = str_split_i(link, "/", 6))
# }
# lookup_replace <- function(value, lookup_df) {
#     lookup_df$primaryName[match(value, lookup_df$nconst)]
# }
# 
# box_office_1977_2023 <- map(.x = urls, .f = \(x) bind_rows(box_office_table_scrap(x))) %>%
#     enframe() %>% 
#     unnest(cols = value) %>% 
#     select(-name) %>%
#     rownames_to_column(var = "id")
# 
# imdb_basics <- read_delim("https://datasets.imdbws.com/title.basics.tsv.gz")
# imdb_crew <- read_delim("https://datasets.imdbws.com/title.crew.tsv.gz")
# imdb_names <- read_delim("https://datasets.imdbws.com/name.basics.tsv.gz")
# 
# imdb_complete <- left_join(imdb_basics, imdb_crew, by = "tconst")
# rm(imdb_basics, imdb_crew)
# imdb_complete2 <- imdb_complete %>%
#     right_join(., box_office_1977_2023, by = c("primaryTitle" = "Name", "startYear" = "Year"), relationship = "many-to-many") %>%
#     mutate(across(.cols = everything(), .fns = ~if_else(.x == "\\N", NA, .x))) %>%
#     separate_wider_delim(., cols = directors, delim = ",", names_sep = "", too_few = "align_start") %>%
#     separate_wider_delim(., cols = writers, delim = ",", names_sep = "", too_few = "align_start") %>%
#     mutate(across(.cols = contains(c("director", "writer")), .fns = ~ lookup_replace(., imdb_names)))
# 
# imdb_complete3 <- imdb_complete2 %>%
#     filter(titleType == "movie") %>%
#     rowwise() %>%
#     mutate(dirCount = sum(!is.na(c_across(contains("director")))),
#            wriCount = sum(!is.na(c_across(contains("writer"))))) %>%
#     ungroup() %>%
#     rename(DIR = directors1, WRI = writers1) %>%
#     select(-contains(c("directors", "writers"))) %>%
#     rename(directors = DIR, writers = WRI) %>%
#     select(-c(titleType, originalTitle, isAdult, endYear, id)) %>%
#     mutate(isAnimation = if_else(str_detect(genres, "Animation"), 1, 0),
#            isDocumentary = if_else(str_detect(genres, "Documentary"), 1, 0),
#            isMusical = if_else(str_detect(genres, "Musical"), 1, 0),
#            isShort = if_else(str_detect(genres, "Short"), 1, 0),
#            isAction = if_else(str_detect(genres, "Action"), 1, 0),
#            isComedy = if_else(str_detect(genres, "Comedy"), 1, 0),
#            isDrama = if_else(str_detect(genres, "Drama"), 1, 0),
#            isSciFi = if_else(str_detect(genres, "Sci-Fi"), 1, 0),
#            isFantasy = if_else(str_detect(genres, "Fantasy"), 1, 0),
#            isRomance = if_else(str_detect(genres, "Romance"), 1, 0),
#            isAdventure = if_else(str_detect(genres, "Adventure"), 1, 0),
#            isHorror = if_else(str_detect(genres, "Horror"), 1, 0),
#            isThriller = if_else(str_detect(genres, "Thriller"), 1, 0),
#            isMystery = if_else(str_detect(genres, "Mystery"), 1, 0),
#            isCrime = if_else(str_detect(genres, "Crime"), 1, 0),
#            isSport = if_else(str_detect(genres, "Sport"), 1, 0),
#            isBiography = if_else(str_detect(genres, "Biography"), 1, 0),
#            isHistory = if_else(str_detect(genres, "History"), 1, 0),
#            isWar = if_else(str_detect(genres, "War"), 1, 0),
#            isWestern = if_else(str_detect(genres, "Western"), 1, 0)) %>%
#     select(-genres) %>%
#     mutate(directors = if_else(is.na(directors), 
#                                case_when(
#                                    primaryTitle == "Vertical Reality" ~ "Warren Miller",
#                                    primaryTitle == "Steeper & Deeper" ~ "Warren Miller",
#                                    primaryTitle == "White Fang" ~ "Randal Kleiser",
#                                    primaryTitle == "Dreamer" ~ "John Gatins",
#                                    primaryTitle == "Warrior of the Hornor" ~ "Unknown",
#                                    primaryTitle == "Beauty and the Beast" ~ "Bill Condon",
#                                    primaryTitle == "Detective Conan: The Scarlet Alibi" ~ "Unknown",
#                                    primaryTitle == "99.9 Criminal Lawyer: The Movie" ~ "Unknown",
#                                    primaryTitle == "On the Wandering Paths" ~ "Unknown",
#                                    primaryTitle == "The Lion King" ~ "Roger Allers",
#                                    primaryTitle == "Big Eyes" ~ "Tim Burton",
#                                    primaryTitle == "The Night Before" ~ "Jonathan Levine",
#                                    primaryTitle == "Apollo 13" ~ "Ron Howard",
#                                    primaryTitle == "Code Blue: The Movie" ~ "Unknown",
#                                    primaryTitle == "Takizawa Enbujo Zero" ~ "Unknown",
#                                    primaryTitle == "How to Become a Detective" ~ "Unknown",
#                                    primaryTitle == "One Hundred Thousand Bad Jokes 2" ~ "Shujie Li",
#                                    primaryTitle == paste0("Utapri Movie 2, Uta No Prince-Sama Maji",
#                                                           "Love Starish Tours Movie") ~ "Unknown",
#                                    primaryTitle == "Samurai Beyond Admiration Record to the World's Best" ~ "Unknown",
#                                    primaryTitle == "Endless Winter" ~ "Kurt Miller",
#                                    primaryTitle == paste0("I Wish I Could Meet You Again on the Hill",
#                                                           "Where That Flower Blooms (2023)") ~ "Unknown",
#                                    primaryTitle == "Most Beautiful Week in My Life" ~ "Unknown",
#                                    primaryTitle == "Twosabu ilchae" ~ "Unknown",
#                                    primaryTitle == "Pandora" ~ "Jeong-woo Park",
#                                    primaryTitle == "Toei Spring Anime Fair" ~ "Unknown",
#                                    primaryTitle == "Family Honor" ~ "Unknown"),
#                                directors),
#            writers = if_else(is.na(writers),
#                              case_when(
#                                  primaryTitle == "White Fang" ~ "Jeanne Rosenberg",
#                                  primaryTitle == "How to Become a Detective" ~ "Unknown",
#                                  primaryTitle == "One Hundred Thousand Bad Jokes 2" ~ "Shujie Li",
#                                  primaryTitle == "Warrior of the Hornor" ~ "Unknown",
#                                  primaryTitle == "Beauty and the Beast" ~ "Stephen Chbosky",
#                                  primaryTitle == "Detective Conan: The Scarlet Alibi" ~ "Unknown",
#                                  primaryTitle == "99.9 Criminal Lawyer: The Movie" ~ "Unknown",
#                                  primaryTitle == "On the Wandering Paths" ~ "Unknown",
#                                  primaryTitle == "The Lion King" ~ "Irene Mecchi",
#                                  primaryTitle == "The Night Before" ~ "Jonathan Levine",
#                                  primaryTitle == "Apollo 13" ~ "William Broyles Jr.",
#                                  primaryTitle == "Code Blue: The Movie" ~ "Unknown",
#                                  primaryTitle == "Takizawa Enbujo Zero" ~ "Unknown",
#                                  primaryTitle == "How to Become a Detective" ~ "Unknown",
#                                  primaryTitle == paste0("Utapri Movie 2, Uta No Prince-Sama Maji",
#                                                         "Love Starish Tours Movie") ~ "Unknown",
#                                  primaryTitle == "Samurai Beyond Admiration Record to the World's Best" ~ "Unknown",
#                                  primaryTitle == "Endless Winter" ~ "Unknown",
#                                  primaryTitle == paste0("I Wish I Could Meet You Again on the Hill",
#                                                         "Where That Flower Blooms (2023)") ~ "Unknown",
#                                  primaryTitle == "Most Beautiful Week in My Life" ~ "Unknown",
#                                  primaryTitle == "Twosabu ilchae" ~ "Unknown",
#                                  primaryTitle == "Pandora" ~ "Jeong-woo Park",
#                                  primaryTitle == "Toei Spring Anime Fair" ~ "Unknown",
#                                  primaryTitle == "Family Honor" ~ "Unknown",
#                                  primaryTitle == "Vertical Reality" ~ "Warren Miller"),
#                              writers),
#            across(.cols = c("directors", "writers"), .fns = ~if_else(is.na(.x), "Unknown", .x)),
#            across(.cols = c("dirCount", "wriCount"), .fns = ~if_else(.x == 0, 1, .x)),
#            runtimeMinutes = as.integer(runtimeMinutes),
#            runtimeMinutes = if_else(is.na(runtimeMinutes), -1, runtimeMinutes)) %>%
#     distinct(., tconst, .keep_all = TRUE) %>%
#     na.omit(.)
# 
# # Cat to numeric
# encode_cat_to_numeric <- function(x) {
#     x <- factor(x, ordered = FALSE)
#     x <- unclass(x)
#     return(x)
# }
# 
# imdb_complete4 <- imdb_complete3 %>%
#     select(-tconst) %>%
#     mutate(across(.cols = c(primaryTitle, directors, writers), .fns = encode_cat_to_numeric),
#            Worldwide = str_replace(Worldwide, "\\$", ""),
#            Domestic = str_replace(Domestic, "\\$", ""),
#            Worldwide = str_replace_all(Worldwide, ",", ""),
#            Domestic = str_replace_all(Domestic, ",", ""),
#            Domestic = str_replace_all(Domestic, "-", "-1"),
#            across(.cols = contains(c("startYear", "Count", "is")), .fns = as.factor),
#            across(.cols = c("Worldwide", "Domestic", "primaryTitle", "directors", "writers"), .fns = as.double))
# 
# imdb_complete5 <- imdb_complete4 %>%
#   mutate(Inflation_factor = case_when(
#                                       startYear == 1977 ~ 5.02,
#                                       startYear == 1978 ~ 4.67,
#                                       startYear == 1979 ~ 4.20,
#                                       startYear == 1980 ~ 3.70,
#                                       startYear == 1981 ~ 3.35,
#                                       startYear == 1982 ~ 3.16,
#                                       startYear == 1983 ~ 3.06,
#                                       startYear == 1984 ~ 2.93,
#                                       startYear == 1985 ~ 2.83,
#                                       startYear == 1986 ~ 2.78,
#                                       startYear == 1987 ~ 2.68,
#                                       startYear == 1988 ~ 2.58,
#                                       startYear == 1989 ~ 2.46,
#                                       startYear == 1990 ~ 2.33,
#                                       startYear == 1991 ~ 2.24,
#                                       startYear == 1992 ~ 2.17,
#                                       startYear == 1993 ~ 2.11,
#                                       startYear == 1994 ~ 2.06,
#                                       startYear == 1995 ~ 2.00,
#                                       startYear == 1996 ~ 1.94,
#                                       startYear == 1997 ~ 1.90,
#                                       startYear == 1998 ~ 1.87,
#                                       startYear == 1999 ~ 1.83,
#                                       startYear == 2000 ~ 1.77,
#                                       startYear == 2001 ~ 1.72,
#                                       startYear == 2002 ~ 1.69,
#                                       startYear == 2003 ~ 1.66,
#                                       startYear == 2004 ~ 1.61,
#                                       startYear == 2005 ~ 1.56,
#                                       startYear == 2006 ~ 1.51,
#                                       startYear == 2007 ~ 1.47,
#                                       startYear == 2008 ~ 1.42,
#                                       startYear == 2009 ~ 1.42,
#                                       startYear == 2010 ~ 1.40,
#                                       startYear == 2011 ~ 1.36,
#                                       startYear == 2012 ~ 1.33,
#                                       startYear == 2013 ~ 1.31,
#                                       startYear == 2014 ~ 1.29,
#                                       startYear == 2015 ~ 1.29,
#                                       startYear == 2016 ~ 1.27,
#                                       startYear == 2017 ~ 1.24,
#                                       startYear == 2018 ~ 1.21,
#                                       startYear == 2019 ~ 1.19,
#                                       startYear == 2020 ~ 1.18,
#                                       startYear == 2021 ~ 1.13,
#                                       startYear == 2022 ~ 1.04,
#                                       startYear == 2023 ~ 1.00),
#          Worldwide_adj = Worldwide * Inflation_factor,
#          Domestic_adj = Domestic * Inflation_factor,
#          Worldwide_log = log1p(Worldwide_adj)
# )
# 
# save.image(file = "MovieRawData.RData")
load("MovieRawData.RData")

imdb_split <- initial_split(imdb_complete5, prop = 3/4)
imdb_train <- training(imdb_split) %>%
    select(-c(primaryTitle, Worldwide, Domestic, Domestic_adj, isShort, Inflation_factor, Worldwide_adj, startYear))

my_boxplot <- function(column) {
    g <- ggplot(imdb_train, aes(x = as.factor(!!sym(column)), y = Worldwide_log)) +
         geom_boxplot(notch = TRUE) +
         stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.5, label = length(x)),
                      geom = "text",
                      aes(label = after_stat(label)))
    print(g)
}

my_boxcols <- names(imdb_train)[c(6:(length(imdb_train)-1))]
walk(.x = my_boxcols, ~my_boxplot(.x))

my_interactionplot <- function(x_value, my_group) {
    g <- ggplot(imdb_train, aes(x = !!sym(x_value), y = Worldwide_log, group = !!sym(my_group), colour = !!sym(my_group))) +
            stat_summary(fun = mean, geom = "line") +
            stat_summary(fun = mean, geom = "point")
    print(g)
}

my_intercols <- names(imdb_train)[c(6:(length(imdb_train)-1))] %>%
  combn(x = ., m = 2) %>% 
  t() %>%
  as.data.frame(.)

my_cols1 <- my_intercols$V1
my_cols2 <- my_intercols$V2

## Plot interactions
# walk2(.x = my_cols1[151:171], .y = my_cols2[151:171], ~my_interactionplot(.x, .y))

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

pair_model <- function(df, v1, v2) { # Model without interactions with only two variables
    tmp_vars <- c("Worldwide_log", v1, v2)
    set.seed(8584)
    m <- train(Worldwide_log ~ ., data = df[, tmp_vars], preProc = NULL, method = "lm", metric = "RMSE", trControl = ctrl)
    return(m)
}

pair_int_model <- function(df, v1, v2) { # Model with interactions with only two variables
    tmp_vars <- c("Worldwide_log", v1, v2)
    set.seed(8584)
    m <- train(Worldwide_log ~ (.)^2, data = df[, tmp_vars], preProc = NULL, method = "lm", metric = "RMSE", trControl = ctrl)
    return(m)
}

ctrl <- trainControl(method = "repeatedcv", repeats = 5, summaryFunction = defaultSummary)

my_cluster <- makeCluster(as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2, type = 'SOCK')
registerDoSNOW(my_cluster)

no_int_mods <- map2(.x = variable1, .y = variable2, .f = \(v1, v2) pair_model(imdb_train, v1, v2))
int_mods <- map2(.x = variable1, .y = variable2, .f = \(v1, v2) pair_int_model(imdb_train, v1, v2))
diff_res <- map2(.x = int_mods, .y = no_int_mods, .f = \(m1, m2) compare_models_1way(m1, m2, metric = "RMSE",
                                                                                     alternative = "greater"))
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
norm_rec <- recipe(Worldwide_log ~ ., data = imdb_train) %>%
    step_dummy(all_nominal_predictors())

set.seed(8584)
norm_m <- train(norm_rec, data = imdb_train, method = "lm", metric = "RMSE", trControl = norm_ctrl)
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
int_m <- map(.x = int_rec, .f = \(r) train(r, data = imdb_train, method = "lm", metric = "RMSE", trControl = int_ctrl))
int_m_rmse <- map_dbl(.x = int_m, .f = \(m) getTrainPerf(m)[1, "TrainRMSE"])
diff_all_res <- map2(.x = int_m, .y = list(norm_m), .f = \(m1, m2) compare_models_1way(m1, m2, metric = "RMSE",
                                                                                       alternative = "greater"))

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

# Penalized regression
my_cluster <- makeCluster(as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2, type = 'SOCK')
registerDoSNOW(my_cluster)

imdb_train_folds <- vfold_cv(imdb_train, v = 10, repeats = 5)

pen_rec <- recipe(Worldwide_log ~ ., data = imdb_train) %>%
  step_dummy(all_nominal_predictors()) %>%
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
  ggplot(data = d, aes(x = penalty, y = mean, label = mixture, colour = mixture)) +
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

pen_rec_int <- recipe(Worldwide_log ~ ., data = imdb_train) %>%
  step_dummy(all_nominal_predictors()) %>%
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

# Tree-based model
imdb_baked <- pen_rec_int %>%
  prep() %>%
  bake(., new_data = NULL)

system.time({
  rf_mod <- ranger::ranger(Worldwide_log ~ ., data = imdb_train, num.trees = 300, importance = "permutation",
                   num.threads = as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")) - 2, seed = 8584)
})

rf_imp <- tibble(Predictor = names(rf_mod$variable.importance),
                 Importance = unname(rf_mod$variable.importance))

ggplot(rf_imp, aes(x = reorder(Predictor, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("")

# Simulated annealing
sa_funcs <- caretSA
sa_funcs$initial <- function(vars, prob = 0.30, ...) { # Change the prob to begin from a lower or higher subset
  sort(sample.int(vars, size = floor(vars * prob) + 1))
}

# Inner control
sa_ctrl_inner <- trainControl(method = "boot", p = 0.90, number = 1, summaryFunction = defaultSummary, allowParallel = FALSE)

# Outer control for SA
sa_ctrl_outer <- safsControl(method = "cv", metric = c(internal = "RMSE", external = "RMSE"), 
                             maximize = c(internal = TRUE, external = TRUE), functions = sa_funcs, improve = 20, 
                             returnResamp = "all", verbose = FALSE, allowParallel = TRUE)

system.time({
  set.seed(8485)
  sa_30pct_init <- safs(pen_rec_int, data = imdb_train, iters = 500, safsControl = sa_ctrl_outer, method = "lm",
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

sa_acc_ext <- sa_30pct_init$external
sa_acc_ext2 <- sa_acc_ext %>%
  group_by(Iter) %>%
  summarise(RMSE = sum(RMSE) / length(unique(sa_acc_ext$Resample))) %>%
  ungroup() %>%
  mutate(Resample = "Averaged") %>%
  bind_rows(sa_acc_ext, .) %>%
  mutate(colour_grp = if_else(Resample == "Averaged", "yes", "no"))

sa_avg_ext <- sa_acc_ext2 %>% filter(Resample == "Averaged") %>% select(Iter, RMSE)
ext_int_corr <- round(cor(sa_avg_int$RMSE, sa_avg_ext$RMSE), 2)

ggplot(mapping = aes(x = Iter, y = RMSE)) +
  geom_point(data = sa_avg_int, aes(colour = "Internal")) +
  geom_point(data = sa_avg_ext, aes(colour = "External")) +
  geom_label(data = sa_avg_ext, x = 5, y = 0.795, label = str_c("Corr: ", ext_int_corr)) +
  labs(colour = "Estimate") +
  scale_colour_manual(values = c("Internal" = "red", "External" = "green"))

stopCluster(my_cluster)
unregister()

save.image(file = "AllData.RData")

ui <- fluidPage(

    # Application title
    titlePanel("Information courtesy of IMDb (https://www.imdb.com). Used with permission."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {
    
        

}

# Run the application 
shinyApp(ui = ui, server = server)
