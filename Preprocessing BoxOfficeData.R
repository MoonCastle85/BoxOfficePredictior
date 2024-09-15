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

# LOAD AND CLEAN MOVIE DATA #
main_url <- "https://www.boxofficemojo.com/year/world/"
years <- seq(1977, 2023, 1)
urls <- map2_vec(.x = main_url, .y = years, .f = paste0)

user_agent <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:120.0) Gecko/20100101 Firefox/120")
my_session <- session(main_url, user_agent)

box_office_table_scrap <- function(link) {
    my_session %>%
        session_jump_to(link) %>%
        read_html() %>%
        html_elements("td") %>%
        html_text() %>%
        enframe() %>%
        bind_cols(., Variable = rep(c("Rank", "Name", "Worldwide", "Domestic", "%", "Foreign", "%2"), times = nrow(.)/7),
                  my_id = rep(c(1:(nrow(.)/7)), times = 1, each = 7)) %>%
        pivot_wider(., names_from = Variable, values_from = value, id_cols = my_id) %>%
        select(Name, Worldwide, Domestic) %>%
        mutate(Year = str_split_i(link, "/", 6))
}
lookup_replace <- function(value, lookup_df) {
    lookup_df$primaryName[match(value, lookup_df$nconst)]
}

box_office_1977_2023 <- map(.x = urls, .f = \(x) bind_rows(box_office_table_scrap(x))) %>%
    enframe() %>%
    unnest(cols = value) %>%
    select(-name) %>%
    rownames_to_column(var = "id")

imdb_basics <- read_delim("https://datasets.imdbws.com/title.basics.tsv.gz")
imdb_crew <- read_delim("https://datasets.imdbws.com/title.crew.tsv.gz")
imdb_names <- read_delim("https://datasets.imdbws.com/name.basics.tsv.gz")

imdb_complete <- left_join(imdb_basics, imdb_crew, by = "tconst")
rm(imdb_basics, imdb_crew)
imdb_complete2 <- imdb_complete %>%
    right_join(., box_office_1977_2023, by = c("primaryTitle" = "Name", "startYear" = "Year"), relationship = "many-to-many") %>%
    mutate(across(.cols = everything(), .fns = ~if_else(.x == "\\N", NA, .x))) %>%
    separate_wider_delim(., cols = directors, delim = ",", names_sep = "", too_few = "align_start") %>%
    separate_wider_delim(., cols = writers, delim = ",", names_sep = "", too_few = "align_start") %>%
    mutate(across(.cols = contains(c("director", "writer")), .fns = ~ lookup_replace(., imdb_names)))

imdb_complete3 <- imdb_complete2 %>%
    filter(titleType == "movie") %>%
    rowwise() %>%
    mutate(dirCount = sum(!is.na(c_across(contains("director")))),
           wriCount = sum(!is.na(c_across(contains("writer"))))) %>%
    ungroup() %>%
    rename(DIR = directors1, WRI = writers1) %>%
    select(-contains(c("directors", "writers"))) %>%
    rename(directors = DIR, writers = WRI) %>%
    select(-c(titleType, originalTitle, isAdult, endYear, id)) %>%
    mutate(isAnimation = if_else(str_detect(genres, "Animation"), 1, 0),
           isDocumentary = if_else(str_detect(genres, "Documentary"), 1, 0),
           isMusical = if_else(str_detect(genres, "Musical"), 1, 0),
           isShort = if_else(str_detect(genres, "Short"), 1, 0),
           isAction = if_else(str_detect(genres, "Action"), 1, 0),
           isComedy = if_else(str_detect(genres, "Comedy"), 1, 0),
           isDrama = if_else(str_detect(genres, "Drama"), 1, 0),
           isSciFi = if_else(str_detect(genres, "Sci-Fi"), 1, 0),
           isFantasy = if_else(str_detect(genres, "Fantasy"), 1, 0),
           isRomance = if_else(str_detect(genres, "Romance"), 1, 0),
           isAdventure = if_else(str_detect(genres, "Adventure"), 1, 0),
           isHorror = if_else(str_detect(genres, "Horror"), 1, 0),
           isThriller = if_else(str_detect(genres, "Thriller"), 1, 0),
           isMystery = if_else(str_detect(genres, "Mystery"), 1, 0),
           isCrime = if_else(str_detect(genres, "Crime"), 1, 0),
           isSport = if_else(str_detect(genres, "Sport"), 1, 0),
           isBiography = if_else(str_detect(genres, "Biography"), 1, 0),
           isHistory = if_else(str_detect(genres, "History"), 1, 0),
           isWar = if_else(str_detect(genres, "War"), 1, 0),
           isWestern = if_else(str_detect(genres, "Western"), 1, 0)) %>%
    select(-genres) %>%
    mutate(directors = if_else(is.na(directors),
                               case_when(
                                   primaryTitle == "Vertical Reality" ~ "Warren Miller",
                                   primaryTitle == "Steeper & Deeper" ~ "Warren Miller",
                                   primaryTitle == "White Fang" ~ "Randal Kleiser",
                                   primaryTitle == "Dreamer" ~ "John Gatins",
                                   primaryTitle == "Warrior of the Hornor" ~ "Unknown",
                                   primaryTitle == "Beauty and the Beast" ~ "Bill Condon",
                                   primaryTitle == "Detective Conan: The Scarlet Alibi" ~ "Unknown",
                                   primaryTitle == "99.9 Criminal Lawyer: The Movie" ~ "Unknown",
                                   primaryTitle == "On the Wandering Paths" ~ "Unknown",
                                   primaryTitle == "The Lion King" ~ "Roger Allers",
                                   primaryTitle == "Big Eyes" ~ "Tim Burton",
                                   primaryTitle == "The Night Before" ~ "Jonathan Levine",
                                   primaryTitle == "Apollo 13" ~ "Ron Howard",
                                   primaryTitle == "Code Blue: The Movie" ~ "Unknown",
                                   primaryTitle == "Takizawa Enbujo Zero" ~ "Unknown",
                                   primaryTitle == "How to Become a Detective" ~ "Unknown",
                                   primaryTitle == "One Hundred Thousand Bad Jokes 2" ~ "Shujie Li",
                                   primaryTitle == paste0("Utapri Movie 2, Uta No Prince-Sama Maji",
                                                          "Love Starish Tours Movie") ~ "Unknown",
                                   primaryTitle == "Samurai Beyond Admiration Record to the World's Best" ~ "Unknown",
                                   primaryTitle == "Endless Winter" ~ "Kurt Miller",
                                   primaryTitle == paste0("I Wish I Could Meet You Again on the Hill",
                                                          "Where That Flower Blooms (2023)") ~ "Unknown",
                                   primaryTitle == "Most Beautiful Week in My Life" ~ "Unknown",
                                   primaryTitle == "Twosabu ilchae" ~ "Unknown",
                                   primaryTitle == "Pandora" ~ "Jeong-woo Park",
                                   primaryTitle == "Toei Spring Anime Fair" ~ "Unknown",
                                   primaryTitle == "Family Honor" ~ "Unknown"),
                               directors),
           writers = if_else(is.na(writers),
                             case_when(
                                 primaryTitle == "White Fang" ~ "Jeanne Rosenberg",
                                 primaryTitle == "How to Become a Detective" ~ "Unknown",
                                 primaryTitle == "One Hundred Thousand Bad Jokes 2" ~ "Shujie Li",
                                 primaryTitle == "Warrior of the Hornor" ~ "Unknown",
                                 primaryTitle == "Beauty and the Beast" ~ "Stephen Chbosky",
                                 primaryTitle == "Detective Conan: The Scarlet Alibi" ~ "Unknown",
                                 primaryTitle == "99.9 Criminal Lawyer: The Movie" ~ "Unknown",
                                 primaryTitle == "On the Wandering Paths" ~ "Unknown",
                                 primaryTitle == "The Lion King" ~ "Irene Mecchi",
                                 primaryTitle == "The Night Before" ~ "Jonathan Levine",
                                 primaryTitle == "Apollo 13" ~ "William Broyles Jr.",
                                 primaryTitle == "Code Blue: The Movie" ~ "Unknown",
                                 primaryTitle == "Takizawa Enbujo Zero" ~ "Unknown",
                                 primaryTitle == "How to Become a Detective" ~ "Unknown",
                                 primaryTitle == paste0("Utapri Movie 2, Uta No Prince-Sama Maji",
                                                        "Love Starish Tours Movie") ~ "Unknown",
                                 primaryTitle == "Samurai Beyond Admiration Record to the World's Best" ~ "Unknown",
                                 primaryTitle == "Endless Winter" ~ "Unknown",
                                 primaryTitle == paste0("I Wish I Could Meet You Again on the Hill",
                                                        "Where That Flower Blooms (2023)") ~ "Unknown",
                                 primaryTitle == "Most Beautiful Week in My Life" ~ "Unknown",
                                 primaryTitle == "Twosabu ilchae" ~ "Unknown",
                                 primaryTitle == "Pandora" ~ "Jeong-woo Park",
                                 primaryTitle == "Toei Spring Anime Fair" ~ "Unknown",
                                 primaryTitle == "Family Honor" ~ "Unknown",
                                 primaryTitle == "Vertical Reality" ~ "Warren Miller"),
                             writers),
           across(.cols = c("directors", "writers"), .fns = ~if_else(is.na(.x), "Unknown", .x)),
           across(.cols = c("dirCount", "wriCount"), .fns = ~if_else(.x == 0, 1, .x)),
           runtimeMinutes = as.integer(runtimeMinutes),
           runtimeMinutes = if_else(is.na(runtimeMinutes), -1, runtimeMinutes)) %>%
    distinct(., tconst, .keep_all = TRUE) %>%
    na.omit(.)

imdb_complete4 <- imdb_complete3 %>%
    select(-tconst) %>%
    mutate(Worldwide = str_replace(Worldwide, "\\$", ""),
           Domestic = str_replace(Domestic, "\\$", ""),
           Worldwide = str_replace_all(Worldwide, ",", ""),
           Domestic = str_replace_all(Domestic, ",", ""),
           Domestic = str_replace_all(Domestic, "-", "-1")) %>%
    mutate(across(.cols = c(startYear, runtimeMinutes, Worldwide, Domestic), .fns = as.numeric))

sum(is.na(imdb_complete4))

imdb_complete5 <- imdb_complete4 %>%
  mutate(Inflation_factor = case_when(
                                      startYear == 1977 ~ 5.02,
                                      startYear == 1978 ~ 4.67,
                                      startYear == 1979 ~ 4.20,
                                      startYear == 1980 ~ 3.70,
                                      startYear == 1981 ~ 3.35,
                                      startYear == 1982 ~ 3.16,
                                      startYear == 1983 ~ 3.06,
                                      startYear == 1984 ~ 2.93,
                                      startYear == 1985 ~ 2.83,
                                      startYear == 1986 ~ 2.78,
                                      startYear == 1987 ~ 2.68,
                                      startYear == 1988 ~ 2.58,
                                      startYear == 1989 ~ 2.46,
                                      startYear == 1990 ~ 2.33,
                                      startYear == 1991 ~ 2.24,
                                      startYear == 1992 ~ 2.17,
                                      startYear == 1993 ~ 2.11,
                                      startYear == 1994 ~ 2.06,
                                      startYear == 1995 ~ 2.00,
                                      startYear == 1996 ~ 1.94,
                                      startYear == 1997 ~ 1.90,
                                      startYear == 1998 ~ 1.87,
                                      startYear == 1999 ~ 1.83,
                                      startYear == 2000 ~ 1.77,
                                      startYear == 2001 ~ 1.72,
                                      startYear == 2002 ~ 1.69,
                                      startYear == 2003 ~ 1.66,
                                      startYear == 2004 ~ 1.61,
                                      startYear == 2005 ~ 1.56,
                                      startYear == 2006 ~ 1.51,
                                      startYear == 2007 ~ 1.47,
                                      startYear == 2008 ~ 1.42,
                                      startYear == 2009 ~ 1.42,
                                      startYear == 2010 ~ 1.40,
                                      startYear == 2011 ~ 1.36,
                                      startYear == 2012 ~ 1.33,
                                      startYear == 2013 ~ 1.31,
                                      startYear == 2014 ~ 1.29,
                                      startYear == 2015 ~ 1.29,
                                      startYear == 2016 ~ 1.27,
                                      startYear == 2017 ~ 1.24,
                                      startYear == 2018 ~ 1.21,
                                      startYear == 2019 ~ 1.19,
                                      startYear == 2020 ~ 1.18,
                                      startYear == 2021 ~ 1.13,
                                      startYear == 2022 ~ 1.04,
                                      startYear == 2023 ~ 1.00),
         Worldwide_adj = Worldwide * Inflation_factor,
         Domestic_adj = Domestic * Inflation_factor,
         Worldwide_log = log1p(Worldwide_adj)
)

save(imdb_complete5, file = "imdb_complete5.RData")
