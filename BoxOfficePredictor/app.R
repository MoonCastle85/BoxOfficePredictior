options(repos = c(CRAN = "https://cran.rstudio.com"))

packages <- c("shiny", "shinythemes", "shinycssloaders", "shinyWidgets", "DT", "tidyverse", "tidymodels", "embed", 
              "stacks", "scales", "baguette", "earth", "kknn", "rules", "kernlab", "ranger", "xgboost", "nnet", "glmnet")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(tidymodels)
library(embed)
library(stacks)
library(scales)
library(kknn)
library(rules)
library(kernlab)
library(ranger)
library(xgboost)
library(nnet)
library(glmnet)

temp_file2 <- tempfile(fileext = ".RData")
download.file("https://github.com/MoonCastle85/BoxOfficePredictior/raw/refs/heads/main/Input%20options.RData", temp_file2)
load(temp_file2, envir = environment())
unlink(temp_file2)

my_ui <- fluidPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),
  tags$style(HTML("
      .btn-disabled {
        pointer-events: none;
        opacity: 0.5;
      }
    ")),
  
  # Application title
  titlePanel("Box office predictor v1.0"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "main_genre", label = "What is the main genre of the film?",
                  choices = dplyr::filter(options_list, name == "genre_options") %>% pull(value) %>% sort(), 
                  selected = "Action"),
      selectInput(inputId = "main_prod_company", label = "Which is the lead production company of the film?",
                  choices = dplyr::filter(options_list, name == "prod_company_options") %>% pull(value) %>% sort(), 
                  selected = "Warner Bros."),
      autonumericInput(inputId = "budget", 
                       label = "What is the (approximate) budget for the film, excluding marketing? Scroll to change.",
                       value = 185000000, minimumValue = 1000000, maximumValue = 500000000, wheelStep = 5000000, 
                       decimalPlaces = 0, digitGroupSeparator = " ", align = "left"),
      numericInput(inputId = "runtime", label = "What is the (approximate) runtime of the film in minutes?", 
                   value = 152, min = 60, max = 500, step = 5),
      textInput(inputId = "writer", label = "Who is the lead writer?", value = "Jonathan Nolan"),
      textInput(inputId = "director", label = "Who is the lead director?", value = "Christopher Nolan"),
      textInput(inputId = "main_actor", label = "Who is the lead actor/actress?", value = "Christian Bale"),
      selectInput(inputId = "wri_count_movie", label = "How many writers are involved in the film?",
                  choices = c("One", "Two", "Three", "Four", "More than four"), selected = "One"),
      selectInput(inputId = "wri_count", label = "How many films has the writer been involved in as a lead writer?",
                  choices = c("One", "Two", "More than two"), selected = "One"),
      selectInput(inputId = "dir_count_movie", label = "How many directors are involved in the film?",
                  choices = c("One", "More than one"), selected = "One"),
      selectInput(inputId = "dir_count", label = "How many films has the director directed as a lead director?",
                  choices = c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "More than seven"), selected = "One"), 
      selectInput(inputId = "actor_count", label = "How many films has the main actor/actress been involved in as a lead?",
                  choices = c("One", "More than one", "Unknown"), selected = "One")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$div(
        textOutput("pred_label"),
        style = "text-align: center; font-size: 24px; font-family: Roboto, sans-serif; color: blue;"
      ),
      tags$div(
        withSpinner(textOutput("prediction")),
        style = "text-align: center; font-size: 60px; font-family: Roboto, sans-serif; color: blue;"
      ),
      br(),
      hr(),
      br(),
      h3("Below are the three most similar films from the database that match the one you defined",
         style = "text-align: center; font-size: 24px; font-family: Roboto, sans-serif; color: blue;"),
      tags$div(
        withSpinner(DT::DTOutput("knn_table")),
        style = "text-align: center; font-size: 14px; font-family: Roboto, sans-serif; color: blue;"
      )
    )
  )
)

my_server <- function(input, output) {
  # Load preprocessed data
  temp_file <- tempfile(fileext = ".RData")
  download.file("https://github.com/MoonCastle85/BoxOfficePredictior/raw/refs/heads/main/Final%20ensamble%20model.RData", 
                temp_file)
  load(temp_file, envir = environment())
  unlink(temp_file)
  
  temp_file3 <- tempfile(fileext = ".RData")
  download.file("https://github.com/MoonCastle85/BoxOfficePredictior/raw/refs/heads/main/Worldwide_adj%20lambda.RData", 
                temp_file3)
  load(temp_file3, envir = environment())
  unlink(temp_file3)
  
  temp_file4 <- tempfile(fileext = ".RData")
  download.file("https://github.com/MoonCastle85/BoxOfficePredictior/raw/refs/heads/main/KNN%20lookup%20data.RData", 
                temp_file4)
  load(temp_file4, envir = environment())
  unlink(temp_file4)

  # Bind input data and predict
  new_data <- reactive({
    new_data <- tibble(tconst = "tt0468569",
                       primaryTitle = "The Dark Knight",
                       orig_Worldwide_adj = 0,
                       startYear = factor(2008, levels = seq(1993, 2023, 1)),
                       main_genre = factor(input$main_genre, 
                                           levels = options_list %>%
                                             filter(name == "genre_options") %>%
                                             pull(value)),
                       main_prod_company = factor(input$main_prod_company,
                                                  levels = options_list %>%
                                                    filter(name == "prod_company_options") %>%
                                                    pull(value)), 
                       Budget_adj = input$budget,
                       runtimeMinutes = input$runtime,
                       writers = input$writer, 
                       directors = input$director,
                       main_actor = input$main_actor,
                       wri_count_movie = case_when(input$wri_count_movie == "One" ~ factor(1, levels = seq(1,5,1)),
                                                   input$wri_count_movie == "Two" ~ factor(2, levels = seq(1,5,1)),
                                                   input$wri_count_movie == "Three" ~ factor(3, levels = seq(1,5,1)),
                                                   input$wri_count_movie == "Four" ~ factor(4, levels = seq(1,5,1)),
                                                   input$wri_count_movie == "More than four" ~ factor(5,
                                                                                                      levels = seq(1,5,1))),
                       wri_count = case_when(input$wri_count == "One" ~ factor(1, levels = seq(1,3,1)),
                                             input$wri_count == "Two" ~ factor(2, levels = seq(1,3,1)),
                                             input$wri_count == "More than two" ~ factor(3, levels = seq(1,3,1))),
                       dir_count_movie = case_when(input$dir_count_movie == "One" ~ factor(1, levels = seq(1,2,1)),
                                                   input$dir_count_movie == "More than one" ~ factor(2,
                                                                                                     levels = seq(1,2,1))),
                       dir_count = case_when(input$dir_count == "One" ~ factor(1, levels = seq(1,8,1)),
                                             input$dir_count == "Two" ~ factor(2, levels = seq(1,8,1)),
                                             input$dir_count == "Three" ~ factor(3, levels = seq(1,8,1)),
                                             input$dir_count == "Four" ~ factor(4, levels = seq(1,8,1)),
                                             input$dir_count == "Five" ~ factor(5, levels = seq(1,8,1)),
                                             input$dir_count == "Six" ~ factor(6, levels = seq(1,8,1)),
                                             input$dir_count == "Seven" ~ factor(7, levels = seq(1,8,1)),
                                             input$dir_count == "More than seven" ~ factor(8, levels = seq(1,8,1))), 
                       actor_count = case_when(input$actor_count == "One" ~ factor(1, levels = seq(1,3,1)),
                                               input$actor_count == "More than one" ~ factor(2, levels = seq(1,3,1)),
                                               input$actor_count == "Unknown" ~ factor(1740, levels = c(1,2,1740))))
    
    return(new_data)
  })
    
    new_prediction <- reactive({
      new_pred <- predict(fitted_ens, new_data()) %>%
        transmute(new_pred = (Worldwide_adj_lambda*.pred+1)^(1/Worldwide_adj_lambda)) %>%
        pull(new_pred)
    
    return(new_pred)
  })
  
  # Predict KNN lookup
  lookup_table <- reactive({
    train_data <- knn_data %>%
      bake(new_data = NULL) %>%
      select(-contains("orig"), -tconst, -primaryTitle)
      
    test_data <- knn_data %>%
      bake(new_data = new_data()) %>%
      select(-contains("orig"), -tconst, -primaryTitle)
    
    knn_lookup <- kknn(Budget_adj ~ ., train = train_data, test = test_data, k = 3, kernel = "triangular")
    
    my_nn <- knn_lookup$C
    nn_knn <- knn_data %>%
      bake(new_data = NULL) %>%
      filter(row.names(.) %in% my_nn) %>%
      mutate(`IMDB link` = paste0('<a href="https://www.imdb.com/title/', tconst, '" target="_blank">',
                                  paste0("https://www.imdb.com/title/", tconst), '</a>'),
             orig_Budget_adj = label_currency(big.mark = " ")(orig_Budget_adj)) %>%
      select(primaryTitle, main_genre, startYear, orig_writers, orig_directors, orig_main_actor, orig_Budget_adj,
             orig_runtimeMinutes, `IMDB link`) %>%
      rename(Title = primaryTitle, Genre = main_genre, Released = startYear, `Lead writer` = orig_writers, 
             `Lead director` = orig_directors, `Lead actor` = orig_main_actor, `Budget (in 2023 dollars)` = orig_Budget_adj,
             `Runtime (mins)` = orig_runtimeMinutes)
    
    return(nn_knn)
  })
  
  output$pred_label <- renderText({"Predicted global box office revenue:"})
  output$prediction <- renderText({label_currency(big.mark = " ")(round(new_prediction(), -4))})
  output$knn_table <- DT::renderDT(lookup_table(), escape = FALSE, 
                                   options = list(searching = FALSE, dom = 't',
                                                  columnDefs = list(list(targets = c(1:2, 4:6, 9), className = "dt-left"),
                                                                    list(targets = c(3, 7:8), className = "dt-center"))))
}

# Run the application 
shinyApp(ui = my_ui, server = my_server)
