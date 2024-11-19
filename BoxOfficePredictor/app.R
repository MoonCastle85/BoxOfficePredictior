options(repos = c(CRAN = "https://cran.rstudio.com"))

packages <- c("shiny", "shinythemes", "shinycssloaders", "shinyWidgets", "DT", "tidyverse", "tidymodels", "embed", 
              "stacks", "scales", "baguette", "earth", "kknn", "rules", "kernlab", "ranger", "xgboost", "nnet", "glmnet",
              "emayili", "config")

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
library(emayili)
library(config)

# Load preprocessed data
load("data/Final ensamble model.RData")
load("data/Input options.RData")
load("data/KNN lookup data.RData")
load("data/Worldwide_adj lambda.RData")

# Set up email service
config <- config::get()

smtp <- server(
  host = "smtp.titan.email",
  port = 465,
  username = config$MY_USERNAME,
  password = config$MY_PASSWORD
)

my_ui <- fluidPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),

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
      autonumericInput(inputId = "budget", currencySymbol = "$",
                       label = "What is the budget (USD) for the film, excluding marketing? Scroll to change.",
                       value = 185000000, minimumValue = 0, maximumValue = 500000000, wheelStep = 5000000, 
                       decimalPlaces = 0, digitGroupSeparator = " ", align = "left"),
      autonumericInput(inputId = "runtime", currencySymbol = " mins", currencySymbolPlacement = "s",
                       label = "What is the (approximate) runtime of the film in minutes? Scroll to change.",
                       value = 152, min = 0, max = 500, wheelStep = 5, decimalPlaces = 0, align = "left"),
      selectizeInput(inputId = "writer", label = "Who is the lead writer?", choices = "Jonathan Nolan"),
      selectizeInput(inputId = "director", label = "Who is the lead director?", choices = "Christopher Nolan"),
      selectizeInput(inputId = "main_actor", label = "Who is the lead actor/actress?", choices = "Christian Bale"),
      selectInput(inputId = "wri_count_movie", label = "How many writers are involved in the film?",
                  choices = c("One", "Two", "Three", "Four", "More than four"), selected = "One"),
      selectInput(inputId = "wri_count", label = "How many films has the writer been involved in as a lead writer?",
                  choices = c("One", "Two", "More than two"), selected = "One"),
      selectInput(inputId = "dir_count_movie", label = "How many directors are involved in the film?",
                  choices = c("One", "More than one"), selected = "One"),
      selectInput(inputId = "dir_count", label = "How many films has the director directed as a lead director?",
                  choices = c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "More than seven"), selected = "One"), 
      selectInput(inputId = "actor_count", label = "How many films has the main actor/actress been involved in as a lead?",
                  choices = c("One", "More than one", "Unknown"), selected = "One"),
      div(actionButton("btnpred", "Predict!", icon = icon("calculator", lib = "font-awesome"), class = "btn-primary"),
          style = "display: flex; justify-content: center;"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Predicted box office result. Based on 8000 films from 1993-2023.",
         style = "text-align: center; font-size: 24px;"),
      h2(withSpinner(textOutput("prediction")), style = "text-align: center; font-size: 60px;"),
      br(),
      hr(),
      br(),
      h3("Below are the three most similar films from the database that match the one you defined",
         style = "text-align: center; font-size: 24px;"),
      withSpinner(DT::DTOutput("knn_table")),
      br(),
      br(),
      hr(),
      div(
        style = "text-align: center;",
        prettyRadioButtons(
          inputId = "feedback",
          label = "What do you think of the app?",
          choices = c("Useful", "Not very useful"),
          inline = TRUE,
          icon = icon("check"),
          animation = "tada",
          status = "info"
        )
      ),
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center;",
        br(),
        div(style = "text-align: center;",
            textInputIcon("email", 
                          label = HTML("Leave your email for further contact<br>(it will NOT be shared to third parties)"), 
                                       icon = icon("envelope"))
        ),
        actionButton("submit", "Send feedback", class = "btn-info")
      )
    )
  )
)

my_server <- function(input, output, session) {
  updateSelectizeInput(session, "writer", selected = "Jonathan Nolan",
                       choices = dplyr::filter(options_list, name == "main_wri_options") %>% pull(value) %>% sort(),
                       server = TRUE)
  updateSelectizeInput(session, "director", selected = "Christopher Nolan",
                       choices = dplyr::filter(options_list, name == "main_dir_options") %>% pull(value) %>% sort(),
                       server = TRUE)
  updateSelectizeInput(session, "main_actor", selected = "Christian Bale",
                       choices = dplyr::filter(options_list, name == "main_actor_options") %>% pull(value) %>% sort(),
                       server = TRUE)
  
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
    
    new_prediction <- eventReactive(input$btnpred, {
      new_pred <- predict(fitted_ens, new_data()) %>%
        transmute(new_pred = (Worldwide_adj_lambda*.pred+1)^(1/Worldwide_adj_lambda)) %>%
        pull(new_pred)
    
    return(new_pred)
  })
  
  # Predict KNN lookup
  lookup_table <- eventReactive(input$btnpred, {
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
  
  output$prediction <- renderText({label_currency(big.mark = " ")(round(new_prediction(), -4))})
  output$knn_table <- DT::renderDT(lookup_table(), escape = FALSE, 
                                   options = list(searching = FALSE, dom = 't',
                                                  columnDefs = list(list(targets = c(1:2, 4:6, 9), className = "dt-left"),
                                                                    list(targets = c(3, 7:8), className = "dt-center"))))
  
  is_valid_email <- function(email) {
    str_detect(email, "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")
  }

  observeEvent(input$submit, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$div(
        style = "display: flex; align-items: center; justify-content: center; flex-direction: column;",
        tags$div(id = "spinner", style = "margin-bottom: 10px;", 
                 tags$img(src = "spinner.gif", width = "50")), # Add a spinner gif or animation
        "Email is being sent. Please wait..."
      ),
      type = "info",
      html = TRUE,
      showCloseButton = FALSE,
      showConfirmButton = FALSE,
      closeOnClickOutside = FALSE
    )
    
    if(input$email == "" | !is_valid_email(input$email)) {
      sendSweetAlert(
        session = session,
        title = "Failed...",
        text = "Please provide a valid email adress.",
        type = "warning"
      )
    } else {
      email <- envelope(
        from = config$MY_USERNAME,
        to = config$MY_USERNAME,
        subject = input$email,
        text = input$feedback
      )
      
      showSpinner("submit")
      smtp(email, verbose = TRUE)
      
      sendSweetAlert(
        session = session,
        title = "Feedback sent!",
        text = "Thank you! I might email you for further suggestions.",
        type = "success"
      )
    }
  })
}

# Run the application 
shinyApp(ui = my_ui, server = my_server)
