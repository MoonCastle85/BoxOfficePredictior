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
load("MovieRawData.RData")




save.image(file = "AllData.RData")
load("AllData.RData")



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
