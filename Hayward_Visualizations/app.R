#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggvis)
library(shinythemes)

#Reading in datasets

advanced <- readRDS("advallplayers.rds") %>% 

#Distighising that this is Gordan Hayward of this year

advanced$player <- as.character(advanced$player)
advanced$player[advanced$player == "Gordan Hayward"] <- "Gordan Hayward 2018"

advanced$tm[advanced$tm != "BOS"] <- "NBA"

Per_36min <- readRDS("allplayers.rds")

Per_36min$tm[Per_36min$tm != "BOS"] <- "NBA"

Per_36min$player <- as.character(Per_36min$player)
Per_36min$player[Per_36min$player == "Gordan Hayward"] <- "Gordan Hayward 2018"

justgh <- Per_36min %>% 
  filter(grepl('Gordan', player))


#Start of the shiny app

ui <- fluidPage( theme = shinytheme("cosmo"),
  titlePanel("Gordan Hayward 2017-2018 Counterfactual"),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", choices = c("Advanced", "Per_36min")),
    uiOutput("xvar2"),uiOutput("yvar2"),uiOutput("idvar2")),
  mainPanel(helpText(h4("About the project"),
                     "In 2017, the newly acquired Boston Celtic, Gordan Hayward had a season-ending injury in his first game. We decided to take his stats from the previous three years and project what he and the Boston Celtics could have been with him healthy.")),
  tabsetPanel(
  tabPanel("Scatterplot",ggvisOutput("scatter")),
  tabPanel("Data Tables", dataTableOutput("data"))
))

server <- (function(input, output, session) {
  
  dataSource <- reactive({switch(input$dataset,"Advanced" = advanced,"Per_36min" = Per_36min)})
  
  # Create the selectInput to change with the datasets
  output$xvar2 <- renderUI({selectInput("xvar", "Choose x",choices = names(dataSource())[c(-1,-2,-3)], selected = names(dataSource())[20])})
  output$yvar2 <- renderUI({selectInput("yvar", "Choose y",choices = names(dataSource())[1], selected = names(dataSource())[1])})
  
  my_subset_data <- reactive({        
    
    # Here check if the column names correspond to the dataset
    if(any(input$xvar %in% names(dataSource())) & any(input$yvar %in% names(dataSource())))
    {
      df <- subset(dataSource(), select = c(input$xvar, input$yvar))
      names(df) <- c("x",".")
      return(df)
    }
  })
  
  observe({
    test <- my_subset_data()
    # Test for null as ggvis will evaluate this way earlier when the my_subset_data is NULL
    if(!is.null(test)){
      test %>% ggvis(~x, ~.) %>% layer_points() %>% bind_shiny("scatter")
    }
  })
  
  output$data <- renderDataTable(dataSource())
})

shinyApp(ui = ui, server = server)