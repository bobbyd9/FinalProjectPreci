

library(shiny)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)

#Reading in datasets

advanced <- readRDS("advallplayers.rds")

#Distighising that this is Gordan Hayward of this year

advanced$player <- as.character(advanced$player)
advanced$player[advanced$player == "Gordan Hayward"] <- "Gordan Hayward 2018"

advanced <- advanced[c(19,1:18),]

Per_36min <- readRDS("allplayers.rds")

Per_36min$player <- as.character(Per_36min$player)

Per_36min$player[Per_36min$player == "Gordan Hayward"] <- "Gordan Hayward 2018"

Per_36min <- Per_36min[c(19,1:18),]

#Creating additional data tables to explore certain relationships

justgh <-  Per_36min%>% 
  filter(grepl('Gordan', player))

bostadv <- advanced %>% 
  filter(grepl('BOS', tm))


#Start of the shiny app

ui <- fluidPage( theme = shinytheme("cosmo"),
  titlePanel("Gordan Hayward 2017-2018 Counterfactual"),
  sidebarPanel(
    
    #Creating the input selection to choose what dataset to use 
    
    selectInput("dataset", "Choose a dataset:", choices = c("Advanced Metrics", "Per 36min", "Boston Celtics", "Predicted vs Current")),
   
    #Now setting up the reactive dropdown menu for variables
    
     uiOutput("xvar2"),uiOutput("yvar2"),uiOutput("idvar2")),
  mainPanel(helpText(h4("What if Gordan Hayward was Healthy?"),
                     "In 2017, the newly acquired Boston Celtic, Gordan Hayward had a season-ending injury in his first game. I decided to take his stats from the previous three years and project what he and the Boston Celtics could have been with him healthy. I do this by taking data and equations from BasketballReference.com and computing what his stats most likely would have been. I then compare his projected stats with his teammates and various other players around the league.")),
  tabsetPanel(type = "tabs",
  tabPanel("Guide", helpText(
    h1("App Guide", align = "center"),
    h3("First, take a look at the tabs, these will help you determine if you want to look at the statistics in a scatterplot or the traditional table format", align = "center"),
    h3("Once you decide that, look at the dropdown menus, here you decided what specific dataset you want to look at and the specific stat you are interested in", align = "center"),
    h5("Statistic Dropdown Menu is only for the Scatterplot", align = "center"),
    h3("The Variables will change dependent on what Dataset you choose", align = "center"))),
  tabPanel("Scatterplot",ggvisOutput("scatter")),
  tabPanel("Data Tables", dataTableOutput("data")),
  tabPanel("Statistics Glossary", helpText(
    h2("Information on what the statistics are can be found at https://www.basketball-reference.com/about/glossary.html", align = "center")
  ))
))

server <- (function(input, output, session) {
  
  dataSource <- reactive({switch(input$dataset,"Advanced Metrics" = advanced,"Per 36min" = Per_36min, "Boston Celtics" = bostadv, "Predicted vs Current" = justgh)})
  
  # Create the selectInput to change with the datasets
  ##THis allows me to vchoose what variables I want someone to be able to pick from 
  
  output$xvar2 <- renderUI({selectInput("xvar", "Choose Statistic:",choices = names(dataSource())[c(-1,-2,-3)], selected = names(dataSource())[20])})
  output$yvar2 <- renderUI({selectInput("yvar", "",choices = names(dataSource())[1], selected = names(dataSource())[1])})
  
  my_subset_data <- reactive({        
    
    # Here check if the column names correspond to the dataset
    if(any(input$xvar %in% names(dataSource())) & any(input$yvar %in% names(dataSource())))
    {
      df <- subset(dataSource(), select = c(input$xvar, input$yvar))
      names(df) <- c("Statistic",".")
      return(df)
    }
  })
  
  #Here I make the scatterplot using ggvis
  
  observe({
    test <- my_subset_data()
    # Test for null as ggvis will evaluate this way earlier when the my_subset_data is NULL
    if(!is.null(test)){
      test %>% ggvis(~Statistic, ~.) %>% layer_points() %>% bind_shiny("scatter")
    }
  })
  
  #I create the data table using the same reactive data to be able to switch between datasets
  
  output$data <- renderDataTable(dataSource())
  
})

shinyApp(ui = ui, server = server)