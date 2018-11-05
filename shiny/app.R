
library(shiny)
library(tidyverse)
library(tidytext)
library(janitor)
library(readxl)

# reads in saved statistical data
ghseasons <- read_excel("GordanHaywardStats.xls") %>% clean_names() %>% head(n = 9)

# Define UI for application that draws bar graph
ui <- fluidPage(
  
  # Application title
  titlePanel("Gordan Hayward Season Stats"),
  
  # Sidebar that shows his stats with the different teams he is on. Dropdown Menu Currently doesnt actually change the data for some reason.
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Team:", 
                  choices=ghseasons$tm)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # Creating Graphic with his points per season colored by his field goal percentage
    ghseasons %>% 
      ggplot(aes(x = season, y = pts, fill = fg_percent)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Gordan Hayward Season Stats", 
           subtile = "Colored by Field Goal Percentage",
           x = "Season",
           y = "Points",
           fill = "FG%")
  })
}


# Link to Publsihed app: https://bobbyd9.shinyapps.io/GordanHaywardPoints/
# Run the application 
shinyApp(ui = ui, server = server)