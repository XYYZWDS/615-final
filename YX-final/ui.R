

library(shiny)
library(shinythemes)
# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  tags$style(HTML('
    body {
      background-color:#f0f0f0;
    }
  ')),
  # Application title
  titlePanel("Cyprus"),
  
  # Sidebar with directory options
  sidebarLayout(
    sidebarPanel(
      selectInput("section", "Select Section:", 
                  choices = c("Map", "Economy", "Population", "Comparison","SWOT Analysis"))
    ),
    
    # Main content area
    mainPanel(
      uiOutput("content")
    )
  )
)
