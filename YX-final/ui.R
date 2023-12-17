


library(shiny)

# Define UI for application
ui <- fluidPage(
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