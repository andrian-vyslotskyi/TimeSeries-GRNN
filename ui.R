library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Time series GRNN"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("windowSize",
                   "Window size:",
                   min = 3,
                   value = 3),
      numericInput("end",
                   "Last x:",
                   min = 0.1,
                   value = 5),
      numericInput("step",
                   "Generation step:",
                   min = 0.01,
                   value = 0.1),
      selectInput("func",
                  "Function",
                  choices = list("sin(2*Pi*x)" = "sin", "cos(2*Pi*x)" = "cos"),
                  selected = "sin"),
      numericInput("firstPredictIndex",
                   "First predict index",
                   value = 41,
                   step = 1)
    ),
    
    mainPanel(
      h3("Test data with neural network results:"),
      tableOutput("table")
    )
  )
))
