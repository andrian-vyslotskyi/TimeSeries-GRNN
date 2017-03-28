library(shiny)

calcR <- function(trainMatr, x) {
  apply(trainMatr, 1, function(row) sqrt(sum(( row - x )^2)))
}
calcD <- function(r, sigma = 1) {
  exp(-(r/sigma)^2)
}

calcY <- function(d, y) sum(d*y)/sum(d)

grnn <- function(trainWithResults, x, windowSize, sigma = 0.1) {
  train <- trainWithResults[,1:windowSize]
  y_train <- trainWithResults[,windowSize+1]
  
  r <- calcR(train, x)
  d <- calcD(r, sigma)
  y <- calcY(d, y_train)
  y
}

generateTimeSeries <- function(f = "sin", end = 5 , step = 0.1) {
  x <- seq(0, end, by=step)
  if(f == "sin") y <- sin(2*pi*x)
  if(f == "cos") y <- cos(2*pi*x)
  y
}

shinyServer(function(input, output) {
  
  output$table <- renderTable({
    timeSeries <- generateTimeSeries(f = input$func, end = input$end, step = input$step)
    
    input_data <- as.data.frame(t(timeSeries[0: input$windowSize + 1]) )
    
    for(i in seq(2, length(timeSeries) - input$windowSize, 1) ) {
      input_data[nrow(input_data)+1,] <- timeSeries[i: (i + input$windowSize) ]
    }
    
    train <- input_data[1: input$firstPredictIndex - 1,]
    test <- input_data[input$firstPredictIndex: nrow(input_data),]
    
    result <- train[input$firstPredictIndex - 1,]
    for(i in seq(1, nrow(test), 1) ) {
      new_input <- result[i, 2:ncol(result)]
      x_next <- grnn(train, new_input, input$windowSize)
      result[i + 1,] <- c( as.numeric( new_input ), x_next)
    }
    
    test["Predicted value"] <- result[-1, input$windowSize + 1]
    colnames(test)[input$windowSize + 1] <- "Value"
    
    # print(test)
    test
  })
  
})
