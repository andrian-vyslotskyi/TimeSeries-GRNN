library(shiny)

calcR <- function(trainMatr, x) {
  apply(trainMatr, 1, function(row) sqrt(sum(( row - x )^2)))
}
calcD <- function(r, sigma = 1) {
  exp(-(r/sigma)^2)
}

calcY <- function(d, y) sum(d*y)/sum(d)

grnn <- function(trainWithResults, x, sigma = 1) {
  train <- trainWithResults[, 1:2]
  y_train <- trainWithResults[,3]
  
  r <- calcR(train, x)
  d <- calcD(r, sigma)
  y <- calcY(d, y_train)
  y
}

generateInputData <- function(n) {
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  y <- (1-x1^2)+2*(1-x2)^2
  x <- data.frame(v1=x1, v2=x2, v3=y)
}

shinyServer(function(input, output) {
   
  output$table <- renderTable({
    set.seed(input$seed)
    
    input_data <- generateInputData(input$n)
    
    smp_size <- floor(input$train_coef * nrow(input_data))
    train_ind <- sample(seq_len(nrow(input_data)), size = smp_size)
    
    train <- input_data[train_ind, ]
    test <- input_data[-train_ind, ]
    
    result <- apply(test, 1, function(x) grnn(train, as.numeric(x[1:2]), input$sigma) )
    
    colnames(test) <- c("x1", "x2", "y")
    test["y_r"] = result
    test
  })
  
})
