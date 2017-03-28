calcR <- function(trainMatr, x) {
  apply(trainMatr, 1, function(row) sqrt(sum(( row - x )^2)))
}
calcD <- function(r, sigma = 1) {
  exp(-(r/sigma)^2)
}

calcY <- function(d, y) sum(d*y)/sum(d)

grnn <- function(trainWithResults, x, windowSize, sigma = 0.01) {
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

windowSize <- 3

timeSeries <- generateTimeSeries()

input_data <- as.data.frame(t(timeSeries[0:windowSize+1]))

for(i in seq(2, length(timeSeries) - windowSize, 1) ) {
  input_data[nrow(input_data)+1,] <- timeSeries[i:(i+windowSize)]
}

firstPredictIndex <- 28
train <- input_data[1:firstPredictIndex-1,]
test <- input_data[firstPredictIndex:nrow(input_data),]

result <- train[firstPredictIndex - 1,]
for(i in seq(1, nrow(test), 1) ) {
  new_input <- result[i, 2:ncol(result)]
  x_next <- grnn(train, new_input, windowSize)
  result[i + 1,] <- c( as.numeric( new_input ), x_next)
}
result

#series.size - window == number of rows in new data