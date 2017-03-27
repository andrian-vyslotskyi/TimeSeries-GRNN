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

windowSize <- 3

timeSeries <- generateTimeSeries()

input <- as.data.frame(t(timeSeries[0:windowSize+1]))

for(i in seq(2, length(timeSeries) - windowSize, 1) ) {
  input[nrow(input)+1,] <- timeSeries[i:(i+windowSize)]
}
input

firstPredictIndex <- 30
train <- input[1:firstPredictIndex-1,]
test <- input[firstPredictIndex:nrow(input),]

results <- apply(test, 1, function(testRow){
  y <- grnn(train, testRow[1:windowSize], windowSize)
  train[nrow(train)+1,] <- c(testRow, y)
  y
})
test["y_r"] <- results
#r <- grnn(train, test[1,1:windowSize], windowSize)
#series.size - window == number of rows in new data