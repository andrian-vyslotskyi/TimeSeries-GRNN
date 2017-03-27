generateTimeSeries <- function(f = "sin", end = 1 , step = 0.1) {
  x <- seq(0, end, by=step)
  if(f == "sin") y <- sin(2*pi*x)
  if(f == "cos") y <- cos(2*pi*x)
  y
}

windowSize <- 3

timeSeries <- generateTimeSeries()
for(i in seq(0, length(timeSeries) - windowSize, 1) ) {
  print(i)
}

#series.size - window == number of rows in new data