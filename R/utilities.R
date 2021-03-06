library(quantmod)

#' Get the daily returns for each column in a data.frame
#'
#' @param x A data.frame of one or more columns to get returns for.
#' @return A data.frame of daily returns for each column.
#' @export
#' @import quantmod

GetDailyReturnsPerColumn <- function(x){
  x <- as.data.frame(x)
  stopifnot(nrow(x) > 0, ncol(x) > 0)
  result <- x
  for( i in 1:ncol(x)){
    data <- dailyReturn(x[,i])
    result[,i] <- data
  }
  result
}

#' Get a window of values starting at an event and ending w instances
#' after the event.
#'
#' @param x An xts vector.
#' @param event A time based object.
#' @param w The number of periods after and including the event.
#' @return A time series vector of values.
#' @export

GetWindow <- function(x, event, w){
  stopifnot(is.xts(x), is.timeBased(event), is.numeric(w))
  event.onward <- window(x, start=event, end=end(x))
  if(nrow(event.onward) >= w){
    result <- event.onward[1: w,]
    result
  } else{

  }
}

#' Get a window of cumulative returns starting at an event and ending w instances
#' after the event.
#'
#' @param x An xts vector of numeric values.
#' @param event A time based object.
#' @param w The number of periods after and including the event.
#' @return A time series vector of cumulative returns.
#' @export

GetWindowCumulativeReturns <- function(x, event, w){
  stopifnot(is.xts(x), is.timeBased(event), is.numeric(w))
  event.onward <- window(x, start=event, end=end(x))
  if(nrow(event.onward) >= w){
    result <- event.onward[1: w,]
    result <- (result / as.numeric(result[1]) - 1)
    result
  } else{

  }
}

#' Get the return percent from a vector where the first value is the start
#' and the last value is the end.
#'
#' @param x A numeric vector
#' @return A numeric value for the return
#' @export

GetReturnPercent <- function(x){
  if(is.list(x)){
    x <- unlist(x)
  }
  x <- as.vector(x)
  (x[length(x)] - x[1]) / x[1]
}

#' Get the value of a time series vector by date.
#'
#' @param x A time based object.
#' @param ind An xts vector.
#' @return A value for that date
#' @export

GetValueByDate <- function(x, ind){
  ind[as.character(x),]
}
