library(quantmod)

shiftdown <- function(x, n){
  # Remove the last, add to the first
  c(rep(NA, n), x[-(length(x) )])
}


getIncReturns <- function(x, len){
  result <- rep(0, len)
  for(index in 2:len){
    result[index] <- result[index - 1] + x[index]
  }
  as.data.frame(result)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

mergeData <- function(syms, env){
  initialData <- get(syms[1], env)
  result <- Ad(initialData)
  if(length(syms) > 1){
    for( i in 2:length(syms)){
      if(!is.na(syms[i])){
        data <- get(trim(syms[i]), env)
        result <- merge(result, Ad(data))
      }
    }
  }
  colnames(result) <- syms[!is.na(syms)]
  result
}

#' Get the daily returns for each column in a data.frame
#'
#' @param x a data.frame of one or more columns to get returns for.
#' @return a data.frame of the stock's daily returns.
#' @export
#' @import quantmod

getDailyReturnsPerColumn <- function(x){
  x <- as.data.frame(x)
  stopifnot(nrow(x) > 0, ncol(x) > 0)
  result <- x
    for( i in 1:ncol(x)){
      data <- dailyReturn(x[,i])
      result[,i] <- data
    }
  result
}


getWindow <- function(x, event, w){
  # Get set from the event onward
  event.onward <- window(x, start=event, end=end(x))
  if(nrow(event.onward) >= w){
    result <- event.onward[1: w,]
    result
  }
}

getWindowCummReturns <- function(x, event, w){
  # Get set from the event onward
  event.onward <- window(x, start=event, end=end(x))
  if(nrow(event.onward) >= w){
    result <- event.onward[1: w,]
    result <- (result / as.numeric(result[1]) - 1)
    result
  }
}


calcReturnPercent <- function(x){
  (x[[length(x)]] - x[[1]]) / x[[1]]
}

getIndicatorByDate <- function(x, ind){
  ind[as.character(x),]
}

GetSMACrossOverBuyEvents <- function(x, fastSma = 20, slowSma = 50){
  stopifnot(is.numeric(fastSma), is.numeric(slowSma), slowSma > fastSma, ncol(x) == 1)

  if(nrow(x) > slowSma + 1){
    smaFast <- SMA(x[,1], n=fastSma)
    smaSlow <- SMA(x[,1], n=slowSma)

    prevFast <- lag(smaFast, 1)
    prevSlow <- lag(smaSlow, 1)

    events <- smaFast < smaSlow & prevFast > prevSlow
    index(events) <- index(x)
    events
  } else{
    stop("Not enough data")
  }
}

cleanEnv <- function(syms, env){
  items <- ls(env)
  for(item in items){
    if(item %in% syms){

    }
    else{
      rm(list=item, envir=env)
    }
  }
}
