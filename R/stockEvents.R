library(quantmod)

#' Get events for the SMA crossover technical indicator.
#'
#' @param x A single column xts or matrix series with numeric values.
#' @param fastSma Number of periods to average over for the faster moving SMA.
#' @param slowSma Number of periods to average over for the slower moving SMA.
#' @param type buy or sell. Defaults to buy.
#' @return Logical vector where TRUE is the row that the event occured.
#' @export
#' @import quantmod

GetEventsForSMACrossOver <- function(x, fastSma = 20, slowSma = 50, type = "buy"){
  stopifnot(is.numeric(fastSma), is.numeric(slowSma), slowSma > fastSma, ncol(x) == 1)

  if(nrow(x) > slowSma){
    smaFast <- SMA(x[,1], n=fastSma)
    smaSlow <- SMA(x[,1], n=slowSma)

    prevFast <- lag(smaFast, 1)
    prevSlow <- lag(smaSlow, 1)

    if(type == "buy"){
      events <- smaFast <= smaSlow & prevFast > prevSlow
      index(events) <- index(x)
      events
    } else{
      if(type == "sell"){
        events <- smaFast >= smaSlow & prevFast < prevSlow
        index(events) <- index(x)
        events
      }
    }

  } else{

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
