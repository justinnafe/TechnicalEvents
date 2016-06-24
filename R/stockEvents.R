

#' Get events for the SMA crossover technical indicator.
#'
#' @param x A single column xts or matrix series with numeric values.
#' @param fastSma Number of periods to average over for the faster moving SMA.
#' @param slowSma Number of periods to average over for the slower moving SMA.
#' @param type buy or sell. Defaults to buy.
#' @return Logical vector where TRUE is the row that the event occured.
#' @examples
#' \dontrun{
#' data(YHOO)
#' GetSMACrossOverEvents(Ad(YHOO), 12, 26)
#' }
#' @export
#' @import TTR
#' @import xts
#' @import zoo

GetSMACrossOverEvents <- function(x, fastSma = 20, slowSma = 50, type = "buy"){
  stopifnot(is.xts(x), is.numeric(fastSma), is.numeric(slowSma),
            slowSma > fastSma, ncol(x) == 1)

  if(nrow(x) > slowSma){
    smaFast <- SMA(x[,1], n=fastSma)
    smaSlow <- SMA(x[,1], n=slowSma)

    prevFast <- lag(smaFast, 1)
    prevSlow <- lag(smaSlow, 1)

    # Fix issue where FALSE & NA = FALSE
    smaSlow <- ifelse(is.na(prevSlow), NA, smaSlow)
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

#' Get events for the EMA crossover technical indicator.
#'
#' @param x A single column xts or matrix series with numeric values.
#' @param fastEma Number of periods to average over for the faster moving EMA.
#' @param slowEma Number of periods to average over for the slower moving EMA.
#' @param type buy or sell. Defaults to buy.
#' @return Logical vector where TRUE is the row that the event occured.
#' @examples
#' \dontrun{
#' data(YHOO)
#' GetEMACrossOverEvents(Ad(YHOO), 12, 26)
#' }
#' @export
#' @import TTR
#' @import xts
#' @import zoo

GetEMACrossOverEvents <- function(x, fastEma = 12, slowEma = 26, type = "buy"){
  stopifnot(is.xts(x), is.numeric(fastEma), is.numeric(slowEma),
            slowEma > fastEma, ncol(x) == 1)

  if(nrow(x) > slowEma){
    emaFast <- EMA(x[,1], n=fastEma)
    emaSlow <- EMA(x[,1], n=slowEma)

    prevFast <- lag(emaFast, 1)
    prevSlow <- lag(emaSlow, 1)

    # Fix issue where FALSE & NA = FALSE
    emaSlow <- ifelse(is.na(prevSlow), NA, emaSlow)
    if(type == "buy"){
      events <- emaFast <= emaSlow & prevFast > prevSlow
      index(events) <- index(x)
      events
    } else{
      if(type == "sell"){
        events <- emaFast >= emaSlow & prevFast < prevSlow
        index(events) <- index(x)
        events
      }
    }

  } else{

  }
}

#' Get events for percentage change
#'
#' @param x A single column xts or matrix series with numeric values.
#' @param percentChange Percent change expressed as a decimal (if the value
#' is greater than 1 or less than -1, then the value is converted to decimal format).
#' @return Logical vector where TRUE is the row that the event occured.
#' @examples
#' \dontrun{
#' data(YHOO)
#' #Get the events that have a 5% drop
#' GetPercentageChangeEvents(Ad(YHOO), -0.05)
#' GetPercentageChangeEvents(Ad(YHOO), -5)
#'
#' # Get the number of times YHOO dropped 3% in price
#' sum(GetPercentageChangeEvents(Ad(YHOO)))
#'
#' # Get the number of times YHOO increased by 3% in price by day
#' sum(GetPercentageChangeEvents(Ad(YHOO), 3))
#' }
#' @export
#' @import xts

GetPercentageChangeEvents <- function(x, percentChange = -0.03){
  stopifnot(is.xts(x), is.numeric(percentChange), percentChange <= 100,
            percentChange >= -100, ncol(x) == 1)
  if(percentChange > 1 || percentChange < -1){
    percentChange <- percentChange / 100
  }
  currentPrice <- x[,1]
  prevPrice <- lag(currentPrice)
  if(percentChange > 0){
    events <- percentChange <= (currentPrice - prevPrice) / currentPrice
    events
  }
  else{
    if(percentChange < 0){
      events <- percentChange >= (currentPrice - prevPrice) / currentPrice
      events
    } else{
      # Percent change is 0
      events <- currentPrice == prevPrice
      events
    }

  }
}
