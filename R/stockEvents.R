library(quantmod)

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
#' @import quantmod

GetSMACrossOverEvents <- function(x, fastSma = 20, slowSma = 50, type = "buy"){
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
