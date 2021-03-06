% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stockEvents.R
\name{GetSMACrossOverEvents}
\alias{GetSMACrossOverEvents}
\title{Get events for the SMA crossover technical indicator.}
\usage{
GetSMACrossOverEvents(x, fastSma = 20, slowSma = 50, type = "buy")
}
\arguments{
\item{x}{A single column xts or matrix series with numeric values.}

\item{fastSma}{Number of periods to average over for the faster moving SMA.}

\item{slowSma}{Number of periods to average over for the slower moving SMA.}

\item{type}{buy or sell. Defaults to buy.}
}
\value{
Logical vector where TRUE is the row that the event occured.
}
\description{
Get events for the SMA crossover technical indicator.
}
\examples{
\dontrun{
data(YHOO)
GetSMACrossOverEvents(Ad(YHOO), 12, 26)
}
}

