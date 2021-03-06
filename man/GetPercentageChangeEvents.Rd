% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stockEvents.R
\name{GetPercentageChangeEvents}
\alias{GetPercentageChangeEvents}
\title{Get events for percentage change}
\usage{
GetPercentageChangeEvents(x, percentChange = -0.03)
}
\arguments{
\item{x}{A single column xts or matrix series with numeric values.}

\item{percentChange}{Percent change expressed as a decimal (if the value
is greater than 1 or less than -1, then the value is converted to decimal format).}
}
\value{
Logical vector where TRUE is the row that the event occured.
}
\description{
Get events for percentage change
}
\examples{
\dontrun{
data(YHOO)
#Get the events that have a 5\% drop
GetPercentageChangeEvents(Ad(YHOO), -0.05)
GetPercentageChangeEvents(Ad(YHOO), -5)

# Get the number of times YHOO dropped 3\% in price
sum(GetPercentageChangeEvents(Ad(YHOO)))

# Get the number of times YHOO increased by 3\% in price by day
sum(GetPercentageChangeEvents(Ad(YHOO), 3))
}
}

