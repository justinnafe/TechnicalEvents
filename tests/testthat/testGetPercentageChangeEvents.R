library(TechnicalEvents)
context("GetPercentageChangeEvents")

data("YHOO")
test.data.dates <- seq(as.Date("2015/12/1"), as.Date("2016/2/1"), "days")
test.data.values <- sin(seq(1, length(test.data.dates)))
test.xts <- as.xts(test.data.values, order.by = test.data.dates)

test_that("GetPercentageChangeEvents", {
  expect_equal(dim(GetPercentageChangeEvents(Ad(YHOO))), c(nrow(YHOO), 1))

  expect_that(GetPercentageChangeEvents(YHOO), throws_error("1 is not TRUE"))

  # Error when percent change is > 100
  expect_that(GetPercentageChangeEvents(Ad(YHOO), 101), throws_error("<= 100 is not TRUE"))

})
