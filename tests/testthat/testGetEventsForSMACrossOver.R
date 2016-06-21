library(TechnicalEvents)
context("GetEventsForSMACrossOver")

data("MSFT")
test.data.dates <- seq(as.Date("2015/12/1"), as.Date("2016/2/1"), "days")
test.data.values <- sin(seq(1, length(test.data.dates)))
test.xts <- as.xts(test.data.values, order.by = test.data.dates)

test_that("GetEventsForSMACrossOver", {
  expect_equal(dim(GetEventsForSMACrossOver(Ad(MSFT))), c(nrow(MSFT), 1))

  expect_that(GetEventsForSMACrossOver(MSFT), throws_error("1 is not TRUE"))

  # Error when the fast is >= the slow
  expect_that(GetEventsForSMACrossOver(Ad(MSFT), 50, 50), throws_error("slowSma > fastSma is not TRUE"))

  # Return NULL when the slowSma is >= the number of rows.
  expect_named(GetEventsForSMACrossOver(test.xts, 20, nrow(test.xts)), NULL)

  # The number of NAs should match the slowSma
  expect_length(GetEventsForSMACrossOver(test.xts, 10, 20)[is.na(GetEventsForSMACrossOver(test.xts, 10, 20)),], 20)

})
