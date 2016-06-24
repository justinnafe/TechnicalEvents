library(TechnicalEvents)
context("GetEMACrossOverEvents")

data("MSFT")
test.data.dates <- seq(as.Date("2015/12/1"), as.Date("2016/2/1"), "days")
test.data.values <- sin(seq(1, length(test.data.dates)))
test.xts <- as.xts(test.data.values, order.by = test.data.dates)

test_that("GetEMACrossOverEvents", {
  expect_equal(dim(GetEMACrossOverEvents(Ad(MSFT))), c(nrow(MSFT), 1))

  expect_that(GetEMACrossOverEvents(MSFT), throws_error("1 is not TRUE"))

  # Error when the fast is >= the slow
  expect_that(GetEMACrossOverEvents(Ad(MSFT), 50, 50), throws_error("slowEma > fastEma is not TRUE"))

  # Return NULL when the slowEma is >= the number of rows.
  expect_named(GetEMACrossOverEvents(test.xts, 20, nrow(test.xts)), NULL)

  # The number of NAs should match the slowEma
  expect_length(GetEMACrossOverEvents(test.xts, 10, 20)[is.na(GetEMACrossOverEvents(test.xts, 10, 20)),], 20)

})
