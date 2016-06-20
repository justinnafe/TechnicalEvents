library(TechnicalEvents)
context("GetEventsForSMACrossOver")

data("MSFT")

test_that("GetEventsForSMACrossOver", {
  expect_equal(nrow(GetEventsForSMACrossOver(Ad(MSFT))), nrow(MSFT))
})
