context("parising ids")

set.seed(2020)

test_that("test the parserownumber function", {
  expect_equal(wiad:::parseRowNumber('002020'), 2020)
})
