context("download example data")

set.seed(2020)

test_that("test the rotate function", {
  expect_null(wiad:::getExampleData(mode = 'test'))
})
