context("download example data")

set.seed(2020)

test_that("test the exampledata function, test", {
  expect_null(wiad:::getExampleData(mode = 'test'))
})


test_that("test the exampledata function, remove", {
  expect_message(wiad:::getExampleData(mode = 'remove'))
})

test_that("test the exampledata function, remove", {
  expect_message(wiad:::getExampleData(mode = 'remove'))
})

