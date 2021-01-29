context("download example data")

set.seed(2020)

test_that("test the exampledata function, test", {
  expect_null(wiad:::getExampleData(mode = 'test', installPath = NULL))
})


test_that("test the exampledata function, install", {
  expect_null(wiad:::getExampleData(mode = 'install', installPath = NULL))
})


test_that("test the exampledata function, install", {
  expect_null(wiad:::getExampleData(mode = 'install', installPath = NULL))
})

test_that("test the exampledata function, update", {
  expect_null(wiad:::getExampleData(mode = 'update', installPath = NULL))
})


test_that("test the exampledata function, remove", {
  expect_null(wiad:::getExampleData(mode = 'remove', installPath = NULL))
})
