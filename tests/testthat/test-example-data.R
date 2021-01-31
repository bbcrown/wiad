context("download example data")

set.seed(2020)

test_that("test the exampledata function, test", {
  expect_null(wiad:::getExampleData(mode = 'test'))
})


test_that("test the exampledata function, remove", {
  expect_message(wiad:::getExampleData(mode = 'remove'))
})

installPath = tempdir()

exampleDataPath <- 
  file.path(
    installPath,
    "ExampleData")

dir.create(exampleDataPath, showWarnings = FALSE)

downloadPath = tempdir()

localzipfile <- 
  file.path(
    downloadPath,
    "WIAD_ExampleData.zip")

cat('test', file = localzipfile)

test_that("test the exampledata function, install", {
  expect_message(wiad:::getExampleData(mode = 'install',
                                       installPath = installPath))
})

test_that("test the exampledata function, install2", {
  expect_message(wiad:::getExampleData(mode = 'install',
                                       installPath = installPath))
})

test_that("test the exampledata function, remove2", {
  expect_message(wiad:::getExampleData(mode = 'remove', 
                                       installPath = installPath))
})

test_that("test the exampledata function, install3", {
  expect_message(wiad:::getExampleData(mode = 'install',
                                       installPath = installPath))
})
