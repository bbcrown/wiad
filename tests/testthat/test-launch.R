context("launch test")

set.seed(2020)

test_that("test launch function", {
  
  expect_warning(wiad::Launch(archiveDir = tempdir(), 
                              Interactive = FALSE))
})
