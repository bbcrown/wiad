context("rotating an RGB array")

set.seed(2020)

w = 10
h = 5

rgbArray = array(data = runif(w * h * 3), 
                 dim = c(w, h, 3))

rgbArray2 = array(data = runif(w * h * 2), 
                 dim = c(w, h, 2))

test_that("test the rotateRGB function", {
  expect_length(wiad:::rotateRGB(rgbArray), w * h * 3)
})

test_that("test the rotateRGB function2", {
  expect_error(wiad:::rotateRGB(rgbArray2))
})

