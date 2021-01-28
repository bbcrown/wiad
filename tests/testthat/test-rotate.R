context("rotating a matrix")

set.seed(2020)

w = 10
h = 5

rgbArray = array(data = runif(w * h * 3), 
                 dim = c(w, h, 3))

test_that("test the rotate function", {
  expect_length(wiad:::rotate(rgbArray), w * h * 3)
})
