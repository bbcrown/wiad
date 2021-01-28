context("testwiad")

set.seed(2020)

w = 10
h = 5

rgbArray = array(data = runif(w * h * 3), 
                 dim = c(w, h, 3))

brightness = wiad:::getBrightness(rgbArray)
