#' The brightness map of an image (0 to 1).
#' 
#' @param rgbArray a 3-column array of R, G abd B color channels
#' @return brightness matrix
#' @keywords internal
#' 
getBrightness <- function(rgbArray){
  bI <- apply(rgbArray, 1:2, max)
  bI
}
