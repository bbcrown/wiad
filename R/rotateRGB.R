#' rotate an RGB array (or any 3D array)
#' 
#' @param imgMat input RGB array
#' @return rotated each color channel
#' @keywords internal
#' @import abind
#' 
rotateRGB <- function (imgMat) {
  if (dim (imgMat) [3] != 3) stop ('matrix must have 3 layers in the 3rd dimension!')
  r <- rotate (imgMat [,,1])
  g <- rotate (imgMat [,,2])
  b <- rotate (imgMat [,,3])
  
  rot <- abind (r, g, b, along = 3)
  return (rot)
}
