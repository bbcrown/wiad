#' rotate a matrix 
#' 
#' @param x the input matrix
#' @return rotated matrix
#' @keywords internal
#' 
rotate <- function (x) {
  rot_x = t (
    apply (
      x, 
      MARGIN = 2, 
      FUN = rev))
  
  return(rot_x)
}