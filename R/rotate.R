#' rotate a matrix 
#' 
#' @param x the input matrix
#' @return rotated matrix
#' @keywords internal
#' 
rotate <- function (x) t (apply (x, 2, rev))