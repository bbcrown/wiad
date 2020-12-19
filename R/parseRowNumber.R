#' Extracts the row id number from the id string to delete individual rows
#' 
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
#' @keywords internal
#' 
parseRowNumber <- function (idstr) {
  res <- as.integer (sub (".*_([0-9]+)", "\\1", idstr))
  if (! is.na (res)) res
}