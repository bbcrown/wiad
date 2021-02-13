#' function to add delete button to the datatable
#' 
#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
#' @keywords internal
#' @import data.table
#' @importFrom DT datatable formatRound JS
#' @importFrom magrittr %>%
#' @import shiny
#' 
displayDataTable <- function (df, id1, id2, ...) {
  
  # re-order df to start with the oldest value
  df <- df [order (no, 
                   decreasing = TRUE)]
  
  # function to create one delete button as string
  f <- function (i) {
    as.character (
      actionButton (
        # The id prefix with index
        inputId = paste (id1, i, sep="_"),
        label   = NULL,
        icon    = icon ('trash-alt'),
        onclick = 'Shiny.setInputValue(\"delete_row\", this.id, {priority: "event"})'))
  }
  
  # function to create one insert button as string
  g <- function (i) {
    as.character (
      actionButton (
        # The id prefix with index
        inputId = paste (id2, i, sep="_"),
        label   = NULL,
        icon    = icon ('plus-circle'),
        onclick = 'Shiny.setInputValue(\"insert_row\", this.id, {priority: "event"})'))
  }
  
  # create vector of actions buttons
  insertCol <- unlist (
    lapply (
      seq_len (
        nrow (
          df)), 
      g))
  
  deleteCol <- unlist (
    lapply (
      seq_len (
        nrow (
          df)), 
      f))
  
  # return output data table
  DT::datatable (cbind (df, 
                        delete = deleteCol, 
                        insert = insertCol),
                 
                 # Need to disable escaping for html as string to work
                 escape = FALSE, 
                 rownames = FALSE,
                 options = list (
                   
                   # Disable sorting for the delete column
                   columnDefs = list (
                     list (targets = c (9, 10), 
                           sortable = FALSE)),
                   
                   initComplete = DT::JS (
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#484e55', 'color': '#fff'});",
                     "$('.dataTables_info').css({'color':'#888'});",
                     "$('.dataTables_filter').css({'color':'#888'});",
                     "$('.dataTables_length').css({'color':'#888'}, );",
                     "}") 
                   
                 ))  %>% 
    DT::formatRound (columns = c ('x','y','pixels','growth'), 
                     digits = 0) %>% 
    
    DT::formatRound (columns = c ('relx','rely'), 
                     digits = 3)
}
