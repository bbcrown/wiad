#######################################################################
# The auxiliary functions for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

# convert RGB array to HSV array
clRGB2HSV <- function(clArray){
  dat <- floor(255*clArray[,,1:3])
  dm <- dim(dat)
  RGB <- aperm(dat,c(3,2,1))
  dim(RGB) <- c(3, prod(dm[1:2]))
  
  HSV <- rgb2hsv(RGB)
  dim(HSV) <- c(3, dm[2], dm[1])
  HSV <- aperm(HSV, c(3,2,1))
  
  HSV
}

#' The brighness map of an image (0 to 1).
getBrightness <- function(rgbArray){
  bI <- apply(rgbArray, 1:2, max)
  bI
}

#' The contrast map of an image (0 to 1).
getContrast <- function(rgbArray){
  dI <- getDarkness(rgbArray)
  bI <- getBrightness(rgbArray)
  cI <- bI-dI
  cI
}

#' The darkness map of an image (0 to 1).
getDarkness <- function(rgbArray){
  dI <- apply(rgbArray, 1:2, min)
  
  dI
}

# get the system dependent temporary directory
gettmpdir <- function() {
  # tm <- Sys.getenv(c('TMPDIR', 'TMP', 'TEMP'))
  # d <- which(file.info(tm)$isdir & file.access(tm, 2) == 0)
  # if (length(d) > 0)
  #   tm[[d[1]]]
  # else 
  if (.Platform$OS.type == 'windows')
    Sys.getenv('R_USER')
  else
    '/tmp'
}

# rotate a matrix 
rotate <- function (x) t (apply (x, 2, rev))

# rotate an RGB array (or any 3D array)
rotateRGB <- function (imgMat) {
  if (dim (imgMat) [3] != 3) stop ('matrix must have 3 layers in the 3rd dimension!')
  r <- rotate (imgMat[,,1])
  g <- rotate (imgMat[,,2])
  b <- rotate (imgMat[,,3])
  
  rot <- abind (r, g, b, along = 3)
  rot
}


# print a message into konsole given the message string for logging purposes
printLog <- function (msg = NULL, init = F, finit = F){
  if (!PRINT_LOGS) return ()
  systime <- Sys.time()
  
  if (init){
    message (paste ('\n--------------------------------------------------------------------\n', 
                    as.character(systime),'New session just started!',
                    '\n--------------------------------------------------------------------\n'))
    return()
  }
  
  if (finit) {
    message (paste ('\n--------------------------------------------------------------------\n', 
                    as.character(systime),'Initial setup was completed!',
                    '\n--------------------------------------------------------------------\n'))
    return()
  }
  
  message (paste (as.character (systime), 
                  signif (as.numeric (systime)-floor (as.numeric (systime)),3),
                  msg, '\t'))
}

# function to add delete button to the datatable
#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
displayDataTable <- function (df, id1, id2, ...) {
  # function to create one delete button as string
  f <- function (i) {
    as.character (
      actionButton (
        # The id prefix with index
        inputId = paste (id1, i, sep="_"),
        label   = NULL,
        icon    = icon ('trash-alt'),
        onclick = 'Shiny.setInputValue(\"deleteRow\", this.id, {priority: "event"})'))
  }
  # function to create one insert button as string
  g <- function (i) {
    as.character (
      actionButton (
        # The id prefix with index
        inputId = paste (id2, i, sep="_"),
        label   = NULL,
        icon    = icon ('plus-circle'),
        onclick = 'Shiny.setInputValue(\"insertRow\", this.id, {priority: "event"})'))
  }
  
  # create vector of actions buttons
  insertCol <- unlist (lapply (seq_len (nrow (df)), g))
  deleteCol <- unlist (lapply (seq_len (nrow (df)), f))
  
  # return output data table
  DT::datatable (cbind (df, delete = deleteCol, insert = insertCol),
                 # Need to disable escaping for html as string to work
                 escape = FALSE, rownames = FALSE,
                 options = list (
                   # Disable sorting for the delete column
                   columnDefs = list (
                     list (targets = c (9, 10), sortable = FALSE)),
                   initComplete = JS (
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#484e55', 'color': '#fff'});",
                     "$('.dataTables_info').css({'color':'#888'});",
                     "$('.dataTables_filter').css({'color':'#888'});",
                     "$('.dataTables_length').css({'color':'#888'}, );",
                     "}") 
                ))  %>% DT::formatRound (columns = c ('x','y','pixels','growth'), digits = 0) %>% 
                        DT::formatRound (columns = c ('relx','rely'),             digits = 3)
}

#' Extracts the row id number from the id string to delete individual rows
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
parseRowNumber <- function (idstr) {
  res <- as.integer (sub (".*_([0-9]+)", "\\1", idstr))
  if (! is.na (res)) res
}
