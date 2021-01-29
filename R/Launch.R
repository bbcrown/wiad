#' Launch WIAD interface
#'
#' This function launches the app by opening the default web browser.
#' 
#' @param archiveDir path to the archive directory
#' @param Interactive logical variable to force an interactive session
#' @return this should be run in an interactive R session
#' @export
#' @import shiny
#' @examples
#'
#' #Launch WIAD interface
#' wiad::Launch()
#'
#'
Launch <- function(archiveDir = './WIAD_ARCHIVE/',
                   Interactive = FALSE){
  
  appDir <- system.file('app', package = "wiad")
  
  ARCHIVE_DIR = archiveDir

    ## Only run examples in interactive R sessions
  if (interactive()|Interactive) {
    
    shinyAppDir(appDir = appDir, 
                options = list(launch.browser = TRUE))
    
  }else{
    
    warning('This function requires an interactive R session!')
    
  }
}