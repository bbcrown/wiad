#' print a message into konsole given the message string for logging purposes
#' 
#' @param msg string message to be printed to stdout
#' @param init boolean variable indicating whether this is the start of the initial setup
#' @param init boolean variable indicating whether this is the end of the initial setup
#' @return No return value
#' @keywords internal
#' 
printLog <- function (msg = NULL, 
                      init = FALSE, 
                      finit = FALSE, 
                      PRINT_LOGS = TRUE){
  
  if (!PRINT_LOGS) 
    return ()
  
  systime <- Sys.time()
  
  if (init){
    message (
      paste ('\n--------------------------------------------------------------------\n', 
             as.character(systime),
             'New session just started!',
             '\n--------------------------------------------------------------------\n'))
  }
  
  if (finit) {
    message (
      paste ('\n--------------------------------------------------------------------\n', 
             as.character(systime),
             'Initial setup was completed!',
             '\n--------------------------------------------------------------------\n'))
  }
  
  message(paste(as.character(systime), 
                signif(
                  as.numeric(
                    systime
                  ) - 
                    floor(
                      as.numeric(
                        systime
                      )
                    ),
                  3),
                msg,
                '\t'))
  
  return()
  
}