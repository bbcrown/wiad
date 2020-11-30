#' Downloads and installs the WIAD Example Dataset
#' 
#' The WIAD Example Dataset can be downloaded from http://wiad.science/WIAD_ExampleData.zip .
#' This function downloads and extracts the WIAD Example Dataset in the package folder.
#' 
#' @param mode \code{"install"} (default),\code{"update"} or \code{"remove"} 
#' @param downloadPath custom location to temporarily store WIAD example dataset while downloading. Default: tempdir() 
#' @param installPath custom location to place WIAD example dataset. Default: WIAD package directory
#' @importFrom utils download.file
#' @export
#' 
getExampleData <-function(mode="install",downloadPath="",installPath=""){
  # URLS and PATHS
  url<-"http://wiad.science/WIAD_ExampleData.zip"
  if (downloadPath=="") downloadPath  <- tempdir()
  localzipfile <- file.path(downloadPath,"WIAD_ExampleData.zip")
  
  if (installPath=="") installPath  <- system.file(package = "wiad") # DEFINE PACKAGE NAME HERE
  exampleDataPath <- file.path(installPath,"ExampleData" )
  
  if (mode %in% c("install","update")){
    
    # Download is started only if /ExampleDataset path does not yet exist or if mode=="update"
    if (!dir.exists(exampleDataPath) || mode=="update") {
      # DOWNLOAD
      message("Downloading WIAD Example Datset (1.83 GB). This may take a while...\n")
      dl <- try(download.file(url,localzipfile, quiet = FALSE, mode = "wb"))
      if(!is.null(attr(dl, "class")) && attr(dl, "class") == "try-error"){
        message("Unable to download data for URL:",url,"\n")
        return()
      }
      
      if (file.exists(localzipfile)){
        # UNZIP
        message ("Extracting WIAD Example Dataset\n")
        utils::unzip(localzipfile, exdir=installPath, overwrite = TRUE)
        # CLEANUP
        message ("Cleanup\n")
        unlink(localzipfile,recursive = FALSE)
        message("sucessfully installed WIAD Example Dataset to ", exampleDataPath,'\n')
      } else {
        message ('Unable to extract WIAD Example Dataset: file not found\n')
      }
    } else {
      message('WIAD Example Dataset is already installed\n')
    }
    
    # DO STUFF HERE TO SET AS ACTIVE DATASET
    
  } else if (mode=="remove"){
    # ATTENTION: Will remove the example dataset folder without further warning
    if (dir.exists(exampleDataPath)) {
      unlink(exampleDataPath ,recursive = TRUE)
      message('Sucessfully removed WIAD Example Dataset\n')
      
    }else{
      message('Cannot remove WIAD Example Dataset: Dataset not found\n')
    }
  }
  
}
