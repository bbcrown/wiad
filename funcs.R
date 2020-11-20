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
rotate <- function(x) t(apply(x, 2, rev))

# rotate an RGB array (or any 3D array)
rotateRGB <- function(imgMat){
  if(dim(imgMat)[3]!=3) stop('matrix must have 3 layers in the 3rd dimension!')
  r <- rotate(imgMat[,,1])
  g <- rotate(imgMat[,,2])
  b <- rotate(imgMat[,,3])
  
  rot <- abind(r, g, b, along = 3)
  rot
}


# print a message into konsole given the message string for logging purposes
printLog <- function(msg=NULL, init=F, finit=F){
  if(!PRINT_LOGS) return()
  systime <- Sys.time()
  
  if(init){
    message(paste('\n--------------------------------------------------------------------\n', 
                  as.character(systime),'New session just started!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }
  
  if(finit){
    message(paste('\n--------------------------------------------------------------------\n', 
                  as.character(systime),'Initial setup was completed!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }
  
  message(paste(as.character(systime), 
                signif(as.numeric(systime)-floor(as.numeric(systime)),3),
                msg, '\t'))
}


#' Downloads and installs the WIAD Example Dataset
#' The WIAD Example Dataset can be downloaded from http://wiad.science/WIAD_ExampleData.zip .
#' This function downloads and extracts the WIAD Example Dataset in the package folder.
#' @param mode \code{"install"} (default),\code{"update"} or \code{"remove"} 
#' @param downloadPath custom location to temporarily store WIAD example dataset while downloading. Default: tempdir() 
#' @param installPath custom location to place WIAD example dataset. Default: WIAD package directory
#' @export
#' 
getWiadExampleData <-function(mode="install",downloadPath="",installPath=""){
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




