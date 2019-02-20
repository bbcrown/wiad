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
