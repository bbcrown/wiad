#######################################################################
# The auxiliary functions for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

# plot jpeg image using as raster given image path.
plotJPEG <- function(path, add=FALSE, xlim = NULL, ylim = NULL)
{
  jpgNonNative <- NULL
  
  jpgNative <-  readJPEG(path, native=T) # read the file
  
  res <-  dim(jpgNative)[2:1] # get the resolution
  
  if(is.null(xlim)) xlim <- c(1,res[1])
  
  if(is.null(ylim)) ylim <- c(1,res[2])
  
  if (!add) # initialize an empty plot area if add==FALSE
    plot(NA, xlim = xlim, ylim = ylim, type='n',
         xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
  
  rasterImage(jpgNative,1,1,res[1],res[2])
  
  invisible(list(res=res, 
                 jpgNonNative=jpgNonNative, 
                 jpgNative=jpgNative ))
}


# plot jpeg image using as raster given image path.
# plotIMG <- function(path, add=FALSE, xlim = NULL, ylim = NULL)
# {
#   ext <- file_ext(path)
#   
#   if(ext%in%c('jpg', 'jpeg')) 
#     mat <- readJPEG(path)
#   else
#     if(ext%in%c('tiff', 'tif')) 
#       mat <- readTIFF(path)
#       else 
#     if(ext%in%c('png'))
#       mat <- readPNG(path)
#   else 
#     stop('wrong extension!')
#   
#   res <-  dim(mat)[2:1] # get the resolution
#   
#   if(is.null(xlim)) 
#     xlim <- c(1,res[1])
#   
#   if(is.null(ylim)) 
#     ylim <- c(1,res[2])
#   
#   if (!add) # initialize an empty plot area if add==FALSE
#   { 
#     par(oma=c(0,0,0,0), mar=c(0,0,0,0))
#     
#     plot(NA, 
#          xlim = xlim, 
#          ylim = ylim,
#          type='n',
#          xaxs='i',
#          yaxs='i',
#          xaxt='n',
#          yaxt='n',
#          xlab='',
#          ylab='',
#          bty='o')
#   }
#   rasterImage(mat,1,1, res[1], res[2])
#   
#   invisible(list(res=res,
#                  mat = mat))
# }
# 
# 
# 
# plotIMGMat <- function(imgMat)
#   {
#   tmp <- tempfile(fileext = '.png')
#   
#   writePNG(imgMat, target = tmp)  
#   
#   plotIMG(tmp)
# }




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



#





tryDownload <- function(path, Update = T, showLoad = T, downloadDir){
  printLog(paste('tryDownload was called with ', path, downloadDir ))
  
  if(!is.url(path)) {
    if(file.exists(path))
      return(path)
    else
      return(NULL)
  } else if(!url.exists(path))return(NULL)
  
  fname <- basename(path)
  
  if(showLoad)showModal(strong(
    modalDialog(HTML(paste0('Loading ', fname , '...')),
                easyClose = F,
                size = 's',
                style='background-color:#3b3a35; color:#fce319; ',
                footer = NULL
    )), session=shiny::getDefaultReactiveDomain())
  
  
  
  destfile <- paste0(downloadDir, '/', fname)
  
  if(!file.exists(destfile)) 
    download.file(path, destfile = destfile, method = 'curl')
  else
    if(as.Date(file.info(destfile)$mtime)<Sys.Date()&Update) 
      download.file(path, destfile = destfile, method = 'curl')
  
  if(showLoad)removeModal()
  
  destfile
}


getNAfromLast <- function(x){
  xrev <- rev(x)
  xnew <- xrev
  repeat{
    w <- which(is.na(xrev))
    xnew[w] <- xrev[w+1]
    if(identical(xnew, xrev)) break
    xrev <- xnew
  }
  rev(xrev)
}

putImageFooter <- function(id, mrgDT, footer='', grid = T, cex = NULL){
  if(is.null(cex)) {
    dummy <- 0
    session <- shiny::getDefaultReactiveDomain()
    cex <- session$clientData$output_imagePlot_width/180
  }
  Date <- mrgDT[ID==id, Date] 
  if(length(Date)==0) return()
  Haze <- mrgDT[ID==id, Haze]
  Black <- signif(mrgDT[ID==id, blackness], 2)
  
  if(grid){
    usr <- par()$usr
    abline(v=seq(usr[1], usr[2], length.out = 10), lty=2, col='yellow', lwd = 2)
    abline(h=seq(usr[3], usr[4], length.out = 10), lty=2, col='yellow', lwd = 2)
  }
  
  rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]*.05, col = 'white')
  mtext(side = 1, Date, line = -1, adj = .05, col = 'black', font = 2, cex = cex)
  mtext(side = 1, footer, line = -1, adj = .5, col = 'black', font = 2, cex = cex)
  mtext(side = 1, Black, line = -1, adj = .85, col = 'black', font = 2, cex = cex)
  mtext(side = 1, Haze, line = -1, adj = .95, col = 'black', font = 2, cex = cex)
  
}


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




addMaskPlot <- function(mask, add = T, col='black'){
  wd <- getwd()
  setwd(tmpDir())
  writeTIFF(mask*1, 'tmp.tif')
  rmask <- raster('tmp.tif')
  rmask[rmask!=0] <- NA
  
  plot(rmask,legend=F, add=T, col=col)
  # file.remove('tmp.tif')
  setwd(wd)
}

rotate <- function(x) t(apply(x, 2, rev))

rotateRGB <- function(imgMat){
  if(dim(imgMat)[3]!=3) stop('matrix must have 3 layers in the 3rd dimension!')
  r <- rotate(imgMat[,,1])
  g <- rotate(imgMat[,,2])
  b <- rotate(imgMat[,,3])
  
  rot <- abind(r, g, b, along = 3)
  rot
}
