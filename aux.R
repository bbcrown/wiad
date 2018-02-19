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
plotIMG <- function(path, add=FALSE, xlim = NULL, ylim = NULL)
{
  ext <- file_ext(path)
  
  if(ext%in%c('jpg', 'jpeg')) mat <- readJPEG(path)
  else if(ext%in%c('tiff', 'tif')) mat <- readTIFF(path)
  else if(ext%in%c('png')) mat <- readPNG(path)
  else stop('wrong extension!')
  
  res <-  dim(mat)[2:1] # get the resolution
  if(is.null(xlim)) xlim <- c(1,res[1])
  if(is.null(ylim)) ylim <- c(1,res[2])
  
  if (!add) # initialize an empty plot area if add==FALSE
  { 
    par(oma=c(0,0,0,0), mar=c(0,0,0,0))
    plot(NA, xlim = xlim, ylim = ylim, type='n',
         xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
  }
  rasterImage(mat,1,1,res[1],res[2])
  invisible(list(res=res, mat = mat))
}



plotIMGMat <- function(imgMat){
  tmp <- tempfile(fileext = '.png')
  writePNG(imgMat, target = tmp)  
  plotIMG(tmp)
}
