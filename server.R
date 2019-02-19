#######################################################################
# The server side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

shinyServer(function(input, output, session)
{
  
  rv <- reactiveValues(
    imgMat = readPNG('not_loaded.png')[,,1:3],
    notLoaded = TRUE,
    procband = 'RGB',
    ringTable = data.table(no = integer(),
                           x = numeric(),
                           y = numeric(),
                           relx = numeric(),
                           rely = numeric(),
                           type = character()
    ))
  
  observeEvent(rv$imgMat,
               {
                 imgDim <- dim(rv$imgMat)
                 rv$imgAsp <- imgDim[2] /imgDim[1]  
               }
               
  )
  
  #=================================================================
  # 
  
  observeEvent(input$image,
               {
                 
                 rv$wrkID <- gsub(as.character(Sys.time()), pattern = ' |:', replacement = '-')
                 
                 rv$wrkDir <- paste0('images/W-', rv$wrkID, '/')
                 
                 dir.create(rv$wrkDir)
                 
                 rv$imgPath <- input$image$datapath
                 
                 rv$imgExt <- file_ext(rv$imgPath)
                 
                 if(rv$imgExt%in%c('jpg', 'jpeg')) 
                   rv$imgMat <- readJPEG(rv$imgPath)
                 else 
                   if(rv$imgExt%in%c('tiff', 'tif')) 
                     rv$imgMat <- readTIFF(rv$imgPath)
                 else 
                   if(rv$imgExt%in%c('png')) 
                     rv$imgMat <- readPNG(rv$imgPath)[,,1:3]
                 else   {      
                   showModal(strong(
                     modalDialog("Only JPEG, TIFF or PNG files are accpeted.",
                                 easyClose = T,
                                 fade = T,
                                 size = 's',
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 footer = NULL
                     )))
                   return()
                 }
                 
                 rv$notLoaded <- FALSE
                 
                 imgDim <- dim(rv$imgMat)
                 
                 if(imgDim[2]<imgDim[1]) {
                   rv$imgMat <- rotateRGB(rv$imgMat)
                 }
                 
                 
                 writePNG(rv$imgMat,  
                          paste0(rv$wrkDir, 'orig-',rv$wrkID, '.png'))
                 
               }
  )
  
  metaData <- reactive(
    {
      
      meta <- list(ownerName = input$ownerName, 
                   ownerEmail = input$ownerEmail,
                   species = input$spp,
                   sampleDate = input$sampleDate,
                   sampleLoc = input$sampleLoc,
                   sampleNote = input$sampleNote
      )
      
      toJSON(meta)
      
    }
  )
  
  observeEvent(input$saveData,
               {
                 write(metaData(), 
                       paste0(rv$wrkDir, 'meta-', rv$wrkID,'.json'))
                 
                 showModal(
                   strong(
                     modalDialog('Sample and metadata were stored!', 
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 footer = NULL, 
                                 easyClose = T, 
                                 size = 'm')
                   )
                 )
               }
  )
  
  output$imageProc <- renderPlot(
    height = function(){
      floor(session$clientData$output_imageProc_width/rv$imgAsp)
    },
    {
      imgtmp <- imgProcessed()
      if(is.null(imgtmp)) return()
      
      imgDim <- dim(imgtmp)
      par(mar= c(0,0,0,0), xaxs = 'i', yaxs = 'i')
      plot(NA, 
           xlim = c(1,imgDim[1]),
           ylim = c(1,imgDim[2]),
           type='n', axes= FALSE, xlab= '', ylab = '')
      window <- par()$usr
      rasterImage(imgtmp, window[1], window[3], window[2], window[4])
      ring_tbl <- rv$ringTable[, .(x, y)]
      ring_tbl[, points(x, y, pch = 19, cex = 2, col = 'yellow')]
      
      if(nrow(ring_tbl)>1){
        ab <- lm(ring_tbl[(nrow(ring_tbl)-1):nrow(ring_tbl)],
                 formula = y~x)
        abline(ab, col = 'yellow', lwd = 2, lty = 2)
      }
    })
  
  
  observeEvent(input$selRed,
               {
                 rv$procband <- 'Red'
               }
  )
  
  
  observeEvent(input$selBlue,
               {
                 rv$procband <- 'Blue'
               }
  )
  
  
  observeEvent(input$selGreen,
               {
                 rv$procband <- 'Green'
               }
  )
  
  observeEvent(input$selHue,
               {
                 rv$procband <- 'Hue'
               }
  )
  
  
  observeEvent(input$selSat,
               {
                 rv$procband <- 'Saturation'
               }
  )
  
  observeEvent(input$selValue,
               {
                 rv$procband <- 'Value'
               }
  )
  
  observeEvent(input$selBright,
               {
                 rv$procband <- 'Brightness'
               }
  )
  
  observeEvent(input$selDark,
               {
                 rv$procband <- 'Darkness'
               }
  )
  
  observeEvent(input$selContrast,
               {
                 rv$procband <- 'Contrast'
               }
  )
  
  observeEvent(input$selTotBr,
               {
                 rv$procband <- 'Total Brightness'
               }
  )
  
  observeEvent(input$selRGB,
               {
                 rv$procband <- 'RGB'
               }
  )
  
  totbrightness <- reactive(
    {
      tmp <- 
        rv$imgMat[,,1] + 
        rv$imgMat[,,2] + 
        rv$imgMat[,,3]
      
      tmp
    }
  )
  
  brightness <- reactive(
    {
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getBrightness(rv$imgMat)
      tmp
    }
  )
  
  darkness <- reactive(
    {
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getDarkness(rv$imgMat)
      tmp
    }
  )
  
  
  contrast <- reactive(
    {
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getContrast(rv$imgMat)
      tmp
    }
  )
  
  
  imgProcessed <- reactive(
    {
      
      if(is.null(rv$imgMat)) 
        return()
      
      if(length(dim(rv$imgMat))==2)
      {
        showModal(strong(
          modalDialog("The image is monochrome!",
                      easyClose = T,
                      fade = T,
                      size = 's',
                      style='background-color:#3b3a35; color:#fce319; ',
                      footer = NULL
          )))
        return(rv$imgMat)
      }
      
      clhsv <- clRGB2HSV(rv$imgMat)
      
      switch(rv$procband,
             'RGB'=rv$imgMat,
             
             'Red'=rv$imgMat[,,1],
             'Green'=rv$imgMat[,,2],
             'Blue'=rv$imgMat[,,3],
             
             'Hue'=clhsv[,,1],
             'Saturation'=clhsv[,,2],
             'Value'=clhsv[,,3],
             
             'Brightness' = brightness(),
             'Darkness' =darkness(),
             'Contrast' = contrast(),
             
             'Total Brightness' = totbrightness()
      )
      
    }
  )
  
  observeEvent(input$clearCanvas, {
    # printLog(paste('input$clearCanvas was changed to:', '\t',input$clearCanvas))
    if(rv$notLoaded==TRUE) return()
    
    rv$slideShow <- 0 
    rv$ringTable <- data.table(no = integer(),
                               x = numeric(),
                               y = numeric(),
                               relx = numeric(),
                               rely = numeric(),
                               type = character()
    )
  })
  
  
  observeEvent(input$undoCanvas, {
    
    # printLog(paste('input$undoCanvas was changed to:', '\t',input$undoCanvas))
    if(rv$notLoaded==TRUE) return()
    
    if (nrow(rv$ringTable) > 1)
      rv$ringTable <- rv$ringTable[-nrow(rv$ringTable),]
    # else 
    #   if (nrow(rv$ringTable) == 2)
    #     rv$ringTable <- matrix(rv$ringTable[1,], 1, 5)
    else
      # if (nrow(rv$ringTable) == 1)
      rv$ringTable <- data.table(no = integer(),
                                 x = numeric(),
                                 y = numeric(),
                                 relx = numeric(),
                                 rely = numeric(),
                                 type = character()
      )
  })
  
  observeEvent(input$ring_point,{
    # printLog(paste('input$ring_point was updated with:', '\t',input$ring_point$x, input$ring_point$y))
    
    if(rv$notLoaded==TRUE) return()
    dummy =0
    
    no <- ifelse(is.null(rv$ringTable), 1, nrow(rv$ringTable) + 1)
    
    newPoint <- data.table(no = no,
                           x = input$ring_point$x,
                           y = input$ring_point$y,
                           relx = input$ring_point$x/input$ring_point$domain$right,
                           rely = input$ring_point$y/input$ring_point$domain$top,
                           type = 'normal'
    )
    
    rv$ringTable <- rbind(rv$ringTable, 
                          newPoint)
    dummy =0
    
  })
  
  output$ring_table <- renderDataTable(rv$ringTable,
                                       options = list(pageLength = 5)
  )
  
  
}
)