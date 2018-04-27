#######################################################################
# The server side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

shinyServer(function(input, output, session)
{
  
  rv <- reactiveValues(imgAsp=1, 
                       procband = 'RGB')
  
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
                     rv$imgMat <- readPNG(rv$imgPath)
                 else 
                   stop('wrong extension!')
                 
                 rv$imgDim <- dim(rv$imgMat)
                 
                 rv$imgAsp <- rv$imgDim[1] /
                   rv$imgDim[2]
                 
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
  
  output$imageOrig <- renderPlot(
    # height = function()
    # {
    #   floor(session$clientData$output_imageOrig_width/rv$imgAsp)
    # }
    # ,
    width = function()
    {
      floor(session$clientData$output_imageOrig_height/rv$imgAsp)
    },
    
    {
      if(is.null(rv$imgMat)) 
        return()
      
      # plotIMG(rv$outPath)
      plotIMGMat(rv$imgMat)
    }
  )
  
  output$imageProc <- renderPlot(
    # height = function(){
    #   floor(session$clientData$output_imageOrig_width/rv$imgAsp)
    # },
    
    width = function(){
      floor(session$clientData$output_imageOrig_height/rv$imgAsp)
    },
    {
      if(is.null(imgProcessed()))
        return()
      
      dummy <- 0
      dummy <- 0
      
      plotIMGMat(imgProcessed())
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
                 rv$procband <- 'TotBrightness'
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
             'TotBrightness' = totbrightness()
      )
      
    }
  )
  
  
}
)