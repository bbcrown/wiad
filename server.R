shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(imgAsp=1)
  
  observeEvent(input$image,{
    rv$wrkID <- gsub(as.character(Sys.time()), pattern = ' |:', replacement = '-')
    rv$wrkDir <- paste0('images/W-', rv$wrkID, '/')
    dir.create(rv$wrkDir)
    
    rv$imgPath <- input$image$datapath
    rv$imgExt <- file_ext(rv$imgPath)
    
    if(rv$imgExt%in%c('jpg', 'jpeg')) rv$imgMat <- readJPEG(rv$imgPath)
    else if(rv$imgExt%in%c('tiff', 'tif')) rv$imgMat <- readTIFF(rv$imgPath)
    else if(rv$imgExt%in%c('png')) rv$imgMat <- readPNG(rv$imgPath)
    else stop('wrong extension!')
    
    rv$imgDim <- dim(rv$imgMat)
    rv$imgAsp <- rv$imgDim[1]/rv$imgDim[2]
    
    rv$imgMatProc <- rv$imgMat
    
    writePNG(rv$imgMat,  paste0(rv$wrkDir, 'orig-',rv$wrkID, '.png'))
    
  })
  
  metaData <- reactive({
    meta <- list(ownerName = input$ownerName, 
                 ownerEmail = input$ownerEmail,
                 species = input$spp,
                 sampleDate = input$sampleDate,
                 sampleLoc = input$sampleLoc,
                 sampleNote = input$sampleNote
                 
    )
    toJSON(meta)
  })
  
  observeEvent(input$saveData,{
    write(metaData(), paste0(rv$wrkDir, 'meta-', rv$wrkID,'.json'))
    showModal(strong(modalDialog('Sample and metadata were stored!', 
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 footer = NULL, easyClose = T, size = 'm')))  })
  
  output$imageOrig <- renderPlot(
    # height = function(){floor(session$clientData$output_imageOrig_width/rv$imgAsp)},
    width = function(){floor(session$clientData$output_imageOrig_height/rv$imgAsp)},
    
    {
      if(is.null(rv$imgMat)) return()
      # plotIMG(rv$outPath)
      plotIMGMat(rv$imgMat)
    })
  
  output$imageProc <- renderPlot(
    # height = function(){floor(session$clientData$output_imageOrig_width/rv$imgAsp)},
    width = function(){floor(session$clientData$output_imageOrig_height/rv$imgAsp)},
    {
    if(is.null(rv$imgMatProc)) return()
    plotIMGMat(rv$imgMatProc)
  })
}
)