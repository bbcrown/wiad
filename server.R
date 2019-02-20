#######################################################################
# The server side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

shinyServer(function(input, output, session)
{
  
  rv <- reactiveValues(
    imgMat = readPNG('not_loaded.png')[,,1:3],
    notLoaded = TRUE,
    procband = 'RGB',
    check_table = 0, 
    ringTable = data.table(no = integer(),
                           x = numeric(),
                           y = numeric(),
                           relx = numeric(),
                           rely = numeric(),
                           type = character(),
                           year = integer()
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
                 updateRadioButtons(session, 'confirmMeta', selected = 'Metadata Not Confirmed!')
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
                   sampleYear = input$sampleYear,
                   sampleLoc = input$sampleLoc,
                   sampleNote = input$sampleNote,
                   ringData = rv$ringTable
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
      
      tmp/3
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
                               type = character(),
                               year = integer()
    )
    rv$check_table <- rv$check_table + 1
  })
  
  observeEvent(input$linkerPoint, {
    if(rv$notLoaded==TRUE) return()
    
    if (nrow(rv$ringTable) == 0)
    {
      showModal(strong(
        modalDialog("No ring point is identified yet!",
                    easyClose = T,
                    fade = T,
                    size = 's',
                    style='background-color:#3b3a35; color:#fce319; ',
                    footer = NULL
        )))
      return()
      
    }else if (nrow(rv$ringTable) == 1){
      showModal(strong(
        modalDialog("First point cannot be a linker!",
                    easyClose = T,
                    fade = T,
                    size = 's',
                    style='background-color:#3b3a35; color:#fce319; ',
                    footer = NULL
        )))
      return()
    }else {
      dummy <- 0
      rv$ringTable[no==nrow(rv$ringTable), type:=switch(type, 
                                                        'Linker' = 'Normal',
                                                        'Normal' = 'Linker')
                   ]
      rv$check_table <- rv$check_table + 1
      # print(rv$ringTable)
      # dummy <- 0
    }
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
                                 type = character(),
                                 year = integer()
      )
    rv$check_table <- rv$check_table + 1
  })
  
  observeEvent(input$ring_point,{
    # printLog(paste('input$ring_point was updated with:', '\t',input$ring_point$x, input$ring_point$y))
    
    if(rv$notLoaded==TRUE) return()
    
    if(input$confirmMeta=='Metadata Not Confirmed!') {
      showModal(strong(
        modalDialog("First review and confirm the metadata!",
                    easyClose = T,
                    fade = T,
                    size = 's',
                    style='background-color:#3b3a35; color:#fce319; ',
                    footer = NULL
        )))
      return()
    }
    
    dummy =0
    
    no <- ifelse(is.null(rv$ringTable), 1, nrow(rv$ringTable) + 1)
    
    newPoint <- data.table(no = no,
                           x = input$ring_point$x,
                           y = input$ring_point$y,
                           relx = input$ring_point$x/input$ring_point$domain$right,
                           rely = input$ring_point$y/input$ring_point$domain$top,
                           type = 'Normal',
                           year = NA
    )
    if(nrow(rv$ringTable)>0){
      last <- rv$ringTable[nrow(rv$ringTable)]
      if(newPoint$x==last$x&newPoint$y==last$y) return()
    }
    rv$ringTable <- rbind(rv$ringTable, 
                          newPoint)
  })
  
  growthTable <- reactive({
    req(rv$check_table)
    growth_table <- rv$ringTable
    
    growth <- sqrt(diff(growth_table$x)^2 + diff(growth_table$y)^2)
    
    if(input$barkSide=='Bark First')
      growth_table$growth <- c(0, growth)
    else
      growth_table$growth <- c(growth, 0)
    
    growth_table[type=='Linker', growth:=NA]  
    growth_table
    })
  
  observe({
    req(input$barkSide)
    req(rv$ringTable)
    req(rv$check_table)
    
    types <- rv$ringTable$type
    n <- length(types)
    years <- rep(NA, n)
    
    if(input$barkSide=='Bark First'){
      for(i in 1:n)
        years[i] <- ifelse(i==1,
                           input$sampleYear,
                           ifelse(types[i]=='Linker', 
                                  years[i-1],
                                  years[i-1] - 1)
                           )
    }else{
      for(i in n:1)
        years[i] <- ifelse(i==n,
                           input$sampleYear,
                           ifelse(types[i]=='Linker', 
                                  years[i+1],
                                  years[i+1] - 1))
    }
    rv$ringTable$year <- years
  })
  
  output$ring_table <- renderDataTable(
    {
      tbl <- growthTable()
      if(nrow(tbl)==0) return()
      req(rv$check_table)
      tbl[order(-no)]
    },
    
    options = list(pageLength = 5)
  )
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0('ringdata-', format(Sys.time(), format = '%Y-%m-%d-%H%M%S'), ".csv")
      
    },
    content = function(file) {
      tbl <- growthTable()
      if(nrow(tbl)==0) return()

      write.table(tbl, file, sep = ',', row.names = F)
      
    }
  )
  
  output$downloadJSON <- downloadHandler(
    filename = function() {
      paste0('ringdata-', format(Sys.time(), format = '%Y-%m-%d-%H%M%S'), ".json")
      
    },
    content = function(file) {
      
      tbl <- growthTable()
      
      if(nrow(tbl)==0) return()
      
      tbl %>% 
        toJSON() %>%
        write_lines(file)
    }
  )
 
  observeEvent(input$confirmMeta, {
    if(input$confirmMeta=='Metadata Not Confirmed!') return()
    if(year(input$sampleDate)!=input$sampleYear){
      showModal(strong(
        modalDialog("Year does not match the sample's date!",
                    easyClose = T,
                    fade = T,
                    size = 's',
                    style='background-color:#3b3a35; color:#fce319; ',
                    footer = NULL
        )))
      
      updateRadioButtons(session, inputId = 'confirmMeta', selected = 'Metadata Not Confirmed!')
    }
      
    
  })
  
}
)