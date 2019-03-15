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
  
  printLog(init = TRUE)
  
  # declaring reactive value
  rv <- reactiveValues(
    
    imgMat = readPNG('not_loaded.png')[,,1:3], #RGB matrix loaded based on the image
    notLoaded = TRUE, # whether the first image is loaded
    procband = 'RGB', # processed matrix from the raw RGB
    check_table = 0, # a flag to update the table
    
    ringTable = data.table( # data.table contains the ring data
      no = integer(), # no ID
      x = numeric(), # x
      y = numeric(), # y
      relx = numeric(), # relative x
      rely = numeric(), # relative y
      type = character(), # type
      year = integer() # year
    ))
  
  # update the image aspect ratio
  observeEvent(rv$imgMat,
               {
                 printLog('observeEvent rv$imgMat')
                 
                 imgDim <- dim(rv$imgMat)
                 rv$imgAsp <- imgDim[2] /imgDim[1]  
               }
  )
  
  #=================================================================
  # 
  
  observeEvent(input$image,
               {
                 printLog('observeEvent input$image')
                 
                 updateRadioButtons(session = session, 
                                    inputId = 'confirmMeta', 
                                    selected = 'Not Confirmed')
                 
                 rv$wrkID <- paste(gsub(x = as.character(Sys.time()), 
                                         pattern = ' |:', 
                                         replacement = '-'),
                                    paste(sample(x = c(0:9, letters, LETTERS),
                                                 size = 32,
                                                 replace=TRUE),
                                          collapse=""), 
                                   sep = '_'
                                    )
                 
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
                 
                 writePNG(rv$imgMat,  
                          
                          paste0(rv$wrkDir,
                                 'imgorg-',
                                 rv$wrkID,
                                 '.png'))
                 
                 if(imgDim[2]<imgDim[1]) {
                   rv$imgMat <- rotateRGB(rv$imgMat)
                 }
                 
                 
                 
                 
                 
                 updateNumericInput(session = session,
                                    inputId = 'sampleDPI',
                                    value = NULL)
                 
                 rv$ringTable <-  data.table( # data.table contains the ring data
                   no = integer(), # no ID
                   x = numeric(), # x
                   y = numeric(), # y
                   relx = numeric(), # relative x
                   rely = numeric(), # relative y
                   type = character(), # type
                   year = integer() # year
                 )
               }
  )
  
  metaData <- reactive(
    {
      printLog('metaData reactive')
      
      meta <- list(ownerName = input$ownerName, 
                   ownerEmail = input$ownerEmail,
                   species = input$spp,
                   sampleDate = input$sampleDate,
                   sampleYear = input$sampleYear,
                   sampleDPI = input$sampleDPI,
                   sampleLoc = input$sampleLoc,
                   sampleNote = input$sampleNote,
                   ringData = rv$ringTable,
                   growth = growthTable(), 
                   status = input$confirmMeta
      )
      
      toJSON(meta)
      
    }
  )
  
  autoInvalidate <- reactiveTimer(2000)
  
  
  # observeEvent(input$saveData,
  #              {
  #                printLog('observeEvent input$saveData')
  #                
  #                writePNG(imgProcessed(), 
  #                         target = paste0(rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
  #                
  #                writePNG(rv$imgMat, 
  #                         target = paste0(rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
  #                
  #                write(metaData(), 
  #                      paste0(rv$wrkDir, 'meta-', rv$wrkID,'.json'))
  #                
  #                showModal(
  #                  strong(
  #                    modalDialog('Sample and metadata were stored!', 
  #                                style='background-color:#3b3a35; color:#fce319; ',
  #                                footer = NULL, 
  #                                easyClose = T, 
  #                                size = 'm')
  #                  )
  #                )
  #              }
  # )
  
  output$imageProc <- renderPlot(
    width = function(){
      floor(input$zoomlevel)
    },
    height = function(){
      # floor(session$clientData$output_imageProc_width/rv$imgAsp)
      floor(input$zoomlevel/rv$imgAsp)
    },
    {
      printLog('output$imageProc renderPlot')
      
      imgtmp <- imgProcessed()
      if(is.null(imgtmp)) return()
      
      
      imgDim <- dim(imgtmp)
      par(mar= c(0,0,0,0), xaxs = 'i', yaxs = 'i')
      plot(NA, 
           xlim = c(1,imgDim[1]),
           ylim = c(1,imgDim[2]),
           type='n', 
           axes= FALSE, 
           xlab= '',
           ylab = '')
      
      window <- par()$usr
      
      rasterImage(imgtmp, 
                  window[1], 
                  window[3], 
                  window[2], 
                  window[4])
      
      ring_tbl <- rv$ringTable[, .(x, y)]
      
      ring_tbl[, points(x,
                        y, 
                        pch = 19, 
                        cex = 2, 
                        col = 'yellow')]
      
      wLinkers <- which(rv$ringTable$type=='Linker')
      
      segments(x0 = rv$ringTable[wLinkers, x], 
               y0 = rv$ringTable[wLinkers, y],
               x1 = rv$ringTable[wLinkers - 1, x], 
               y1 = rv$ringTable[wLinkers - 1, y], 
               lwd = 2, 
               col = 'yellow')
      
      if(nrow(ring_tbl)>1){
        
        ab <- lm(ring_tbl[(nrow(ring_tbl)-1):nrow(ring_tbl)],
                 formula = y~x)
        
        abline(ab, 
               col = 'yellow',
               lwd = 2, 
               lty = 2)
      }
    })
  
  
  observeEvent(input$selRed,
               {
                 printLog('observeEvent input$selRed')
                 
                 rv$procband <- 'Red'
               }
  )
  
  
  observeEvent(input$selBlue,
               {
                 printLog('observeEvent input$selBlue')
                 
                 rv$procband <- 'Blue'
               }
  )
  
  
  observeEvent(input$selGreen,
               {
                 printLog('observeEvent input$selGreen')
                 
                 rv$procband <- 'Green'
               }
  )
  
  observeEvent(input$selHue,
               {
                 printLog('observeEvent input$selHue')
                 
                 rv$procband <- 'Hue'
               }
  )
  
  
  observeEvent(input$selSat,
               {
                 printLog('observeEvent input$selSat')
                 
                 rv$procband <- 'Saturation'
               }
  )
  
  observeEvent(input$selValue,
               {
                 printLog('observeEvent input$selValue')
                 
                 rv$procband <- 'Value'
               }
  )
  
  observeEvent(input$selBright,
               {
                 printLog('observeEvent input$selBright')
                 
                 rv$procband <- 'Brightness'
               }
  )
  
  observeEvent(input$selDark,
               {
                 printLog('observeEvent input$selDark')
                 
                 rv$procband <- 'Darkness'
               }
  )
  
  observeEvent(input$selContrast,
               {
                 printLog('observeEvent input$selContrast')
                 
                 rv$procband <- 'Contrast'
               }
  )
  
  observeEvent(input$selTotBr,
               {
                 printLog('observeEvent input$selTotBr')
                 
                 rv$procband <- 'Total Brightness'
               }
  )
  
  observeEvent(input$selRGB,
               {
                 printLog('observeEvent input$selRGB')
                 
                 rv$procband <- 'RGB'
               }
  )
  
  totbrightness <- reactive(
    {
      printLog('totbrightness reactive')
      
      tmp <- 
        rv$imgMat[,,1] + 
        rv$imgMat[,,2] + 
        rv$imgMat[,,3]
      
      tmp/3
    }
  )
  
  brightness <- reactive(
    {
      printLog('brightness reactive')
      
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getBrightness(rv$imgMat)
      tmp
    }
  )
  
  darkness <- reactive(
    {
      printLog('darkness reactive')
      
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getDarkness(rv$imgMat)
      tmp
    }
  )
  
  
  contrast <- reactive(
    {
      printLog('contrast reactive')
      
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getContrast(rv$imgMat)
      tmp
    }
  )
  
  
  imgProcessed <- reactive(
    {
      printLog('imgProcessed reactive')
      
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
      
      # clhsv <- clRGB2HSV(rv$imgMat)
      
      switch(rv$procband,
             'RGB'=rv$imgMat,
             
             'Red'=rv$imgMat[,,1],
             'Green'=rv$imgMat[,,2],
             'Blue'=rv$imgMat[,,3],
             
             # 'Hue'=clhsv[,,1],
             # 'Saturation'=clhsv[,,2],
             # 'Value'=clhsv[,,3],
             # 
             # 'Brightness' = brightness(),
             # 'Darkness' =darkness(),
             # 'Contrast' = contrast(),
             
             'Total Brightness' = totbrightness()
      )
      
    }
  )
  
  observeEvent(input$clearCanvas, 
               {
                 printLog('observeEvent input$clearCanvas')
                 
                 printLog(paste('input$clearCanvas was changed to:', '\t',input$clearCanvas))
                 
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
  
  observeEvent(input$linkerPoint, 
               {
                 printLog('observeEvent input$linkerPoint')
                 
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
  
  observeEvent(input$undoCanvas, 
               {
                 
                 printLog('observeEvent input$undoCanvas')
                 
                 if(rv$notLoaded==TRUE) return()
                 
                 if (nrow(rv$ringTable) > 1)
                   rv$ringTable <- rv$ringTable[-nrow(rv$ringTable),]
                 else
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
  
  observeEvent(input$ring_point,
               {
                 
                 printLog('observeEvent input$ring_point')
                 
                 if(rv$notLoaded==TRUE) return()
                 
                 if(input$confirmMeta=='Not Confirmed') {
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
      growth_table$pixels <- c(0, growth)
    else
      growth_table$pixels <- c(growth, 0)
    
    growth_table[type=='Linker', growth:=NA]  
    growth_table[,growth:=pixels/as.numeric(input$sampleDPI)*25.4]
    growth_table
  })
  
  observe(
    {
      printLog('rv$ringTable$year <- years')
      
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
      printLog('output$ring_table renderDataTable')
      
      tbl <- growthTable()
      
      if(nrow(tbl)==0) 
        return()
      
      req(rv$check_table)
      
      tbl[order(-no)]
    },
    
    options = list(pageLength = 5)
  )
  
  output$downloadCSV <- downloadHandler(
    
    filename = function() {
      
      printLog('output$downloadCSV downloadHandler filename')
      
      paste0('ringdata-', 
             format(Sys.time(),
                    format = '%Y-%m-%d-%H%M%S'),
             ".csv")
      
    },
    content = function(file) {
      
      printLog('output$downloadCSV downloadHandler content')
      
      if(!rv$notLoaded) {
        writePNG(imgProcessed(), 
                 target = paste0(rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        writePNG(rv$imgMat, 
                 target = paste0(rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
        
        write(metaData(), 
              paste0(rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      tbl <- growthTable()
      
      if(nrow(tbl)==0) 
        return()
      
      write.table(tbl, 
                  file, 
                  sep = ',',
                  row.names = F)
      
    }
  )
  
  output$downloadJSON <- downloadHandler(
    
    filename = function() {
      
      printLog('output$downloadJSON downloadHandler filename')
      
      paste0('ringdata-', 
             format(Sys.time(),
                    format = '%Y-%m-%d-%H%M%S'),
             ".json")
      
    },
    
    content = function(file) {
      
      printLog('output$downloadJSON downloadHandler content')
      
      if(!rv$notLoaded) {
        writePNG(imgProcessed(), 
                 target = paste0(rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        writePNG(rv$imgMat, 
                 target = paste0(rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
        
        write(metaData(), 
              paste0(rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      data <- list(growth_table = growthTable(),
                   meta_data = metaData())
      
      # if(nrow(tbl)==0) 
      #   return()
      
      data %>% 
        toJSON() %>%
        write_lines(file)
    }
  )
  
  observeEvent(input$confirmMeta, {
    
    printLog('observeEvent input$confirmMeta')
    
    if(input$confirmMeta=='Not Confirmed') return()
    
    if(year(input$sampleDate)!=input$sampleYear){
      
      showModal(strong(
        
        modalDialog("Year does not match the sample's date!",
                    easyClose = T,
                    fade = T,
                    size = 's',
                    style='background-color:#3b3a35; color:#fce319; ',
                    footer = NULL
        )))
      
      updateRadioButtons(session, 
                         inputId = 'confirmMeta', 
                         selected = 'Not Confirmed')
    }
  })
  
  output$ring_plot <- renderPlotly({
    
    printLog('output$ring_plot renderPlotly')
    
    tbl <- growthTable()
    
    if(nrow(tbl)==0) 
      return()
    
    
    fontList <- list(
      family = "Courier New, monospace",
      size = 16,
      color = "#7f7f7f"
    )
    
    xAxis <- list(
      title = "Year",
      titlefont = fontList
    )
    
    yAxis <- list(
      title = ifelse(test = is.null(input$sampleDPI),
                     yes = "Growth (mm)",
                     no ="Growth (pixels)"),
      titlefont = fontList
    )
    
    tbl[,year:=as.factor(year)]
    
    data <- tbl[type!='Linker']
    
    p <- plot_ly(data = data, 
                 x=~year, 
                 y= ~growth,
                 type = 'bar',
                 marker=list(color='#e26828')
    ) %>%
      layout(xaxis = xAxis,
             yaxis = yAxis)
    
    p$elementId <- NULL
    
    
    return(p)
    
  }
  
  )
  
  observeEvent(input$rotate180,{
    rv$imgMat <- rotateRGB(rotateRGB(rv$imgMat))
  })
  printLog(finit = TRUE)
}
)