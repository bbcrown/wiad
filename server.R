#######################################################################
# The server side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

# increase maximal size of images to 100 MB
options (shiny.maxRequestSize = 100 * 1024^2)

shinyServer (function (input, output, session)
{
  
  # sent the initial log message
  printLog (init = TRUE)
  
  
  # declaring reactive value
  rv <- reactiveValues (
    
    imgMat = readPNG ('not_loaded.png')[,,1:3], #RGB matrix loaded based on the image
    notLoaded = TRUE, # whether the first image is loaded
    procband = 'RGB', # processed matrix from the raw RGB
    check_table = 0, # a flag to update the table
    
    markerTable = data.table ( # data.table contains the marker data
      no   = integer (), # no ID
      x    = numeric (), # x
      y    = numeric (), # y
      relx = numeric (), # relative x
      rely = numeric (), # relative y
      type = character ())
    )
  
  # update the image aspect ratio
  observeEvent (rv$imgMat,
               {
                 printLog('observeEvent rv$imgMat')
                 
                 imgDim <- dim (rv$imgMat)
                 rv$imgAsp <- imgDim[2] / imgDim[1]  
               }
  )
  
  #=================================================================
  # whenever new image was uploaded
  observeEvent (input$image,
               {
                 printLog('observeEvent input$image')
                 
                 updateRadioButtons (session = session, 
                                     inputId = 'confirmMeta', 
                                     selected = 'Not Confirmed')
                 
                 rv$wrkID <- paste (gsub (x = as.character (Sys.time()), 
                                          pattern = ' |:', 
                                          replacement = '-'),
                                   paste (sample (x = c (0:9, letters, LETTERS),
                                                  size = 32,
                                                  replace = TRUE),
                                          collapse = ""), 
                                   sep = '_'
                 )
                 
                 # set the sub directory for the sample
                 rv$wrkDir <- paste0 (ARCHIVE_DIR, 'W-', rv$wrkID, '/')
                 
                 # create the sub directory for the sample
                 dir.create (rv$wrkDir)
                 
                 # get path to image
                 rv$imgPath <- input$image$datapath
                 
                 # get file extension
                 rv$imgExt <- file_ext (rv$imgPath)
                 
                 # read image
                 if (rv$imgExt %in% c ('jpg', 'jpeg', 'JPG', 'JPEG')) {
                   rv$imgMat <- readJPEG (rv$imgPath)
                 } else if (rv$imgExt %in% c('tiff', 'tif', 'TIF', 'TIFF')) {
                   rv$imgMat <- readTIFF (rv$imgPath)
                 } else if (rv$imgExt %in% c ('png','PNG')) {
                   rv$imgMat <- readPNG (rv$imgPath)[,,1:3]
                 } else {      
                   showModal (strong (
                     modalDialog ("Only JPEG, TIFF or PNG files are accepted.",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#A41034; ',
                                  footer = NULL
                     )))
                   
                   return ()
                 }
                 
                 rv$notLoaded <- FALSE
                 
                 imgDim <- dim (rv$imgMat)
                 
                 writePNG (rv$imgMat,  
                          
                          paste0 (rv$wrkDir,
                                  'imgorg-',
                                  rv$wrkID,
                                  '.png'))
                 
                 if (imgDim [2] < imgDim [1]) {
                   rv$imgMat <- rotateRGB (rv$imgMat)
                 }
                 
                 updateNumericInput (session = session,
                                     inputId = 'sampleDPI',
                                     value = NULL)
                 
                 rv$markerTable <- data.table ( # data.table contains the marker data
                   no   = integer (),   # no ID
                   x    = numeric (),   # x
                   y    = numeric (),   # y
                   relx = numeric (),   # relative x
                   rely = numeric (),   # relative y
                   type = character ()  # type
                 )
               }
  )
  
  # whenever metadata is uploaded update all the metadata below # TR still needs work and review
  observeEvent (input$metadata,
               {
                 printLog ('observeEvent input$metadata')

                 # get path to metadata
                 rv$metaPath <- input$metadata$datapath
                 print (rv$metaPath)

                 # get file extension
                 rv$metaExt <- file_ext (rv$metaPath)
                 print (rv$metaExt)

                 # read metadata from .xlsx, .csv, or .json file
                 if (rv$metaExt %in% c ('xlsx', 'XLSX')) {
                   metadata <- read_excel (path = rv$metaPath,
                                           col_names = c ('ownerName','ownerEmail','species',
                                                          'sampleDate','sampleYear','sampleDPI',
                                                          'siteLoc','siteLocID','plotID',
                                                          'sampleID','sampleNote','collection',
                                                          'contributor'),
                                           col_types = c ('text','text','text','date','numeric',
                                                          'numeric','text','text','text','text',
                                                          'text','text','text'), skip = 1)
                 } else if (rv$metaExt %in% c ('csv', 'CSV')) {
                   metadata <- read_csv (file = rv$metaPath, 
                                         col_names = c ('ownerName','ownerEmail','species',
                                                        'sampleDate','sampleYear','sampleDPI',
                                                        'siteLoc','siteLocID','plotID',
                                                        'sampleID','sampleNote','collection',
                                                        'contributor'),
                                         col_types = 'cccDiiccccccc', skip = 1)
                 } else if (rv$metaExt %in% c ('json', 'JSON')) {
                   metadata <- read_json (rv$metaPath)
                 } else {
                   showModal (strong (
                     modalDialog ("Only xlsx, csv or json files are accepted for metadata.",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#A41034; ',
                                  footer = NULL)))

                   return ()
                 }
                 showModal (strong (
                   modalDialog ("Review and confirm metadata below.",
                                easyClose = T,
                                fade = T,
                                size = 's',
                                style = 'background-color:#3b3a35; color:#A41034; ',
                                footer = NULL)))
                 
                 # update metadata fields
                 updateTextInput (session = session,
                                  inputId = 'ownerName',
                                  value = metadata$ownerName)
                 updateTextInput (session = session,
                                  inputId = 'ownerEmail',
                                  value = metadata$ownerEmail)
                 updateTextInput (session = session,
                                  inputId = 'species',
                                  value = metadata$species)
                 updateTextInput (session = session,
                                  inputId = 'sampleDate',
                                  value = metadata$sampleDate)
                 updateTextInput (session = session,
                                  inputId = 'sampleYear',
                                  value = metadata$sampleYear)
                 updateTextInput (session = session,
                                  inputId = 'sampleDPI',
                                  value = metadata$sampleDPI)
                 updateTextInput (session = session,
                                  inputId = 'siteLoc',
                                  value = metadata$siteLoc)
                 updateTextInput (session = session,
                                  inputId = 'siteLocID',
                                  value = metadata$siteLocID)
                 updateTextInput (session = session,
                                  inputId = 'plotID',
                                  value = metadata$plotID)
                 updateTextInput (session = session,
                                  inputId = 'sampleNote',
                                  value = metadata$sampleNote)
                 updateTextInput (session = session,
                                  inputId = 'sampleID',
                                  value = metadata$sampleID)
                 updateTextInput (session = session,
                                  inputId = 'collection',
                                  value = metadata$collection)
                 updateTextInput (session = session,
                                  inputId = 'contributor',
                                  value = metadata$contributor)
                 
                 # make sure the metadata is reviewed
                 updateRadioButtons (session = session, 
                                     inputId = 'confirmMeta', 
                                     selected = 'Not Confirmed')
                 
# TR I need to create a metadata file in csv and json format to test this function.
               }
  )
  
  metaData <- reactive (
    {
      printLog ('metaData reactive')
      
      meta <- list (ownerName   = input$ownerName, 
                    ownerEmail  = input$ownerEmail,
                    species     = input$species,
                    sampleDate  = input$sampleDate,
                    sampleYear  = input$sampleYear,
                    sampleYearGrowth = ifelse (input$sampleYearGrowingSeason         == 'only started', 
                                               ifelse (input$sampleYearGrowingSeason == 'already ended', 
                                                       'all', 'some'), 
                                               'none'), 
                    sampleDPI   = input$sampleDPI,
                    siteLoc     = input$siteLoc,
                    siteLocID   = input$siteLocID,
                    plotID      = input$plotID,
                    sampleNote  = input$sampleNote,
                    sampleID    = input$sampleID,
                    collection  = input$collection,
                    contributor = input$contributor,
                    markerData  = rv$markerTable,
                    growth      = growthTable (), 
                    status      = input$confirmMeta)
    }
  )
  
  autoInvalidate <- reactiveTimer (2000)
  
  
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
  
  output$imageProc <- renderPlot (
    width = function(){
      floor (input$zoomlevel)
    },
    height = function(){
      # floor(session$clientData$output_imageProc_width/rv$imgAsp)
      floor (input$zoomlevel/rv$imgAsp)
    },
    {
      printLog ('output$imageProc renderPlot')
      
      imgtmp <- imgProcessed ()
      if (is.null(imgtmp)) return ()
      
      # get images dimensions
      imgDim <- dim (imgtmp)
      
      # set margins
      par (mar = c (0,0,0,0), xaxs = 'i', yaxs = 'i')
      plot (NA, 
            xlim = c (1, imgDim [2]),
            ylim = c (1, imgDim [1]),
            type = 'n', 
            axes = FALSE, 
            xlab = '',
            ylab = '') # TR Added to create scroll overflow
      
      window <- par()$usr
      
      rasterImage (imgtmp, 
                   window [1], 
                   window [3], 
                   window [2], 
                   window [4])
      
      marker_tbl <- rv$markerTable [, .(x, y)]
      
      # plot all markers in yellow
      marker_tbl [, points (x,
                            y, 
                            pch = 19, 
                            cex = 1.2, 
                            col = 'yellow')]
      
      # plot years between two markers to more easily identify the growth rings
      if (input$displayYears) {
        years <- growthTable ()
        
        # sort out all linkers
        years <- years [type != "Linker"]
        if (nrow (years) > 1) {
          xs <- rollmean (years$x, 2)
          ys <- rollmean (years$y, 2)
          years <- years [2:nrow (years), year]
          text (x = xs,
                y = ys,
                labels = years,
                pos = 3,
                col = 'yellow')
        }
      }
      
      # identify linker and pith markers
      wLinkers <- which (rv$markerTable$type == 'Linker')
      wPith    <- which (rv$markerTable$type == 'Pith')
      
      # draw blue lines which symbolise linked segmemts, that are not
      # check whether there is at least one linker marker
      if (sum (wLinkers, na.rm = TRUE) >= 1) {
        
        # check whether the last marker was a linker or there is another marker afterwards
        if (nrow (rv$markerTable) > max (wLinkers, na.rm = TRUE)) { 
          
          # check whether the following marker is an increment or linker marker
          if (rv$markerTable$type [wLinkers + 1] != 'Linker') {
            segments (x0 = rv$markerTable [wLinkers, x], 
                      y0 = rv$markerTable [wLinkers, y],
                      x1 = rv$markerTable [wLinkers - 1, x], 
                      y1 = rv$markerTable [wLinkers - 1, y], 
                      lwd = 2, 
                      col = 'cornflowerblue')
          }
        # the last marker was a linker
        } else if (nrow (rv$markerTable) == max (wLinkers, na.rm = TRUE)) {
          segments (x0 = rv$markerTable [max (wLinkers, na.rm = TRUE), x], 
                    y0 = rv$markerTable [max (wLinkers, na.rm = TRUE), y],
                    x1 = rv$markerTable [max (wLinkers, na.rm = TRUE) - 1, x], 
                    y1 = rv$markerTable [max (wLinkers, na.rm = TRUE) - 1, y], 
                    lwd = 2, 
                    col = 'cornflowerblue')  
        }
      }
      # above code draws lines between last marker and linker markers for single linker markers
      # and between the two linker markers for consecutive linker markers
      
      # overplot linker markers in blue
      points (x = rv$markerTable [wLinkers, x], 
              y = rv$markerTable [wLinkers, y],
              col = 'cornflowerblue',
              pch = 19,
              cex = 1.2,
              lwd = 2)
      
      # overplot the pith marker in red
      points (x = rv$markerTable [wPith, x], 
              y = rv$markerTable [wPith, y],
              col = '#c90016',
              pch = 19,
              cex = 1.2,
              lwd = 2)
      
      # check whether there are already two points to draw a guide 
      if (nrow (marker_tbl) > 1) { 
        
        # calculate slope and intercept for abline dissecting the last two point (i.e., guide)
        xy <- marker_tbl [(nrow (marker_tbl)-1):nrow (marker_tbl)]
        slope <- diff (xy$y) / diff (xy$x)
        
        # rotate slope of guide by 90 degree after linker points
        if (rv$markerTable [nrow (rv$markerTable), type] == 'Linker') slope <- -1 / slope
        
        # calculate the intercept
        intercept <- xy$y [2] - slope * xy$x [2]
        
        # check whether slope is finite and plot guide
        if (is.finite (slope)) {
          abline (intercept, 
                  slope,
                  col = 'yellow',
                  lwd = 2, 
                  lty = 2)
        } else {
          # plot vertical line between the markers, if slope is not finite
          abline (v = mean (xy$x),
                  col = 'yellow',
                  lwd = 2, 
                  lty = 2)
        }
        
      }
    })
  
  
  observeEvent (input$selRed,
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
                 
                 rv$procband <- 'Brightness'
               }
  )
  
  observeEvent(input$selRGB,
               {
                 printLog('observeEvent input$selRGB')
                 
                 rv$procband <- 'RGB'
               }
  )
  
  totbrightness <- reactive (
    {
      printLog('totbrightness reactive')
      
      tmp <- 
        rv$imgMat[,,1] + 
        rv$imgMat[,,2] + 
        rv$imgMat[,,3]
      
      tmp/3
    }
  )
  
  brightness <- reactive (
    {
      printLog('brightness reactive')
      
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getBrightness(rv$imgMat)
      tmp
    }
  )
  
  darkness <- reactive (
    {
      printLog('darkness reactive')
      
      if(is.null(rv$imgMat)) 
        return()
      tmp <- getDarkness(rv$imgMat)
      tmp
    }
  )
  
  
  contrast <- reactive (
    {
      printLog ('contrast reactive')
      
      if (is.null (rv$imgMat)) 
        return ()
      tmp <- getContrast(rv$imgMat)
      tmp
    }
  )
  
  
  imgProcessed <- reactive (
    {
      printLog ('imgProcessed reactive')
      
      if (is.null (rv$imgMat)) 
        return ()
      
      if (length (dim (rv$imgMat)) == 2)
      {
        showModal (strong (
          modalDialog("The image is monochrome!",
                      easyClose = T,
                      fade = T,
                      size = 's',
                      style = 'background-color:#3b3a35; color:#A41034; ',
                      footer = NULL
          )))
        return (rv$imgMat)
      }
      
      # clhsv <- clRGB2HSV(rv$imgMat)
      
      switch (rv$procband,
             'RGB' = rv$imgMat,
             'Blue' = rv$imgMat[,,3],
             'Brightness' = totbrightness ()
      )
      
    }
  )
  
  observeEvent (input$clearCanvas, 
               {
                 printLog ('observeEvent input$clearCanvas')
                 
                 printLog (paste ('input$clearCanvas was changed to:', '\t',input$clearCanvas))
                 
                 if (rv$notLoaded == TRUE) return()
                 
                 rv$slideShow <- 0 
                 
                 # reset the marker table
                 rv$markerTable <- data.table (no = integer(),
                                               x = numeric(),
                                               y = numeric(),
                                               relx = numeric(),
                                               rely = numeric(),
                                               type = character()
                 )
                 rv$check_table <- rv$check_table + 1
               })
  
  observeEvent (input$linkerPoint, 
               {
                 printLog ('observeEvent input$linkerPoint')
                 
                 if (rv$notLoaded==TRUE) return ()
                 
                 # check whether no marker has been set yet
                 if (nrow (rv$markerTable) == 0)
                 {
                   showModal (strong (
                     modalDialog ("No ring marker is identified yet!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style='background-color:#3b3a35; color:#A41034; ',
                                  footer = NULL
                     )))
                   return ()
                 # check whether only one marker has been set yet 
                 } else if (nrow (rv$markerTable) <= 2) {
                   showModal (strong (
                     modalDialog ("The first two points cannot be linkers! Maybe start on a ring?",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style ='background-color:#3b3a35; color:#A41034; ',
                                  footer = NULL
                     )))
                   return ()
                 # else more than one marker have been set and the type is switched
                 } else {
                   rv$markerTable [no == nrow (rv$markerTable), 
                                 type:=switch (type, 'Linker' = 'Normal', 'Normal' = 'Linker')]
                   rv$check_table <- rv$check_table + 1
                 }
               })
  
  observeEvent (input$pith, 
               {
                 printLog ('observeEvent input$linkerPoint')
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check whether no marker has been set yet
                 if (nrow (rv$markerTable) == 0)
                 {
                   showModal (strong (
                     modalDialog ("No ring marker is identified yet!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style='background-color:#3b3a35; color:#A41034; ',
                                  footer = NULL
                     )))
                   return ()
                 # else at least one marker was set
                 } else {
                   rv$markerTable [no == nrow (rv$markerTable), 
                                   type := switch (type, 'Pith'   = 'Normal', 'Normal' = 'Pith')]
                   rv$check_table <- rv$check_table + 1
                 }
               })
  
  # undo last marker action
  observeEvent (input$undoCanvas, 
               {
                 
                 printLog('observeEvent input$undoCanvas')
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check there is more than one marker
                 if (nrow (rv$markerTable) > 1)
                   # delete last marker
                   rv$markerTable <- rv$markerTable [-nrow (rv$markerTable), ]
                 # else no or one marker was set yet
                 else
                   # create new marker table
                   rv$markerTable <- data.table (no   = integer (),
                                                 x    = numeric (),
                                                 y    = numeric (),
                                                 relx = numeric (),
                                                 rely = numeric (),
                                                 type = character ()
                   )
                 # increase check_table
                 rv$check_table <- rv$check_table + 1
               })
  
  observeEvent (input$ring_point,
               {
                 
                 printLog ('observeEvent input$ring_point')
                 
                 if (rv$notLoaded == TRUE) return()
                 
                 if (input$confirmMeta == 'Not Confirmed') {
                     showModal (strong (
                     modalDialog ("First review and confirm the metadata!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style ='background-color:#3b3a35; color:#A41034; ',
                                  footer = NULL
                     )))
                   return ()
                 }
                 
                 # set point index to 1 for first marker or increase the last marker index by one
                 no <- ifelse (is.null (rv$markerTable), 1, nrow (rv$markerTable) + 1)
                 
                 # initialise new point
                 newPoint <- data.table (no = no,
                                         x = input$ring_point$x,
                                         y = input$ring_point$y,
                                         relx = input$ring_point$x/input$ring_point$domain$right,
                                         rely = input$ring_point$y/input$ring_point$domain$top,
                                         type = 'Normal')
                 
                 # check that new point is different from old point
                 if (nrow (rv$markerTable) > 0){
                   last <- rv$markerTable[nrow(rv$markerTable)]
                   if (newPoint$x == last$x & newPoint$y == last$y) return ()
                 }
                 # add new point to the marker table
                 rv$markerTable <- rbind (rv$markerTable, newPoint)
               })
  
  
  growthTable <- reactive ({
    req (rv$check_table)
    req (rv$markerTable)
    req (input$barkSide)
    
    # copy markerTable into growth_table
    growth_table <- rv$markerTable
    
    # get types of markers
    types <- growth_table$type
    n <- length (types)
    
    # initialise years vector
    years <- rep (NA, n)
    
    # check whether there is no pith marker yet
    if (sum (types == 'Pith', na.rm = TRUE) == 0){

      # check whether the measurement series starts at the bark
      if (input$barkSide) {

        # loop over all points from bark towards the pith
        for (i in 1:n)
          years [i] <- ifelse (i == 1,
                               ifelse (input$sampleYearGrowingSeason == 'only started',
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] == 'Linker',
                                       years [i-1],
                                       years [i-1] - 1)
          )
      # else the measurement series starts at the inner most ring
      } else {
        # loop over all points from inner most ring towards the bark
        for (i in n:1)
          years [i] <- ifelse (i == n,
                               ifelse (input$sampleYearGrowingSeason == 'only started',
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] == 'Linker',
                                       years [i+1],
                                       years [i+1] - 1))
      }
    # else there is a pith marker already
    } else if (sum (types == 'Pith', na.rm = TRUE) == 1) {
      # find the index of pith marker
      p <- which (types == 'Pith')

      # check whether the measurement series starts at the bark
      if (input$barkSide){
        # loop over all points from bark to pith
        for (i in 1:p)
          years[i] <- ifelse (i == 1,
                              ifelse (input$sampleYearGrowingSeason == 'only started',
                                      input$sampleYear + 1,
                                      input$sampleYear),
                              ifelse (types [i] == 'Linker',
                                      years [i-1],
                                      years [i-1] - 1))
        # loop over all point from pith towards the bark in potential second profile
        for (i in (p + 1):n)
          years [i] <- ifelse (i == 1,
                               ifelse (input$sampleYearGrowingSeason == 'only started',
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] == 'Linker',
                                       years [i-1],
                                       years [i-1] + 1))
      # else the measurement series starts at the pith
      } else {
        # loop over points from potential second profile to the pith
        for (i in n:p)
          years [i] <- ifelse (i == n,
                               ifelse (input$sampleYearGrowingSeason == 'only started',
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] == 'Linker',
                                       years [i+1],
                                       years [i+1] - 1))

        # loop over points from pith to bark
        for(i in (p-1):1)
          years [i] <- ifelse (i == n,
                               ifelse (input$sampleYearGrowingSeason == 'only started',
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse(types [i] == 'Linker',
                                      years [i+1],
                                      years [i+1] + 1))
      }
    }
    
    # add years to growth table
    growth_table$year <- years
    
    # check whether at least two markers have been set
    if (nrow (growth_table) <= 1)  return (growth_table)
    
    # calculate distances between markers, aka growth
    # if one linker is set we measure the distance from linker to next marker
    growth <- sqrt (diff (growth_table$x)^2 + diff (growth_table$y)^2)
    if (input$barkSide) {
      growth_table$pixels <- c (0, growth)
    } else {
      growth_table$pixels <- c (growth, 0)
    }
    # if two consecutive linkers are set, we measure from marker to linker and 
    # add the distance from second linker to the next marker, this allows to 
    # more precisely jump gaps in the sample
    if (nrow (growth_table) >= 3) {
      for (i in 3:nrow (growth_table)) {
        # if one of the two preceeding markers is not a linker move on
        if (growth_table$type [i-1] != 'Linker' | growth_table$type [i-2] != 'Linker') next
        growth_table$pixels [i] <- growth_table$pixels [i-2] + growth_table$pixels [i]
      }
    }
    
    # set all linker markers' growth to NA
    #growth_table [type == 'Linker', pixels := NA]  
    
    # convert growth from pixels to mm
    growth_table [, growth := pixels / as.numeric (input$sampleDPI) * 25.4]
    
    # replace NAs with " " to generate empty cells
    growth_table$growth [is.na (growth_table$growth)] <- " "
    
    # return growth_table
    return (growth_table)
  })

  output$growth_table <- DT::renderDataTable (
    {
      printLog ('output$growth_table renderDataTable')
      
      # get growth data
      tbl <- growthTable ()
      
      # return if there is no data
      if (nrow (tbl) == 0) return ()
      
      # check that the table has been updated # TR Not sure why this is needed
      req (rv$check_table)
      
      # order table, aka starting with the most recent year
      tbl <- tbl [order (no)]
      
      # display markers in data table below image
      datatable (tbl [ , 2:dim (tbl) [2]], options = list (initComplete = JS (
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#484e55', 'color': '#fff'});",
        "$('.dataTables_info').css({'color':'#888'});",
        "$('.dataTables_filter').css({'color':'#888'});",
        "$('.dataTables_length').css({'color':'#888'}, );",
        "}")
      )) 
      
    },
    
    options = list (pageLength = 5)
  )
  
  output$downloadCSV <- downloadHandler (
    
    filename = function () {
      
      printLog ('output$downloadCSV downloadHandler filename')
      
      paste0 ('ringdata-', 
              input$ownerName,
              '_',
              input$species,
              '_',
              input$siteLocID,
              '_',
              input$sampleDate, 
              '_',
              rv$wrkID, 
              '_',
              format (Sys.time (),
                     format = '%Y-%m-%d-%H%M%S'),
              ".csv")
      
    },
    content = function (file) {
      
      printLog ('output$downloadCSV downloadHandler content')
      
      if (!rv$notLoaded) {
        writePNG(imgProcessed(), 
                 target = paste0(rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        writePNG(rv$imgMat, 
                 target = paste0(rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
        
        write(toJSON(metaData()), 
              paste0(rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      tbl <- growthTable ()
      
      if (nrow (tbl) == 0) 
        return ()
      
      write.table (tbl, 
                   file, 
                   sep = ',',
                   row.names = F)
      
    }
  )
  
  output$downloadJSON <- downloadHandler(
    
    filename = function () {
      
      printLog ('output$downloadJSON downloadHandler filename')
      
      paste0 ('ringdata-', 
              input$ownerName,
              '_',
              input$species,
              '_',
              input$siteLocID,
              '_',
              input$sampleDate, 
              '_',
              rv$wrkID, 
              '_',
              format (Sys.time(),
                     format = '%Y-%m-%d-%H%M%S'),
              ".json")
      
    },
    
    content = function (file) {
      
      printLog ('output$downloadJSON downloadHandler content')
      
      if (!rv$notLoaded) {
        writePNG (imgProcessed (), 
                  target = paste0 (rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        writePNG (rv$imgMat, 
                  target = paste0 (rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
        
        write (toJSON (metaData()), 
               paste0 (rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      # data <- list(growth_table = growthTable(),
      #              meta_data = metaData())
      
      metaData () %>% 
        toJSON () %>%
        write_lines (file)
    }
  )
  
  observeEvent (input$confirmMeta, {
    
    printLog ('observeEvent input$confirmMeta')
    
    if (input$confirmMeta == 'Not Confirmed') return ()
    
    if (year (input$sampleDate) != input$sampleYear) {
      
      showModal (strong (
        
        modalDialog ("Year does not match the sample's date!",
                     easyClose = T,
                     fade      = T,
                     size      = 's',
                     style     = 'background-color:#3b3a35; color:#fce319; ',
                     footer    = NULL
        )))
      
      updateRadioButtons (session, 
                          inputId  = 'confirmMeta', 
                          selected = 'Not Confirmed')
    }
  })
  
  output$ring_plot <- renderPlotly({
    
    printLog('output$ring_plot renderPlotly')
    
    tbl <- growthTable ()
    
    # check whether there is at least one growth icrement
    if (nrow (tbl) == 0) 
      return ()
    
    
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
      title = ifelse(test = is.na (input$sampleDPI),
                     no = "Radial growth increment (mm)",
                     yes ="Radial growth increment (pixels)"), 
      titlefont = fontList
    )
    
    tbl [, year := as.factor (year)]
    
    data <- tbl [type != 'Linker']
    
    if (is.na (input$sampleDPI)){
      data [,toplot := pixels]
    }
    else
    {
      data[,toplot:=growth]
    }
    
    p <- plot_ly(data = data, 
                 x=~year, 
                 y= ~toplot,
                 type = 'scatter',
                 mode = 'lines',
                 marker=list(color='#e26828')
    ) %>%
      layout(xaxis = xAxis,
             yaxis = yAxis)
    
    p$elementId <- NULL
    
    
    return(p)
    
  }
  
  )
  
  observeEvent (input$rotate180,{
    rv$imgMat <- rotateRGB (rotateRGB (rv$imgMat))
  })
  printLog (finit = TRUE)
}
)


