#######################################################################
# The server side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

# increase maximal size of images to 170 MB
options (shiny.maxRequestSize = 170 * 1024^2)

shinyServer (function (input, output, session)
{
  
  # sent the initial log message
  printLog (init = TRUE)
  
  
  # declaring reactive value
  rv <- reactiveValues (
    
    imgMat = readJPEG ('no_image_loaded.jpeg')[,,1:3], #RGB matrix loaded based on the image
    notLoaded = TRUE, # whether the first image is loaded
    procband = 'RGB', # processed matrix from the raw RGB
    check_table = 0, # a flag to make sure a marker table exists
    
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
                 printLog ('observeEvent rv$imgMat')
                 
                 imgDim <- dim (rv$imgMat)
                 rv$imgAsp <- imgDim [2] / imgDim [1]  
               }
  )
  
  #=================================================================
  # whenever new image was uploaded
  observeEvent (input$image,
               {
                 printLog ('observeEvent input$image')
                 
                 # reset radio button, so that metadata needs to be confirmed
                 updateRadioButtons (session = session, 
                                     inputId = 'confirmMeta', 
                                     selected = 'Not Confirmed')
                 
                 # generate working directory id
                 rv$wrkID <- paste (gsub (x = as.character (Sys.time()), 
                                          pattern = ' |:', 
                                          replacement = '-'),
                                   paste (sample (x = c (0:9, letters, LETTERS),
                                                  size = 32,
                                                  replace = TRUE),
                                          collapse = ""), 
                                   sep = '_')
                 
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
                     modalDialog ("Error: Only JPEG, TIFF or PNG files are accepted!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL
                     )))
                   
                   return ()
                 }
                 
                 # change notLoaded boolean now that image is loaded
                 rv$notLoaded <- FALSE
                 
                 # get image directory
                 imgDim <- dim (rv$imgMat)
                 
                 # write image to the temporary working directory
                 writePNG (rv$imgMat, paste0 (rv$wrkDir,
                                              'imgorg-',
                                              rv$wrkID,
                                              '.png'))
                 
                 # get the image matrix
                 if (imgDim [2] < imgDim [1]) {
                   rv$imgMat <- rotateRGB (rv$imgMat)
                 }
                 
                 # reset image resolution to make sure that it is checked by user
                 updateNumericInput (session = session,
                                     inputId = 'sampleDPI',
                                     value = NULL)
                 
                 # reset the markerTable
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
  
  # whenever a marker file is uploaded update all the markers and plots them
  observeEvent (input$markers,
                {
                  printLog ('observeEvent input$markers')
                  
                  # get path to path to the marker file
                  rv$markersPath <- input$markers$datapath
                  
                  # get file extension
                  rv$markersExt <- file_ext (rv$markersPath)
                  
                  # check whether an image is loaded
                  if (rv$notLoaded) {
                    showModal (strong (
                      modalDialog ("Error: Am image must be loaded first!",
                                   easyClose = T,
                                   fade = T,
                                   size = 's',
                                   style = 'background-color:#3b3a35; color:#eb99a9; ',
                                   footer = NULL)))
                    return ()
                  }
                  
                  # check whether there are already markers set
                  if (nrow (rv$markerTable) == 0) {
                    
                    # read markers file from .csv, or .json file
                    if (rv$markersExt %in% c ('csv', 'CSV')) {
                      
                      # read csv file
                      markers <- as.data.table (read_csv (file = rv$markersPath, 
                                                          col_names = TRUE,
                                                          col_types = 'iddddcidd'))
                      
                      # update marker table from csv file
                      rv$markerTable <- markers [, .(no, x, y, relx, rely, type)]
                      
                    # upload marker table from json file, if there is none yet
                    } else if (rv$markersExt %in% c ('json', 'JSON')) {
                      
                      # read json file
                      markers <- read_json (rv$markersPath)
                        
                      # update marker table from json file 
                      rv$markerTable <- data.table::rbindlist (markers$markerData)
                      
                      # update metadata fields
                      updateTextInput (session = session,
                                       inputId = 'ownerName',
                                       value = markers$ownerName)
                      updateTextInput (session = session,
                                       inputId = 'ownerEmail',
                                       value = markers$ownerEmail)
                      updateTextInput (session = session,
                                       inputId = 'species',
                                       value = markers$species)
                      updateTextInput (session = session,
                                       inputId = 'sampleDate',
                                       value = markers$sampleDate)
                      updateTextInput (session = session,
                                       inputId = 'sampleYear',
                                       value = markers$sampleYear)
                      updateRadioButtons (session = session,
                                          inputId = 'sampleYearGrowingSeason',
                                          selected = ifelse (markers$sampleYearGrowth == 'none', 
                                                             'not started', 
                                                             ifelse (markers$sampleYearGrowth == 'some', 
                                                                     'only started', 
                                                                     'already ended')))
                      updateTextInput (session = session,
                                       inputId = 'sampleDPI',
                                       value = markers$sampleDPI)
                      updateCheckboxInput (session = session,
                                           inputId = 'pithInImage',
                                           value = markers$pithInImage)
                      updateCheckboxInput (session = session,
                                           inputId = 'barkFirst',
                                           value = markers$barkFirst)
                      updateTextInput (session = session,
                                       inputId = 'siteLoc',
                                       value = markers$siteLoc)
                      updateTextInput (session = session,
                                       inputId = 'siteLocID',
                                       value = markers$siteLocID)
                      updateTextInput (session = session,
                                       inputId = 'plotID',
                                       value = markers$plotID)
                      updateTextInput (session = session,
                                       inputId = 'sampleNote',
                                       value = markers$sampleNote)
                      updateTextInput (session = session,
                                       inputId = 'sampleID',
                                       value = markers$sampleID)
                      updateTextInput (session = session,
                                       inputId = 'collection',
                                       value = markers$collection)
                      updateTextInput (session = session,
                                       inputId = 'contributor',
                                       value = markers$contributor)
                      
                      # make sure the metadata is reviewed
                      updateRadioButtons (session = session, 
                                          inputId = 'confirmMeta', 
                                          selected = 'Not Confirmed')
                      
                      # Prompt metadata review
                      showModal (strong (
                        modalDialog ("Review and confirm metadata below.",
                                     easyClose = T,
                                     fade = T,
                                     size = 's',
                                     style = 'background-color:#3b3a35; color:#b91b9a4; ',
                                     footer = NULL)))
              
                    } else {
                      showModal (strong (
                        modalDialog ("Error: Only csv or json files are accepted for marker files!",
                                     easyClose = T,
                                     fade = T,
                                     size = 's',
                                     style = 'background-color:#3b3a35; color:#eb99a9; ',
                                     footer = NULL)))
                      return ()
                    }
                  } else {
                    showModal (strong (
                      modalDialog ("Error: Erase existing markers before uploading new markers!",
                                   easyClose = T,
                                   fade = T,
                                   size = 's',
                                   style = 'background-color:#3b3a35; color:#eb99a9; ',
                                   footer = NULL)))
                  }
                  
                  # return
                  return ()
                }
  )
  
  # whenever metadata is uploaded update all the metadata below and make user review it.
  observeEvent (input$metadata,
               {
                 printLog ('observeEvent input$metadata')

                 # get path to metadata
                 rv$metaPath <- input$metadata$datapath

                 # get file extension
                 rv$metaExt <- file_ext (rv$metaPath)

                 # read metadata from .xlsx, .csv, or .json file
                 if (rv$metaExt %in% c ('xlsx', 'XLSX')) {
                   metadata <- read_excel (path = rv$metaPath,
                                           col_names = c ('ownerName','ownerEmail','species',
                                                          'sampleDate','sampleYear',
                                                          'sampleYearGrowth','sampleDPI',
                                                          'pithInImage','barkFirst','siteLoc',
                                                          'siteLocID','plotID','sampleID',
                                                          'sampleNote','collection','contributor'),
                                           col_types = c ('text','text','text','date','numeric',
                                                          'text','numeric','logical','logical',
                                                          'text','text','text','text','text',
                                                          'text','text'), 
                                           skip = 1)
                 } else if (rv$metaExt %in% c ('csv', 'CSV')) {
                   metadata <- read_csv (file = rv$metaPath, 
                                         col_names = c ('ownerName','ownerEmail','species',
                                                        'sampleDate','sampleYear',
                                                        'sampleYearGrowth','sampleDPI',
                                                        'pithInImage','barkFirst','siteLoc',
                                                        'siteLocID','plotID','sampleID',
                                                        'sampleNote','collection','contributor'),
                                         col_types = 'cccDicillccccccc', skip = 1)
                 } else if (rv$metaExt %in% c ('json', 'JSON')) {
                   metadata <- read_json (rv$metaPath)
                 } else {
                   showModal (strong (
                     modalDialog ("Error: Only xlsx, csv or json files are accepted for metadata.",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL)))

                   return ()
                 }
                 
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
                 updateRadioButtons (session  = session,
                                     inputId  = 'sampleYearGrowingSeason',
                                     selected = ifelse (metadata$sampleYearGrowth == 'none', 
                                                        'not started', 
                                                        ifelse (metadata$sampleYearGrowth == 'some', 
                                                                'only started', 
                                                                'already ended')))
                 updateTextInput (session = session,
                                  inputId = 'sampleDPI',
                                  value = metadata$sampleDPI)
                 updateCheckboxInput (session = session,
                                      inputId = 'pithInImage',
                                      value = metadata$pithInImage)
                 updateCheckboxInput (session = session,
                                      inputId = 'barkFirst',
                                      value = metadata$barkFirst)
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
                 
                 # Prompt metadata review
                 showModal (strong (
                   modalDialog ("Review and confirm metadata below.",
                                easyClose = T,
                                fade = T,
                                size = 's',
                                style = 'background-color:#3b3a35; color:#b91b9a4; ',
                                footer = NULL)))
               }
  )
  
  metaData <- reactive (
    {
      printLog ('metaData reactive')
      
      meta <- list (ownerName        = input$ownerName, 
                    ownerEmail       = input$ownerEmail,
                    species          = input$species,
                    sampleDate       = input$sampleDate,
                    sampleYear       = input$sampleYear,
                    sampleYearGrowth = ifelse (input$sampleYearGrowingSeason         == 'not started', 'none',
                                               ifelse (input$sampleYearGrowingSeason == 'already ended', 
                                                       'all', 'some')),
                    sampleDPI        = input$sampleDPI,
                    pithInImage      = input$pithInImage,
                    barkFirst        = input$barkFirst,
                    siteLoc          = input$siteLoc,
                    siteLocID        = input$siteLocID,
                    plotID           = input$plotID,
                    sampleNote       = input$sampleNote,
                    sampleID         = input$sampleID,
                    collection       = input$collection,
                    contributor      = input$contributor,
                    markerData       = rv$markerTable,
                    growth           = growthTable (),
                    status           = input$confirmMeta)
    }
  )
  
  autoInvalidate <- reactiveTimer (2000)
  
  output$imageProc <- renderPlot (
    width = function () {
      floor (input$zoomlevel)
    },
    height = function () {
      # floor(session$clientData$output_imageProc_width/rv$imgAsp)
      floor (input$zoomlevel / rv$imgAsp)
    },
    {
      printLog ('output$imageProc renderPlot')
      
      imgtmp <- imgProcessed ()
      if (is.null (imgtmp)) return ()
      
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
            ylab = '')
      
      window <- par()$usr
      
      rasterImage (imgtmp, 
                   window [1], 
                   window [3], 
                   window [2], 
                   window [4])
      
      marker_tbl <- rv$markerTable [, .(x, y)]
      
      # identify normal, linker, misc and pith markers
      wNormal  <- which (rv$markerTable$type == 'Normal')
      wLinkers <- which (rv$markerTable$type == 'Linker')
      wMisc    <- which (rv$markerTable$type == 'Misc')
      wPith    <- which (rv$markerTable$type == 'Pith')
      
      
      # plot all normal markers, if markers should be displayed
      if (input$displayMarkers) {
        points (x = marker_tbl [wNormal, x],
                y = marker_tbl [wNormal, y], 
                pch = 19, 
                cex = 1.2, 
                col = colours [['colour']] [colours [['type']] == 'Normal'])
      }
      
      # plot marker numbers, if desired
      if (input$displayMarkerIDs) {
        text (x = marker_tbl$x,
              y = marker_tbl$y,
              labels = rv$markerTable$no,
              pos = 1,
              col = '#666666')
      }
      
      # plot years between two markers to more easily identify the growth rings
      if (input$displayYears) {
        years <- growthTable ()
        
        # find only normal and pith markers
        years <- years [years$type %in% c ('Normal', 'Pith')]
        if (nrow (years) > 1) {
          xs <- rollmean (years$x, 2)
          ys <- rollmean (years$y, 2)
          years <- years [2:nrow (years), year]
          text (x = xs,
                y = ys,
                labels = years,
                pos = 3,
                col = colours [['colour']] [colours [['type']] == 'Normal'])
        }
      }
      
      # draw blue lines which symbolise linked segmemts, that are not
      # check whether there is at least one linker marker
      if (sum (wLinkers, na.rm = TRUE) >= 1) {
        
        # check whether the last marker was a linker
        if (nrow (rv$markerTable) > max (wLinkers, na.rm = TRUE)) { 
          
          # loop over all linkers
          for (i in 1:length (wLinkers)) {
            
            # check whether the following marker is an increment or linker marker
            if (rv$markerTable$type [wLinkers [i] + 1] != 'Linker') {
              segments (x0 = rv$markerTable [wLinkers [i], x], 
                        y0 = rv$markerTable [wLinkers [i], y],
                        x1 = rv$markerTable [wLinkers [i] - 1, x], 
                        y1 = rv$markerTable [wLinkers [i] - 1, y], 
                        lwd = 2, 
                        col = colours [['colour']] [colours [['type']] == 'Linker'])
            }
          }
        # the last marker was a linker
        } else if (nrow (rv$markerTable) == max (wLinkers, na.rm = TRUE) ) {
          segments (x0 = rv$markerTable [max (wLinkers, na.rm = TRUE), x], 
                    y0 = rv$markerTable [max (wLinkers, na.rm = TRUE), y],
                    x1 = rv$markerTable [max (wLinkers, na.rm = TRUE) - 1, x], 
                    y1 = rv$markerTable [max (wLinkers, na.rm = TRUE) - 1, y], 
                    lwd = 2, 
                    col = colours [['colour']] [colours [['type']] == 'Linker'])  
        }
      }
      # above code draws lines between last marker and linker markers for single linker markers
      # and between the two linker markers for consecutive linker markers
      
      # plot linker markers in blue, if markers should be displayed
      if (input$displayMarkers) {
        points (x = rv$markerTable [wLinkers, x], 
                y = rv$markerTable [wLinkers, y],
                col = colours [['colour']] [colours [['type']] == 'Linker'],
                pch = 19,
                cex = 1.2,
                lwd = 2)
  
        # plot misc markers in Cambridge blue
        points (x = rv$markerTable [wMisc, x],
                y = rv$markerTable [wMisc, y],
                col = colours [['colour']] [colours [['type']] == 'Misc'],
                pch = 19,
                cex = 1.2, 
                lwd = 2)
        
        # plot the pith marker in crimson as a round point when oldest ring and larger cross 
        # when the actual pith
        points (x = rv$markerTable [wPith, x], 
                y = rv$markerTable [wPith, y],
                col = colours [['colour']] [colours [['type']] == 'Pith'],
                pch = ifelse (input$pithInImage, 4, 19),
                cex = ifelse (input$pithInImage, 2, 1.2),
                lwd = 2)
      }
      
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
                  col = colours [['colour']] [colours [['type']] == 'Normal'],
                  lwd = 2, 
                  lty = 2)
        } else {
          # plot vertical line between the markers, if slope is not finite
          abline (v = mean (xy$x),
                  col = colours [['colour']] [colours [['type']] == 'Normal'],
                  lwd = 2, 
                  lty = 2)
        }
        
      }
    })
  
  
  observeEvent (input$selRed,
               {
                 printLog ('observeEvent input$selRed')
                 
                 rv$procband <- 'Red'
               }
  )
  
  
  observeEvent (input$selBlue,
               {
                 printLog ('observeEvent input$selBlue')
                 
                 rv$procband <- 'Blue'
               }
  )
  
  
  observeEvent (input$selGreen,
               {
                 printLog ('observeEvent input$selGreen')
                 
                 rv$procband <- 'Green'
               }
  )
  
  observeEvent (input$selHue,
               {
                 printLog ('observeEvent input$selHue')
                 
                 rv$procband <- 'Hue'
               }
  )
  
  
  observeEvent (input$selSat,
               {
                 printLog ('observeEvent input$selSat')
                 
                 rv$procband <- 'Saturation'
               }
  )
  
  observeEvent (input$selValue,
               {
                 printLog ('observeEvent input$selValue')
                 
                 rv$procband <- 'Value'
               }
  )
  
  observeEvent (input$selBright,
               {
                 printLog ('observeEvent input$selBright')
                 
                 rv$procband <- 'Brightness'
               }
  )
  
  observeEvent (input$selDark,
               {
                 printLog ('observeEvent input$selDark')
                 
                 rv$procband <- 'Darkness'
               }
  )
  
  observeEvent (input$selContrast,
               {
                 printLog ('observeEvent input$selContrast')
                 
                 rv$procband <- 'Contrast'
               }
  )
  
  observeEvent (input$selTotBr,
               {
                 printLog ('observeEvent input$selTotBr')
                 
                 rv$procband <- 'Brightness'
               }
  )
  
  observeEvent (input$selRGB,
               {
                 printLog ('observeEvent input$selRGB')
                 
                 rv$procband <- 'RGB'
               }
  )
  
  totbrightness <- reactive (
    {
      printLog ('totbrightness reactive')
      
      tmp <- 
        rv$imgMat [,,1] + 
        rv$imgMat [,,2] + 
        rv$imgMat [,,3]
      
      tmp / 3
    }
  )
  
  brightness <- reactive (
    {
      printLog ('brightness reactive')
      
      if (is.null (rv$imgMat)) 
        return ()
      tmp <- getBrightness (rv$imgMat)
      tmp
    }
  )
  
  darkness <- reactive (
    {
      printLog ('darkness reactive')
      
      if (is.null (rv$imgMat)) 
        return ()
      tmp <- getDarkness (rv$imgMat)
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
          modalDialog ("Warning: The image is monochrome!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
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
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 rv$slideShow <- 0 
                 
                 # reset the marker table
                 rv$markerTable <- data.table (no = integer (),
                                               x = numeric (),
                                               y = numeric (),
                                               relx = numeric (),
                                               rely = numeric (),
                                               type = character ())
                 
                 # make sure to update table
                 rv$check_table <- rv$check_table + 1
               })
  
  observeEvent (input$linkerPoint, 
               {
                 printLog ('observeEvent input$linkerPoint')
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check whether no marker has been set yet
                 if (nrow (rv$markerTable) == 0) {
                   showModal (strong (
                     modalDialog ("Error: No ring marker is identified yet!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style='background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL
                     )))
                   return ()
                 # check whether only one or two markers have been set yet 
                 } else if (nrow (rv$markerTable) <= 2) {
                   showModal (strong (
                     modalDialog ("Error: The first two points cannot be linkers! Maybe start on a ring?",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style ='background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL
                     )))
                   return ()
                 # check whether this is the third linker marker in a row 
                 } else if (sum (tail (rv$markerTable$type, n = 2) == 'Linker', na.rm = TRUE) == 2) {
                   showModal (strong (
                     modalDialog ("Error: You can set a maximum of two consecutive linkers!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style ='background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL
                     )))
                   return ()
                   # else more than two normal markers have been set and the type of this marker is switched
                 } else {
                   rv$markerTable [no == nrow (rv$markerTable), 
                                   type := switch (type, 'Linker' = 'Normal', 'Normal' = 'Linker')]
                   
                   # validate that a marker table exists
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
                     modalDialog ("Error: No ring marker is identified yet!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL
                     )))
                   return ()
                 # else at least one marker was set
                 } else {
                   rv$markerTable [no == nrow (rv$markerTable), 
                                   type := switch (type, 'Pith' = 'Normal', 'Normal' = 'Pith')]
                   
                   # validate that a marker table exists
                   rv$check_table <- rv$check_table + 1
                 }
               })
  
  # undo last marker action
  observeEvent (input$undoCanvas, 
               {
                 
                 printLog('observeEvent input$undoCanvas')
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check there is more than one marker
                 if (nrow (rv$markerTable) > 1) {
                   # delete last marker
                   rv$markerTable <- rv$markerTable [-nrow (rv$markerTable), ]
                 # else no or one marker was set yet
                 } else {
                   # create new marker table
                   rv$markerTable <- data.table (no   = integer (),
                                                 x    = numeric (),
                                                 y    = numeric (),
                                                 relx = numeric (),
                                                 rely = numeric (),
                                                 type = character ())
                 }
                 
                 #  validate that a marker table exists
                 rv$check_table <- rv$check_table + 1
               })
  
  observeEvent (input$ring_point,
               {
                 
                 printLog ('observeEvent input$ring_point')
                 
                 # check that images is loaded
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check that metadata has been confirmed
                 if (input$confirmMeta == 'Not Confirmed') {
                     showModal (strong (
                     modalDialog ("First review and confirm the metadata!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style ='background-color:#3b3a35; color:#b91b9a4; ',
                                  footer = NULL)))
                   
                   return ()
                 }
                 
                 # set point index to 1 for first marker or increase the last marker index by one
                 no <- ifelse (is.null (rv$markerTable), 1, nrow (rv$markerTable) + 1)
                 
                 # initialise new point
                 newPoint <- data.table (no = no,
                                         x = input$ring_point$x,
                                         y = input$ring_point$y,
                                         relx = input$ring_point$x / input$ring_point$domain$right,
                                         rely = input$ring_point$y / input$ring_point$domain$top,
                                         type = 'Normal')
                 
                 # check that new point is different from old point
                 if (nrow (rv$markerTable) > 0){
                   last <- rv$markerTable [nrow (rv$markerTable)]
                   if (newPoint$x == last$x & newPoint$y == last$y) return ()
                 }
                 
                 # add new point to the marker table
                 rv$markerTable <- rbind (rv$markerTable, newPoint)
                 
                 # validate that a marker table exists
                 rv$check_table <- rv$check_table + 1
               })
  
  observeEvent (input$misc_point,
                {
                  
                  printLog ('observeEvent input$misc')
                  
                  if (rv$notLoaded == TRUE) return ()
                  
                  if (input$confirmMeta == 'Not Confirmed') {
                    showModal (strong (
                      modalDialog ("First review and confirm the metadata!",
                                   easyClose = T,
                                   fade = T,
                                   size = 's',
                                   style ='background-color:#3b3a35; color:#b91b9a4; ',
                                   footer = NULL)))
                    return ()
                  }
                  
                  # increase point index or throw error if this is the first point
                  if (nrow (rv$markerTable) >= 1) {
                    no <- nrow (rv$markerTable) + 1
                  } else if (nrow (rv$markerTable) < 1) {
                    showModal (strong (
                      modalDialog ("Error: The first marker cannot be a misc marker!",
                                   easyClose = T,
                                   fade = T,
                                   size = 's',
                                   style ='background-color:#3b3a35; color:#b91b9a4; ',
                                   footer = NULL)))
                    return ()
                  }
                  
                  # initialise new point
                  newPoint <- data.table (no = no,
                                          x = input$misc_point$x,
                                          y = input$misc_point$y,
                                          relx = input$misc_point$x / input$misc_point$domain$right,
                                          rely = input$misc_point$y / input$misc_point$domain$top,
                                          type = 'Misc')
                  
                  # check that new point is different from old point
                  if (nrow (rv$markerTable) > 0) {
                    last <- rv$markerTable [nrow (rv$markerTable)]
                    if (newPoint$x == last$x & newPoint$y == last$y) return ()
                  }
                  
                  # add new point to the marker table
                  rv$markerTable <- rbind (rv$markerTable, newPoint)
                  
                  # validate that marker table exists
                  rv$check_table <- rv$check_table + 1
                })
  
  growthTable <- reactive ({
    req (rv$check_table)
    req (rv$markerTable)
    
    # copy markerTable into growth_table
    growth_table <- rv$markerTable
    
    # get types of markers
    types <- growth_table [, type]
    n <- length (types)
    
    # initialise years vector
    years <- rep (NA, n)
    
    # check whether there is no pith marker (which marks oldest ring or pith) yet
    if (sum (types == 'Pith', na.rm = TRUE) == 0) {

      # check whether the measurement series starts at the bark
      if (input$barkFirst) {

        # loop over all points from bark towards the pith
        for (i in 1:n)
          years [i] <- ifelse (i == 1,
                               ifelse (input$sampleYearGrowingSeason %in% 
                                         c ('only started', 'already ended'),
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] %in% c ('Linker', 'Misc'),
                                       years [i-1],
                                       years [i-1] - 1)
          )
      # else the measurement series starts at the inner most ring
      } else if (!input$barkFirst) {
        
        # loop over all points from inner most ring towards the bark in reverse order
        for (i in n:1)
          years [i] <- ifelse (i == n,
                               ifelse (input$sampleYearGrowingSeason %in% 
                                         c ('only started', 'already ended'),
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] %in% c ('Linker', 'Misc'),
                                       years [i+1],
                                       years [i+1] - 1))
      }
    # else there is a pith marker already
    } else if (sum (types == 'Pith', na.rm = TRUE) == 1) {
      
      # find the index of pith marker
      p <- which (types == 'Pith')

      # check whether the measurement series starts at the bark
      if (input$barkFirst){
        
        # loop over all points from bark to pith
        for (i in 1:p) {
          years [i] <- ifelse (i == 1,
                               ifelse (input$sampleYearGrowingSeason %in% 
                                         c ('only started', 'already ended'),
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] %in% c ('Linker', 'Misc'),
                                       years [i-1],
                                       years [i-1] - 1))
        }
        
        # only if the last point was not the oldest ring (i.e., pith marker) 
        if (n > p) {
          # loop over all point from pith towards the bark in potential second profile
          for (i in (p + 1):n) {
            years [i] <- ifelse (i == 1,
                                 ifelse (input$sampleYearGrowingSeason %in% 
                                           c ('only started', 'already ended'),
                                         input$sampleYear + 1,
                                         input$sampleYear),
                                 ifelse (types [i] %in% c ('Linker', 'Misc'),
                                         ifelse (types [i-1] != 'Pith', 
                                                 years [i-1], 
                                                 years [i-1] - 1),
                                         years [i-1] + 1))
          }
        }
      # else the measurement series starts at the pith
      } else if (!input$barkFirst) {
        
        # loop over points from potential second profile to the pith
        for (i in n:p) {
          years [i] <- ifelse (i == n,
                               ifelse (input$sampleYearGrowingSeason %in% 
                                         c ('only started', 'already ended'),
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i]  %in% c ('Linker', 'Misc'),
                                       years [i+1],
                                       years [i+1] - 1))
        }

        # loop over points from pith to bark
        for (i in (p-1):1) {
          years [i] <- ifelse (i == n,
                               ifelse (input$sampleYearGrowingSeason %in% 
                                         c ('only started', 'already ended'),
                                       input$sampleYear + 1,
                                       input$sampleYear),
                               ifelse (types [i] %in% c ('Linker', 'Misc'),
                                       years [i+1],
                                       years [i+1] + 1))
        }
      }
    }
    
    # add years to growth table
    growth_table$year <- years
    
    # check whether at least two markers have been set
    if (nrow (growth_table) <= 1)  return (growth_table)
    
    # intialise growth
    growth <- rep (NA, n)
    
    # calculate "growth" for the various markers and combination 
    # first marker has to be normal and "growth" will be set to 0 for the first marker
    # loop over remaining markers to figure out their "growth"
    for (i in n:2) { 
      
      # identify the index for the last and penultimate linker marker, if they exists
      if (sum (growth_table$type == 'Linker', na.rm = TRUE) >= 1) {
        
        # check whether there is still a previously set linker
        if (min (which (growth_table$type == 'Linker')) < i) {
          lastLinker <- max (which (growth_table [no < i, type] == 'Linker')) 
        } else {
          lastLinker <- 0
        }
        
        # check whether at least two smaller linkers were set
        if (sum (growth_table [no < i, type] == 'Linker', na.rm = TRUE) >= 2) {
          penultimateLinker <- Rfast::nth (which (growth_table [no < i, type] == 'Linker'), 
                                                2, descending = TRUE)
        } else {
          penultimateLinker <- 0
        }
      } else {
        lastLinker <- 0
        penultimateLinker <- 0
      }
      
      # identify the index for the last normal marker
      lastPoint  <- max (which (growth_table [no < i, type] == 'Normal'))
      
      # marker is a normal or misc marker (i.e., "growth" is distance to previous reference marker)
      # exception: two previous reference markers are linker markers
      if (growth_table$type [i] %in% c ('Normal','Misc')) {
        
        # last reference marker was a normal marker
        if (lastPoint > lastLinker) {
          growth [i] <- sqrt ((growth_table$x [i] - growth_table$x [lastPoint])^2 + 
                              (growth_table$y [i] - growth_table$y [lastPoint])^2)
          
        # last reference point was a linker marker
        } else if (lastPoint < lastLinker) {

          # check whether the penultimate reference marker was a normal marker
          if (lastPoint > penultimateLinker) { # TR There is an issue here when we have only one linker marker
            growth [i] <- sqrt ((growth_table$x [i] - growth_table$x [lastLinker])^2 + 
                                (growth_table$y [i] - growth_table$y [lastLinker])^2)
            
          # or a linker, hence there were two consecutive linker markers
          # in this case we measure from last normal marker to penultimate linker and 
          # add the distance from last linker to the current marker 
          # (i.e., jumping the gap between two linker markers)
          } else if (lastPoint < penultimateLinker) {
            growth [i] <- (sqrt ((growth_table$x [lastPoint] - 
                                  growth_table$x [penultimateLinker])^2 + 
                                 (growth_table$y [lastPoint] - 
                                  growth_table$y [penultimateLinker])^2)) +
                          (sqrt ((growth_table$x [lastLinker] - growth_table$x [i])^2 + 
                                 (growth_table$y [lastLinker] - growth_table$y [i])^2))
          }

        # marker is a linker (i.e. no "growth" is calculated)
        } else if (growth_table$type [i] == 'Linker') {
          growth [i] <- NA
        }
      }
    }

    # add growth in pixels to the growth_table 
    growth_table$pixels <- growth
    
    # convert growth from pixels (using dots per inch input resolution) to mm
    growth_table [, growth := pixels / as.numeric (input$sampleDPI) * 25.4]
    
    # replace NAs with " " to generate empty cells
    #growth_table$growth [is.na (growth_table$growth)] <- " "
    
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
      
      # check that the table has been updated
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
      
    }
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
        writePNG (imgProcessed (), 
                  target = paste0 (rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        writePNG (rv$imgMat, 
                  target = paste0 (rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
        
        write (toJSON (metaData ()), 
               paste0 (rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      tbl <- growthTable ()
      
      # if there are no markers yet
      if (nrow (tbl) == 0) return ()
      
      # write csv file
      write.table (tbl, 
                   file, 
                   sep = ',',
                   row.names = F)
      
    }
  )
  
  output$downloadJSON <- downloadHandler (
    
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
        
        write (toJSON (metaData ()), 
               paste0 (rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
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
        
        modalDialog ("Warning: Year does not match the sample's date!",
                     easyClose = T,
                     fade      = T,
                     size      = 's',
                     style     = 'background-color:#3b3a35; color:#f3bd48; ',
                     footer    = NULL
        )))
      
      updateRadioButtons (session, 
                          inputId  = 'confirmMeta', 
                          selected = 'Not Confirmed')
    }
  })
  
  output$ring_plot <- renderPlotly({
    
    printLog ('output$ring_plot renderPlotly')
    
    tbl <- datatable (growthTable ())
    
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
    
    yAxis <- list (
      title = ifelse (test = is.na (input$sampleDPI),
                      no = "Radial growth increment (mm)",
                      yes ="Radial growth increment (pixels)"), 
      titlefont = fontList
    )
    
    tbl [, year := as.factor (year)]
    
    data <- tbl [type != 'Linker']
    
    if (is.na (input$sampleDPI)){
      data [, toplot := pixels]
    } else {
      data [, toplot := growth]
    }
    
    p <- plot_ly (data = data, 
                  x = ~year, 
                  y = ~toplot,
                  type = 'scatter',
                  mode = 'lines',
                  marker = list (color = '#e26828')
    ) %>%
      layout (xaxis = xAxis,
              yaxis = yAxis)
    
    p$elementId <- NULL
    
    return (p)
    
  }
  )
  
  observeEvent (input$rotate180,{
    rv$imgMat <- rotateRGB (rotateRGB (rv$imgMat))
  })
  printLog (finit = TRUE)
}
)


