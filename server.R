#######################################################################
# The server side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

# increase maximal size of images to maxImageSize in MB 
# maxImageSize is initialised in global.R
#----------------------------------------------------------------------------------------
options (shiny.maxRequestSize = maxImageSize * 1024^2)

shinyServer (function (input, output, session)
{
  
  # sent the initial log message
  #--------------------------------------------------------------------------------------
  printLog (init = TRUE)
  
  # declaring reactive value
  #--------------------------------------------------------------------------------------
  rv <- reactiveValues (
    
    imgMat = readJPEG ('no_image_loaded.jpeg')[,,1:3], # RGB matrix loaded based on the image
    notLoaded = TRUE,  # whether the first image is loaded
    demoMode = FALSE,  # whether app is in demo mode 
    procband = 'RGB',  # processed matrix from the raw RGB
    check_table   = 0, # a flag to make sure a label table exists
    index         = 0, # index of the last modified label or the label that should be changed (e.g., insertion)
    previousIndex = 0, # index of the previous label
      
    markerTable = data.table ( # data.table contains the marker data
      no   = integer (),   # no ID
      x    = numeric (),   # x
      y    = numeric (),   # y
      relx = numeric (),   # relative x
      rely = numeric (),   # relative y
      type = character (), # type of marker ('Normal','Linker','Misc','Density fluctuation',
                           #                 'Frost ring','Fire scar',
                           #                 'Early-to-latewood transition' or 'Pith')
      pixels = numeric (),   # "growth" in pixels of the image
      growth = numeric (),   # "growth" in micrometers
      year   = numeric (),   # year of formation
      delete = character (), # column to add "delete" actions button 
      insert = character ()) # column to add "insert" action button
  )
  
  # update the image aspect ratio
  #--------------------------------------------------------------------------------------
  observeEvent (rv$imgMat,
               {
                 # write log
                 printLog ('observeEvent rv$imgMat')
                 
                 # get image dimensions
                 imgDim <- dim (rv$imgMat)
                 
                 # update image aspect
                 rv$imgAsp <- imgDim [2] / imgDim [1]  
               }
  )
  
  # delete specific row in "growth" table
  #--------------------------------------------------------------------------------------
  observeEvent (input$deleteRow,
                {
                  # write log
                  printLog ('observeEvent input$deleteRow')

                  # check that the markerTable and outTable exist
                  req (rv$markerTable)

                  # get the row number that should be deleted
                  rowNum <- parseRowNumber (input$deleteRow)
                  
                  # correct for the fact the the table is displayed from back to front 
                  rowNum <- nrow (rv$markerTable) - rowNum + 1
                  
                  # delete the row
                  rv$markerTable <- rv$markerTable [-rowNum, ]
                  
                  # reduce all marker numbers that were higher than the deleted marker
                  rv$markerTable [no > rowNum, no := no - 1]
                }
  )
  
  # insert row after specific row in "growth" table
  #--------------------------------------------------------------------------------------
  observeEvent (input$insertRow,
                {
                  printLog ('observeEvent input$insertRow')
                  
                  # check that the markerTable and outTable exist
                  req (rv$markerTable)
                  
                  # get the row number (e.g. marker number) after which the new marker should be inserted
                  rowNum <- parseRowNumber (input$insertRow)
                  
                  # correct for the fact the the table is displayed from back to front 
                  rowNum <- nrow (rv$markerTable) - rowNum + 1
                  
                  # share the row number (e.g. marker number)  
                  rv$index <- rowNum
                  
                  # check whether user wants to insert a missing ring using input modal
                  showModal (strong (
                    modalDialog ("Do you want to insert a missing ring or other marker?",
                                 easyClose = T,
                                 fade = T,
                                 size = 'm',
                                 style ='background-color:#3b3a35; color:#b91b9a4; ',
                                 footer = tagList (
                                   actionButton (inputId = 'missingRing',
                                                 label   = 'Missing ring'),
                                   modalButton (label   = 'Other'))))
                  )
                }
  )
  
  # insert a missing ring after specific row in "growth" table
  #--------------------------------------------------------------------------------------
  observeEvent (input$missingRing,
                {
                  # write log
                  printLog ('observeEvent input$missingRing')
                  
                  # check that the markerTable and outTable exist
                  req (rv$markerTable)

                  # initialise missing ring
                  missingRing <- data.table (no   = rv$index+1,
                                             x    = rv$markerTable$x    [rv$index],
                                             y    = rv$markerTable$y    [rv$index],
                                             relx = rv$markerTable$relx [rv$index],
                                             rely = rv$markerTable$rely [rv$index],
                                             type = 'Missing')
                  

                  # increase all marker numbers after the inserted marker
                  rv$markerTable [no > rv$index, no := no + 1]

                  # insert marker after identified row
                  rv$markerTable <- rbind (rv$markerTable [1:rv$index, ],
                                           missingRing,
                                           rv$markerTable [(rv$index+1):nrow (rv$markerTable), ],
                                           fill = TRUE)

                  # reset insert index to the last label in the series
                  rv$index <- nrow (rv$markerTable)
                  
                  # close modal dialog
                  removeModal ()
                }
  )
  
  #--------------------------------------------------------------------------------------
  # whenever new image was uploaded
  observeEvent (input$image,
               {
                 # write log
                 printLog ('observeEvent input$image')
                 
                 # reset radio button, so that metadata needs to be confirmed
                 updateRadioButtons (session = session, 
                                     inputId = 'confirmMeta', 
                                     selected = 'Not Confirmed')
                 
                 # exit demo mode
                 rv$demoMode <- FALSE
                 
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
                 } else if (rv$imgExt %in% c ('tiff', 'tif', 'TIF', 'TIFF')) {
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
                 
                 # get image dimenions
                 imgDim <- dim (rv$imgMat)
                 
                 # write image to the temporary working directory
                 writePNG (rv$imgMat, paste0 (rv$wrkDir,
                                              'imgorg-',
                                              rv$wrkID,
                                              '.png'))
                 
                 # rotate the image matrix by 270 degrees, if image is higher than wide
                 if (imgDim [2] < imgDim [1]) { 
                   rv$imgMat <- rotateRGB (rotateRGB (rotateRGB (rv$imgMat)))
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
                 
                 # reset the index marker
                 rv$index <- 0
               }
  )
  
  # whenever a marker file is uploaded update all the labels and plot them
  #--------------------------------------------------------------------------------------
  observeEvent (input$labelUpload,
                {
                  printLog ('observeEvent input$labelUpload')
                  
                  # get path to path to the marker file
                  rv$labelsPath <- input$labelUpload$datapath
                  
                  # get file extension
                  rv$labelsExt <- file_ext (rv$labelsPath)
                  
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
                  
                  # check whether there are already labels set
                  if (nrow (rv$markerTable) == 0) {
                    
                    # read label file from .csv, or .json file
                    if (rv$labelsExt %in% c ('csv', 'CSV')) {
                      
                      # read csv file
                      labels <- as.data.table (read_csv (file = rv$labelsPath, 
                                                          col_names = TRUE,
                                                          col_types = 'iddddcidd'))
                      
                      # update marker table from csv file
                      rv$markerTable <- labels [, .(no, x, y, relx, rely, type)]
                      
                    # upload marker table from json file, if there is none yet
                    } else if (rv$labelsExt %in% c ('json', 'JSON')) {
                      
                      # read json file
                      labels <- read_json (rv$labelsPath)
                        
                      # update marker table from json file 
                      rv$markerTable <- data.table::rbindlist (labels$markerData, 
                                                               fill = TRUE)
                      
                      # set the index 
                      rv$index <- nrow (rv$markerTable)
                      rv$previousIndex <- nrow (rv$markerTable) 
                        
                      # update metadata fields
                      updateTextInput (session = session,
                                       inputId = 'ownerName',
                                       value = labels$ownerName)
                      updateTextInput (session = session,
                                       inputId = 'ownerEmail',
                                       value = labels$ownerEmail)
                      updateTextInput (session = session,
                                       inputId = 'species',
                                       value = labels$species)
                      updateTextInput (session = session,
                                       inputId = 'sampleDate',
                                       value = labels$sampleDate)
                      updateTextInput (session = session,
                                       inputId = 'sampleYear',
                                       value = labels$sampleYear)
                      updateRadioButtons (session = session,
                                          inputId = 'sampleYearGrowingSeason',
                                          selected = ifelse (labels$sampleYearGrowth == 'none', 
                                                             'not started', 
                                                             ifelse (labels$sampleYearGrowth == 'some', 
                                                                     'only started', 
                                                                     'already ended')))
                      updateTextInput (session = session,
                                       inputId = 'sampleDPI',
                                       value = labels$sampleDPI)
                      updateCheckboxInput (session = session,
                                           inputId = 'pithInImage',
                                           value = unlist (labels$pithInImage))
                      updateCheckboxInput (session = session,
                                           inputId = 'barkFirst',
                                           value = unlist (labels$barkFirst))
                      updateTextInput (session = session,
                                       inputId = 'siteLoc',
                                       value = labels$siteLoc)
                      updateTextInput (session = session,
                                       inputId = 'siteLocID',
                                       value = labels$siteLocID)
                      updateTextInput (session = session,
                                       inputId = 'plotID',
                                       value = labels$plotID)
                      updateTextInput (session = session,
                                       inputId = 'sampleNote',
                                       value = labels$sampleNote)
                      updateTextInput (session = session,
                                       inputId = 'sampleID',
                                       value = labels$sampleID)
                      updateTextInput (session = session,
                                       inputId = 'collection',
                                       value = labels$collection)
                      updateTextInput (session = session,
                                       inputId = 'contributor',
                                       value = labels$contributor)
                      
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
                      modalDialog ("Error: Erase existing labels before uploading new labels!",
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
  
  # whenever metadata is uploaded update all the metadata below and make user review it
  #--------------------------------------------------------------------------------------
  observeEvent (input$metadataUpload,
               {
                 # write log
                 printLog ('observeEvent input$metadataUpload')
                 
                 # get path to metadata
                 rv$metaPath <- input$metadataUpload$datapath
                 
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
                                      value = unlist (metadata$pithInImage))
                 updateCheckboxInput (session = session,
                                      inputId = 'barkFirst',
                                      value = unlist (metadata$barkFirst))
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
  
  # create metaData object that is pulled when metadata is saved
  #--------------------------------------------------------------------------------------
  metaData <- reactive (
    {
      # write log
      #----------------------------------------------------------------------------------
      printLog ('metaData reactive')
      
      # check for demo mode
      #----------------------------------------------------------------------------------
      if (rv$demoMode) {
        showModal (strong (
          modalDialog ("Warning: You are still in demo mode!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
      }
      
      # compile and return metadata
      #----------------------------------------------------------------------------------
      meta <- list (version          = TRIADversion,
                    ownerName        = input$ownerName, 
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
                    markerData       = growthTable (),
                    status           = input$confirmMeta)
      
      # return metadata
      #----------------------------------------------------------------------------------
      return (meta)
    }
  )
  
  # create detrendedData object that is pulled when detrendedData is saved
  #--------------------------------------------------------------------------------------
  detrendedData <- reactive (
    {
      # write log
      #----------------------------------------------------------------------------------
      printLog ('detrendedData reactive')
      
      # check for demo mode
      #----------------------------------------------------------------------------------
      if (rv$demoMode) {
        showModal (strong (
          modalDialog ("Warning: You are still in demo mode!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
      }
      
      # compile and return metadata
      #----------------------------------------------------------------------------------
      meta <- metaData ()
      
      # get detrended data
      #----------------------------------------------------------------------------------
      detrended <- detrendGrowth () 
      detrended [['nSeries']] <- NULL
      detrended [['data']] <- NULL
      
      # add detrended and metadata
      detrended <- c (meta, detrended)
      
      # return detrended data
      #----------------------------------------------------------------------------------
      return (detrended)
      
    }
  )
  
  autoInvalidate <- reactiveTimer (2000)
  
  # render labels on the image
  #--------------------------------------------------------------------------------------
  output$imageProc <- renderPlot (
    width = function () {
      floor (input$zoomlevel)
    },
    height = function () {
      # floor(session$clientData$output_imageProc_width/rv$imgAsp)
      floor (input$zoomlevel / rv$imgAsp)
    },
    {
      # write log
      printLog ('output$imageProc renderPlot')
      
      # check for image and make local copy
      imgtmp <- imgProcessed ()
      
      # check that it actually exists
      if (is.null (imgtmp)) return ()
      
      # get images dimensions
      imgDim <- dim (imgtmp)
      
      # set margins and plot are
      par (mar = c (0,0,0,0), xaxs = 'i', yaxs = 'i')
      plot (NA, 
            xlim = c (1, imgDim [2]),
            ylim = c (1, imgDim [1]),
            type = 'n', 
            axes = FALSE, 
            xlab = '',
            ylab = '')
      
      window <- par()$usr
      
      # plot actual image
      #----------------------------------------------------------------------------------
      rasterImage (imgtmp, 
                   xleft   = window [1], 
                   ybottom = window [3], 
                   xright  = window [2], 
                   ytop    = window [4])
      
      # make local copy of marker table
      #----------------------------------------------------------------------------------
      marker_tbl <- rv$markerTable [, .(x, y)]
      
      # check that there are labels to plot
      if (nrow (marker_tbl) == 0) return ()
      
      # identify normal, linker, misc and pith labels
      wNormal  <- which (rv$markerTable$type == 'Normal')
      wLinkers <- which (rv$markerTable$type == 'Linker')
      wMisc    <- which (rv$markerTable$type %in% c ('Misc',
                                                     'Density fluctuation',
                                                     'Frost ring',
                                                     'Fire scar',
                                                     'Early-to-latewood transition'))
      wPith    <- which (rv$markerTable$type == 'Pith')
      wMissing <- which (rv$markerTable$type == 'Missing')
      
      # plot all normal labels indicate missing rings, if labels should be displayed
      if (input$displayLabels) {
        points (x = marker_tbl [wNormal, x],
                y = marker_tbl [wNormal, y], 
                pch = 19, 
                cex = 1.2, 
                col = colours [['colour']] [colours [['type']] == 'Normal'])
        # make empty darker cirlces around the marker to indicate missing years
        points (x = marker_tbl [wMissing, x],
                y = marker_tbl [wMissing, y], 
                pch = 1, 
                cex = 1.5, 
                lwd = 2,
                col = colours [['colour']] [colours [['type']] == 'Missing'])
      }
      
      # plot marker numbers, if desired
      if (input$displayLabelIDs) {
        text (x = marker_tbl$x,
              y = marker_tbl$y,
              labels = rv$markerTable$no,
              pos = 1,
              col = '#666666')
      }
      
      # plot years between two labels to more easily identify the growth rings
      if (input$displayYears) {
        years <- growthTable ()
        
        # find only normal and pith labels
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
      # above code draws lines between last marker and linker labels for single linker labels
      # and between the two linker labels for consecutive linker labels
      
      # plot linker labels in blue, if labels should be displayed
      if (input$displayLabels) {
        points (x = rv$markerTable [wLinkers, x], 
                y = rv$markerTable [wLinkers, y],
                col = colours [['colour']] [colours [['type']] == 'Linker'],
                pch = 19,
                cex = 1.2,
                lwd = 2)
  
        # plot misc labels in Cambridge blue
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
                cex = ifelse (input$pithInImage, 1.8, 1.2),
                lwd = ifelse (input$pithInImage, 3, 2))
      }
      
      # check whether there are already two points to draw a guide 
      if (nrow (marker_tbl) > 1) { 
        
        # calculate slope and intercept for abline dissecting the last two point (i.e., guide)
        xy <- marker_tbl [(nrow (marker_tbl)-1):nrow (marker_tbl)]
        slope <- diff (xy$y) / diff (xy$x)
        
        # rotate slope of guide by 90 degree after single linker point
        if (rv$markerTable [nrow (rv$markerTable),     type] == 'Linker' &
            rv$markerTable [nrow (rv$markerTable) - 1, type] != 'Linker') {
          slope <- -1 / slope
        }
        
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
          # plot vertical line between the labels, if slope is not finite
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
      # write log
      printLog ('totbrightness reactive')
      
      # sum birghtness of all three colour channels
      tmp <- rv$imgMat [,,1] + rv$imgMat [,,2] + rv$imgMat [,,3]
      
      # average tog et total birghtness
      tmp / 3
    }
  )
  
  brightness <- reactive (
    {
      # write log
      printLog ('brightness reactive')
      
      # check image exists
      if (is.null (rv$imgMat)) return ()
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
      # write log
      printLog ('contrast reactive')
      
      # check that image exists
      if (is.null (rv$imgMat)) return ()
      tmp <- getContrast (rv$imgMat)
      tmp
    }
  )
  
  
  imgProcessed <- reactive (
    {
      # write log
      printLog ('imgProcessed reactive')
      
      # check that image exists
      if (is.null (rv$imgMat)) return ()
      
      # check that the image has three colour bands
      if (length (dim (rv$imgMat)) == 2) {
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
     
      # extract save rbg, blue channel only and total brightness version of the image 
      switch (rv$procband,
              'RGB'  = rv$imgMat,
              'Blue' = rv$imgMat [,,3],
              'Brightness' = totbrightness ())

    }
  )
  
  # erase all previous labels
  #--------------------------------------------------------------------------------------
  observeEvent (input$clearCanvas, 
               {
                 # write log
                 printLog ('observeEvent input$clearCanvas')
                 printLog (paste ('input$clearCanvas was changed to:', '\t',input$clearCanvas))
                 
                 # check that an image was loaded
                 if (rv$notLoaded == TRUE) return ()
                 
                 rv$slideShow <- 0 
                 
                 # reset the marker table
                 rv$markerTable <- data.table (no = integer (),
                                               x  = numeric (),
                                               y  = numeric (),
                                               relx = numeric (),
                                               rely = numeric (),
                                               type = character ())
                 
                 # reset indices for insertion and last set marker
                 rv$index  <- 0
                 
                 # make sure to update table
                 rv$check_table <- rv$check_table + 1
               })
  
  # swtich marker type of previsouly set marker from "Normal" to "Linker"
  #--------------------------------------------------------------------------------------
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
                 # check whether only one or two labels have been set yet 
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
                   # else more than two normal labels have been set and 
                   # the type of the last indexed marker is switched
                 } else {
                   i <- ifelse (rv$previousIndex < rv$index - 1, rv$previousIndex, rv$index) 
                   rv$markerTable [no == i, # TR this seems to crash occassionally with the following warning: Warning: Error in [.data.table: When deleting columns, i should not be provided 
                                   type := switch (type, 
                                                   'Linker' = 'Normal', 
                                                   'Normal' = 'Linker')]
                   
                   # validate that a marker table exists
                   rv$check_table <- rv$check_table + 1
                 }
               })
  
  # change type of previously set marker from "Normal" to "Pith"
  #--------------------------------------------------------------------------------------
  observeEvent (input$pith, 
               {
                 # write log
                 printLog ('observeEvent input$pith')
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check whether no label has been set yet
                 if (nrow (rv$markerTable) == 0) {
                   showModal (strong (
                     modalDialog ("Error: No ring marker is identified yet!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL)))
                   return ()
                 # check whether there is already a pith label
                 } else if (sum (rv$markerTable$type == 'Pith', na.rm = TRUE) > 0) {
                   showModal (strong (
                     modalDialog ("Error: You can only set one pith!",
                                  easyClose = T,
                                  fade = T,
                                  size = 's',
                                  style = 'background-color:#3b3a35; color:#eb99a9; ',
                                  footer = NULL)))
                   return ()
                 # else we have at least one label and no pith yet
                 } else {
                   # change the label type of the last indexed label
                   i <- ifelse (rv$previousIndex < rv$index - 1, rv$previousIndex, rv$index)
                   rv$markerTable [no == i, 
                                   type := switch (type, 
                                                   'Pith' = 'Normal', 
                                                   'Normal' = 'Pith')]
                   
                   # validate that a marker table exists
                   rv$check_table <- rv$check_table + 1
                 }
               })
  
  # undo last marker 
  #--------------------------------------------------------------------------------------
  observeEvent (input$undoCanvas, 
               {
                 
                 printLog ('observeEvent input$undoCanvas')
                 
                 if (rv$notLoaded == TRUE) return ()
                 
                 # check there is more than one marker
                 if (nrow (rv$markerTable) > 1) {
                   
                   # delete the previously modified marker or the last marker
                   rv$markerTable <- rv$markerTable [-rv$index, ]
                   # N.B. Currently there is only memory of one index and thereafter 
                   #      points will be deleted from the end of the markerTable  
                   
                   # reset index to index of last label
                   rv$index <- nrow (rv$markerTable)
                     
                   # make sure that the no are consecutive, after marker was deleted
                   rv$markerTable$no <- 1:nrow (rv$markerTable)
                   
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
  
  # add a "Normal" marker by simple click
  #--------------------------------------------------------------------------------------
  observeEvent (input$normal_point,
               {
                 
                 # write log
                 printLog ('observeEvent input$normal_point')
                 
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
                 newPoint <- data.table (no = rv$index + 1,
                                         x  = input$normal_point$x,
                                         y  = input$normal_point$y,
                                         relx = input$normal_point$x / input$normal_point$domain$right,
                                         rely = input$normal_point$y / input$normal_point$domain$top,
                                         type = 'Normal',
                                         growth = NA,
                                         pixels = NA,
                                         year   = NA)
                 
                 # check that new point is different from old point
                 if (nrow (rv$markerTable) > 0){
                   last <- rv$markerTable [nrow (rv$markerTable)]
                   if (newPoint$x == last$x & newPoint$y == last$y) return ()
                 }
                 
                 # insert or append new point to the label table
                 if (rv$index < nrow (rv$markerTable)) {
                   
                   # increase all label numbers with higher no than the inserted label
                   rv$markerTable [no >= rv$index+1, no := no + 1]
                   
                   # insert label after identified row 
                   rv$markerTable <- rbind (rv$markerTable [1:rv$index, ], 
                                            newPoint,
                                            rv$markerTable [(rv$index+1):nrow (rv$markerTable), ],
                                            fill = TRUE)
                 } else {
                   rv$markerTable <- rbind (rv$markerTable, newPoint, fill = TRUE)
                 }
                 
                 # save index and reset to index of last label
                 rv$previousIndex <- rv$index
                 rv$index <- nrow (rv$markerTable)
                 
                 # validate that a marker table exists
                 rv$check_table <- rv$check_table + 1
               })
  
  # Change type of previously set "Misc" label
  #--------------------------------------------------------------------------------------
  observeEvent (input$selectMiscType,
                {
                  # write log
                  printLog ('observeEvent input$selectMiscType')
                  
                  # change type of the misc label that was just set
                  rv$markerTable [no == rv$index, type := input$miscType]
                  
                  # close the modal
                  removeModal ()
                  
                })
  
  # add a "Misc" label by double click
  #--------------------------------------------------------------------------------------
  observeEvent (input$misc_point,
                {
                  
                  # write log
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
                  
                  # check what kind of special feature is labeled
                  showModal (strong (
                    modalDialog ("What kind of special feature are you labelling?",
                                 easyClose = TRUE,
                                 fade = TRUE,
                                 size = 'm',
                                 style ='background-color:#3b3a35; color:#b91b9a4; ',
                                 footer = tagList (
                                   radioButtons (inputId = 'miscType',
                                                 label   = '',
                                                 choices = c ('Misc',
                                                              'Density fluctuation',
                                                              'Frost ring',
                                                              'Fire scar',
                                                              'Early-to-latewood transition'),
                                                 selected = NULL,
                                                 inline = TRUE)),
                                 actionButton (inputId = 'selectMiscType',
                                               label = 'Select'),
                                 modalButton ('Cancel')
                                 ))
                  )
                  
                  # initialise new point
                  newPoint <- data.table (no = rv$index + 1,
                                          x  = input$misc_point$x,
                                          y  = input$misc_point$y,
                                          relx = input$misc_point$x / input$misc_point$domain$right,
                                          rely = input$misc_point$y / input$misc_point$domain$top,
                                          type = 'Misc')
                                          
                  # check that new point is different from old point
                  if (nrow (rv$markerTable) > 0) {
                    last <- rv$markerTable [nrow (rv$markerTable)]
                    if (newPoint$x == last$x & newPoint$y == last$y) return ()
                  }
                  
                  # insert new point to the marker table
                  if (rv$index < nrow (rv$markerTable)) {
                    
                    # increase all marker number with higher no than the inserted marker
                    rv$markerTable [no > rv$index, no := no + 1]
                    
                    # insert marker after identified row 
                    rv$markerTable <- rbind (rv$markerTable [1:rv$index, ], 
                                             newPoint,
                                             rv$markerTable [(rv$index+1):nrow (rv$markerTable), ],
                                             fill = TRUE)
                    
                  # or append new point in the end
                  } else {
                    rv$markerTable <- rbind (rv$markerTable, newPoint, fill = TRUE)
                  }
                  
                  # save index and set to index of last label
                  rv$previousIndex <- rv$index
                  rv$index <- nrow (rv$markerTable)
                  
                  # validate that marker table exists
                  rv$check_table <- rv$check_table + 1
                })
  
  # calculate the growth when necessary (i.e. to display or for download)
  #--------------------------------------------------------------------------------------
  growthTable <- reactive ({
    
    # write log
    #----------------------------------------------------------------------------------
    printLog ('reactive$growthTable')
    
    # check for requirements
    #------------------------------------------------------------------------------------
    req (rv$check_table)
    req (rv$markerTable)
    
    # copy rv$markerTable into growth_table, if it has at least one row
    #------------------------------------------------------------------------------------
    if (nrow (rv$markerTable) > 0) {
      growth_table <- rv$markerTable
    } else {
      return ()
    }
    
    # get types of labels
    #------------------------------------------------------------------------------------
    types <- growth_table [, type]
    n <- length (types)
    
    # initialise years vector, which associates the starting boundary of the growth ring 
    # with the year of the growing season  
    #------------------------------------------------------------------------------------
    years <- rep (NA, n)
   
    # measurement series starts at the bark
    #------------------------------------------------------------------------------------
    if (input$barkFirst) {
      
      # is there a pith or oldest ring label? If so, find its index p
      #------------------------------------------------------------------------------------
      if (sum (types == 'Pith', na.rm = TRUE) == 1) {
        p <- which (types == 'Pith') 
      } else {
        p <- n
      }
      
      # loop over all points from bark to pith
      for (i in 1:p) {
        if (i == 1 & input$sampleYearGrowingSeason %in% c ('only started', 
                                                           'already ended')) {
          years [i] <- input$sampleYear + 1
        } else if (i == 1 & input$sampleYearGrowingSeason == 'not started') {
          years [i] <- input$sampleYear
        } else {
          
          # find the last 'Normal' or 'Missing' label index j
          j <- max (which (types [1:(i-1)] %in% c ('Normal','Missing')))
          
          # for normal or misc label, substract one year
          if (types [i] %in% c ('Normal','Missing','Misc','Density fluctuation',
                                'Frost ring','Fire scar','Early-to-latewood transition')) {
            years [i] <- years [j] - 1
          } else if (types [i] == "Linker") {
            years [i] <- years [j]
          }
        }
      }
      
      # and beyond the pith/oldest ring if there are additional labels
      if (n > p) {
        
        # loop over all point from pith towards the bark in potential second profile
        for (i in (p + 1):n) {
          
          # find the last normal label index j
          j <- max (which (types [1:(i-1)] == 'Normal'))
          
          # for normal or misc label, add one year
          if (types [i] %in% c ('Normal','Missing')) {
            years [i] <- years [j] + 1
            # for misc labels use the same year
          } else if (types [i] %in% c ('Linker','Misc','Density fluctuation','Frost ring',
                                       'Fire scar','Early-to-latewood transition')) {
            years [i] <- years [j]
          }
        }
      }
      
    # else the measurement series starts at the oldest ring or pith
    } else if (!input$barkFirst) {
    
      # is there a pith or oldest ring label? If so, find its index p
      #------------------------------------------------------------------------------------
      if (sum (types == 'Pith', na.rm = TRUE) == 1) {
        p <- which (types == 'Pith') 
        
        # check that the first label is the oldest ring or pith
        if (p != 1) {
          showModal (strong (
            modalDialog ("Error: You start your measurement at the pith, but your first label is not the pith. Something is not correct!",
                         easyClose = T,
                         fade = T,
                         size = 's',
                         style = 'background-color:#3b3a35; color:#f3bd48; ',
                         footer = NULL)))
          return ()
        }
    
        # loop over all points from inner most ring towards the bark in reverse order
        for (i in n:1) {
          if (i == n & input$sampleYearGrowingSeason %in% c ('only started', 
                                                             'already ended')) {
            years [i] <- input$sampleYear + 1
          } else if (i == n & input$sampleYearGrowingSeason == 'not started') {
            years [i] <- input$sampleYear
          } else {
            
            # find the last normal label index j
            j <- min (which (types [(i+1):n] == 'Normal'))
            
            # for normal or misc label, substract one year
            if (types [i] %in% c ('Normal','Missing','Misc','Density fluctuation',
                                  'Frost ring','Fire scar','Early-to-latewood transition')) {
              years [i] <- years [j] - 1
            } else if (types [i] == 'Linker') {
              years [i] <- years [i]
            }
          }
        }
      }
    } # Nota bene: "Linker" labels are not associated with any particular year
  
    # add years to growth table
    growth_table$year <- years
    
    # with only one label, no meaningful growth can be calculated 
    if (nrow (growth_table) == 1)  return (growth_table)
    
    # intialise growth in pixels
    pixels <- rep (NA, n)
    
    # identify reference label to which "growth" will be calculated
    # first label has to be normal and "growth" will be set to NA, loop over rest
    for (i in 2:n) { 
      
      # jump to next iteration for "Missing" and "Linker" labels 
      if (growth_table$type [i] %in% c ('Missing', 'Linker')) next
      
      # identify growth year of the label
      iYr <- growth_table$year [i]
      
      # identify reference label's years, which is next year for "Normal" labls
      if (growth_table$type [i] == 'Normal') {
          refYr <- iYr + 1
      # and the same year for "Misc" labels 
      } else if (growth_table$type [i] %in% c ('Misc','Density fluctuation',
                                               'Frost ring','Fire scar',
                                               'Early-to-latewood transition')) {
        refYr <- iYr
      }
      
      # identify the reference label index
      iRef <- which (growth_table$year == refYr & growth_table$type == 'Normal')
      if (length (iRef) == 0) next
      
      # identify the number of Linker labels between label and reference label
      nLinkers <- sum (growth_table$year == refYr & 
                       growth_table$type == 'Linker' & 
                       growth_table$no < i, na.rm = TRUE)
       
      # calculate distance if there is no Linker label in between
      if (nLinkers == 0) {
        pixels [i] <- sqrt ((growth_table$x [i] - growth_table$x [iRef])^2 + 
                            (growth_table$y [i] - growth_table$y [iRef])^2)
        #print (c (i, iRef, iYr, refYr, nLinkers))
      } else if (nLinkers == 1) {
        # identify Linker label's index
        iLinker <- which (growth_table$year == refYr & 
                          growth_table$type == 'Linker' & 
                          growth_table$no < i)
        
        pixels [i] <- sqrt ((growth_table$x [i] - growth_table$x [iLinker])^2 + 
                            (growth_table$y [i] - growth_table$y [iLinker])^2)
        #print (c (i, iRef, iYr, refYr, nLinkers, iLinker))
      } else if (nLinkers == 2) {
        # identify Linker label's index
        iLinker1 <- min (which (growth_table$year == refYr & 
                                growth_table$type == 'Linker' & 
                                growth_table$no < i))
        iLinker2 <- max (which (growth_table$year == refYr & 
                                growth_table$type == 'Linker' & 
                                growth_table$no < i))
        
        pixels [i] <- (sqrt ((growth_table$x [iRef] - growth_table$x [iLinker1])^2 + 
                             (growth_table$y [iRef] - growth_table$y [iLinker1])^2)) +
          (sqrt ((growth_table$x [iLinker2] - growth_table$x [i])^2 + 
                 (growth_table$y [iLinker2] - growth_table$y [i])^2))
        #print (c (i, iRef, iYr, refYr, nLinkers, iLinker1, iLinker2))
      }
    }

    # add growth in pixels to the growth_table 
    #------------------------------------------------------------------------------------
    growth_table$pixels <- pixels
    
    # convert growth from pixels (using dots per inch input resolution) to micrometers
    #------------------------------------------------------------------------------------
    growth_table [, growth := pixels / as.numeric (input$sampleDPI) * 25.4 * 1000]
    
    # return growth_table
    #------------------------------------------------------------------------------------
    return (growth_table)
  })
  
  # compute detrended growth using the dplR functions
  #--------------------------------------------------------------------------------------
  detrendGrowth <- reactive ({
    
    # write log
    #------------------------------------------------------------------------------------
    printLog ('reactive$detrendGrowth ')
    
    # get growth table 
    #------------------------------------------------------------------------------------
    tbl <- growthTable ()
    
    # deselect Linker and Misc labels
    #------------------------------------------------------------------------------------
    tbl <- tbl [type %in% c ('Normal','Pith')]
    
    # check whether there are two radial series
    #------------------------------------------------------------------------------------
    if ('Pith' %in% tbl [['type']]) {
      
      # find pith label's index
      wPith <- which (tbl [['type']] == 'Pith')
      
      # is there only one profile 
      if (wPith == nrow (tbl)) {
        data1 <- tbl
        nSeries <- 1
        
      # if there are two series split them at the pith
      } else {
        data2 <- tbl [(index+1):nrow (tbl), ]
        data1 <- tbl [1:index-1]
        nSeries <- 2
      }
    }
    
    # check whether data is in pixels or microns
    #------------------------------------------------------------------------------------
    if (is.na (sampleDPI)){
      data1 [, toplot := pixels]
      if (nSeries == 2) data2 [, toplot := pixels]
    } else {
      data1 [, toplot := growth]
      if (nSeries == 2) data2 [, toplot := growth]
    }
    
    # remove the first label, which does not have any "growth"
    #------------------------------------------------------------------------------------
    data1 <- data1 [-1, ]
    
    # convert table to dlpR format, which reads rwl files
    #------------------------------------------------------------------------------------
    if (nSeries == 2) {
      data <- right_join (x = data1 [, .(year, toplot)], 
                          y = data2 [, .(year, toplot)], 
                          by = 'year', 
                          suffix = c ('.1','.2'))
      
      # compute the mean of the two series
      #------------------------------------------------------------------------------------
      data [['toplot']] <- rowMeans (data [, c ('toplot.1', 'toplot.2')], na.rm = TRUE)
    } else {
      data <- data1 [, .(year, toplot)]
    }
    
    # determine whether residuals are obtained by division (as ratio) or substraction
    #------------------------------------------------------------------------------------
    detrendingDifference <- ifelse (input$detrendingDifference == 'Division', TRUE, FALSE) 
    
    # use mean ring width for detrending
    #------------------------------------------------------------------------------------
    if (input$detrendingMethod == 'Mean') {
      detrended <- detrend.series (y      = data [['toplot']], 
                                   y.name = 'toplot', 
                                   method = 'Mean', 
                                   difference  = detrendingDifference,
                                   make.plot   = FALSE,
                                   return.info = TRUE)
    # use spline for detrending
    #------------------------------------------------------------------------------------
    } else if (input$detrendingMethod == 'Spline') {
      detrended <- detrend.series (y      = data [['toplot']], 
                                   y.name = 'toplot', 
                                   method = 'Spline', 
                                   nyrs   = input$detrendingWavelength,
                                   f      = input$detrendingFrequencyResponse,
                                   difference  = detrendingDifference,
                                   make.plot   = FALSE,
                                   return.info = TRUE)
      
    # use modified negative exponential for detrending
    #------------------------------------------------------------------------------------
    } else if (input$detrendingMethod == 'Modified negative exponential') {
      detrended <- detrend.series (y      = data [['toplot']], 
                                   y.name = 'toplot', 
                                   method = 'ModNegExp', 
                                   pos.slope = input$detrendingPosSlope,
                                   constrain.nls = input$dedetrendingConstrainNLS,
                                   difference  = detrendingDifference,
                                   make.plot   = FALSE,
                                   return.info = TRUE)
      
    # use Ar model for detrending
    #------------------------------------------------------------------------------------
    } else if (input$detrendingMethod == 'Prewhitening') {
      detrended <- detrend.series (y      = data [['toplot']], 
                                   y.name = 'toplot', 
                                   method = 'Ar', 
                                   difference  = detrendingDifference,
                                   make.plot   = FALSE,
                                   return.info = TRUE)
      
    # use Friedman's Super Smoother for detrending
    #------------------------------------------------------------------------------------
    } else if (input$detrendingMethod == 'Friedman') {
      detrended <- detrend.series (y      = data [['toplot']], 
                                   y.name = 'toplot', 
                                   method = 'Friedman', 
                                   #wt = input$detrendingWeights, # TR Notta bene: We only allow defaults for now.
                                   #span = input$detrendingSpan, # TR Nota bene: We only allow defaults for now.
                                   bass = input$detrendingBASS,
                                   difference  = detrendingDifference,
                                   make.plot   = FALSE,
                                   return.info = TRUE)
      
    # use a modified Hugershoff for detrending
    #------------------------------------------------------------------------------------
    } else if (input$detrendingMethod == 'Modified Hugershoff') {
      detrended <- detrend.series (y      = data [['toplot']], 
                                   y.name = 'toplot', 
                                   method = 'ModHugershoff', 
                                   pos.slope = input$detrendingPosSlope,
                                   constrain.nls = input$dedetrendingConstrainNLS,
                                   difference  = detrendingDifference,
                                   make.plot   = FALSE,
                                   return.info = TRUE) 
    }
    
    # add data and number of series
    #------------------------------------------------------------------------------------
    detrended [['data']]    <- data
    detrended [['nSeries']] <- nSeries
    
    # return the detrended data 
    #------------------------------------------------------------------------------------
    return (detrended)
  })
  
  # render data table with label numbers, coordinates and calculated "growth" 
  #--------------------------------------------------------------------------------------
  output$growth_table <- DT::renderDataTable ({
    
      # write log
      #----------------------------------------------------------------------------------
      printLog ('output$growth_table renderDataTable')
      
      # make local copy of label and growth data, unless there is no data
      #----------------------------------------------------------------------------------
      if (nrow (rv$markerTable) > 0) {
        labelTable <- growthTable ()
      } else {
        return ()
      }
      
      # order table, aka starting with the most recent year
      #----------------------------------------------------------------------------------
      labelTable <- labelTable [order (no)]
      
      # add a delete button and display the formatted datatable
      #----------------------------------------------------------------------------------
      displayDataTable (labelTable, 
                        id1 = 'delete',
                        id2 = 'insert') 
      
    }
  )
  
  # download a csv file with the label locations and growth
  #--------------------------------------------------------------------------------------
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

      # write log
      #----------------------------------------------------------------------------------
      printLog ('output$downloadCSV downloadHandler content')
      
      # check for demo mode
      #----------------------------------------------------------------------------------
      if (rv$demoMode) {
        showModal (strong (
          modalDialog ("Warning: You are still in demo mode! Downloads not possible!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
      }
      
      # check that image is loaded
      #----------------------------------------------------------------------------------
      if (!rv$notLoaded) {
        writePNG (imgProcessed (), 
                  target = paste0 (rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        writePNG (rv$imgMat, 
                  target = paste0 (rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))
        
        write (toJSON (metaData ()), 
               paste0 (rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      # calculate "growth"
      tbl <- growthTable ()
      
      # check that there are some labels
      if (nrow (tbl) == 0) return ()
      
      # write csv file
      write.table (tbl, 
                   file, 
                   sep = ',',
                   row.names = F)
      
    }
  )
  
  # download a json file with the metadata and marker locations and growth
  #--------------------------------------------------------------------------------------
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
      
      # write log
      printLog ('output$downloadJSON downloadHandler content')
      
      # check for demo mode
      #----------------------------------------------------------------------------------
      if (rv$demoMode) {
        showModal (strong (
          modalDialog ("Warning: You are still in demo mode! Downloads not possible!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
      }
      
      # check that an image was loaded 
      if (!rv$notLoaded ) {
        
        # save processed image
        writePNG (imgProcessed (), 
                  target = paste0 (rv$wrkDir, 'imgprc-', rv$wrkID,'.png'))
        
        # save raw the image
        writePNG (rv$imgMat, 
                  target = paste0 (rv$wrkDir, 'imgraw-', rv$wrkID,'.png'))

        # write metadata json file
        write (toJSON (metaData ()), 
               paste0 (rv$wrkDir, 'meta-', rv$wrkID,'.json'))
        
      }
      
      metaData () %>% 
        toJSON () %>%
        write_lines (file)
    }
  )
  
  # confirm the metadata
  #--------------------------------------------------------------------------------------
  observeEvent (input$confirmMeta, {
    
    # write log
    printLog ('observeEvent input$confirmMeta')
    
    # check that metadata was confirmed
    if (input$confirmMeta == 'Not Confirmed') return ()
    
    # check that the sample year and year of sample date match
    if (year (input$sampleDate) != input$sampleYear) {
      
      showModal (strong (
        
        modalDialog ("Warning: Year does not match the sample's date!",
                     easyClose = T,
                     fade      = T,
                     size      = 's',
                     style     = 'background-color:#3b3a35; color:#f3bd48; ',
                     footer    = NULL
        )))
      
      # update radio button
      updateRadioButtons (session, 
                          inputId  = 'confirmMeta', 
                          selected = 'Not Confirmed')
    }
  })
  
  # draw plot of absolute growth
  #--------------------------------------------------------------------------------------
  output$growth_plot <- renderPlotly ({
    
    # write log
    #------------------------------------------------------------------------------------
    printLog ('output$growth_plot renderPlotly')
    
    # select font
    #------------------------------------------------------------------------------------
    fontList <- list (
      family = "helvetica",
      size = 16,
      color = "#7f7f7f"
    )
    
    # specify x-axis
    #------------------------------------------------------------------------------------
    xAxis <- list (
      title = "Year",
      titlefont = fontList
    )
    
    # specify y-axis
    #------------------------------------------------------------------------------------
    yAxis <- list (
      title = ifelse (test = is.na (input$sampleDPI),
                      no = "Radial growth increment (micrometers)",
                      yes ="Radial growth increment (pixels)"), 
      titlefont = fontList
    )
    
    # initiate margins
    #------------------------------------------------------------------------------------
    m <- list (
      l = 100,
      r = 50,
      b = 100,
      t = 50,
      pad = 4
    )
    
    # get detrended series
    #------------------------------------------------------------------------------------
    detrended <- detrendGrowth ()
    
    # plot absolute growth data to object p
    #------------------------------------------------------------------------------------
    p <- plot_ly (data = detrended [['data']], 
                  x = ~year, 
                  y = ~toplot,
                  name = ifelse (detrended [['nSeries']] [[1]] == 2, 'Mean series', 'Series'),
                  type = 'scatter',
                  mode = 'lines+markers',
                  marker = list (color = 'cornflowerblue', symbol = 'circle-dot')) %>%
      layout (xaxis  = xAxis,
              yaxis  = yAxis,
              margin = m)
    
    # add the two potential series
    #------------------------------------------------------------------------------------
    if (detrended [['nSeries']] == 2) {
      p <- p %>% add_trace (data = detrended [['data']],
                            x = ~year, 
                            y = ~toplot.1,
                            name = 'Series A',
                            mode = 'lines+markers',
                            marker = list (color = 'grey', opacity = 0.6),
                            line   = list (color = 'grey', opacity = 0.2, width = 0.7))
      
      p <- p %>% add_trace (data = detrended [['data']],
                            x = ~year, 
                            y = ~toplot.2,
                            name = 'Series B',
                            mode = 'lines+markers',
                            marker = list (color = 'grey', opacity = 0.6),
                            line   = list (color = 'grey', opacity = 0.2, width = 0.7))
    }
    
    # add detrending curve to plot
    #------------------------------------------------------------------------------------
    p <- p %>% add_trace (y = ~detrended [['curves']], 
                          name = input$detrendingMethod,
                          mode = 'lines+markers', 
                          line = list (color = '#901c3b'),
                          marker = list (color ='transparent'))
    
    p$elementId <- NULL
    
    # return the plot object
    #------------------------------------------------------------------------------------
    return (p)
    
  })

  # draw plot of detrended growth
  #--------------------------------------------------------------------------------------
  output$detrended_growth_plot <- renderPlotly ({
    
    # write log
    #------------------------------------------------------------------------------------
    printLog ('output$detrended_growth_plot renderPlotly')
    
    # check whether there is at least one growth icrement
    #------------------------------------------------------------------------------------
    if (nrow (tbl) == 0) return ()
    
    fontList <- list (
      family = "Helvetica",
      size = 16,
      color = "#7f7f7f"
    )
    
    xAxis <- list (
      title = "Year",
      titlefont = fontList
    )
    
    yAxisD <- list (
      title = 'Ring width index', 
      titlefont = fontList
    )
    
    # get detrended series
    #------------------------------------------------------------------------------------
    detrended <- detrendGrowth ()
    
    # extract rwi indices, detrending curve, and years
    #------------------------------------------------------------------------------------
    rwi   <- detrended$series
    years <- detrended$data [['year']]
    
    # second plot with ring width indices
    #------------------------------------------------------------------------------------
    d <- plot_ly (x = ~years, 
                  y = ~rwi,
                  name = 'RWI of mean series',
                  type = 'scatter',
                  mode = 'lines+markers',
                  marker = list (color = '#af95a3', symbol = 'circle-dot'),
                  line = list (color = '#af95a3')) %>%
      layout (xaxis  = xAxis,
              yaxis  = yAxisD,
              margin = m)
    
    d$elementId <- NULL
    
    # return plot
    #------------------------------------------------------------------------------------
    return (d)
    
  })
  
  # download metadata template
  #--------------------------------------------------------------------------------------
  output$downloadTemplate <- downloadHandler (
    
    filename = function () {
    
      # write log
      #----------------------------------------------------------------------------------
      printLog ('output$downloadTemplate downloadHandler filename')
      
      # paste file name together
      #----------------------------------------------------------------------------------
      paste0 ('TRIAD_metadata_template',
              format (Sys.time (),
                      format = '%Y-%m-%d-%H%M%S'),
              '.xlsx')
    },
    content = function (file) {
      
      # write log
      #----------------------------------------------------------------------------------
      printLog ('output$downloadTemplate downloadHandler content')
      
      # check for demo mode
      #----------------------------------------------------------------------------------
      if (rv$demoMode) {
        showModal (strong (
          modalDialog ("Warning: You are still in demo mode! Downloads not possible!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
      }
      
      # download existing metadata template file
      #----------------------------------------------------------------------------------
      file.copy ('TRIAD-metadata-template-2020-09-14.xlsx', file)
    }
  )
  
  # rotate image 180 degree
  #--------------------------------------------------------------------------------------
  observeEvent (input$rotate180,{
    rv$imgMat <- rotateRGB (rotateRGB (rv$imgMat))
  })
  
  # update oldest ring/pith action button label depending on whether the pith is in the image or not
  #--------------------------------------------------------------------------------------
  observeEvent (input$pithInImage, {
    
    # write log
    #------------------------------------------------------------------------------------
    printLog ('input$pithInImage changed button')
    
    # update the label on the pith/oldest ring action button
    #------------------------------------------------------------------------------------
    updateActionButton (session = session, 
                        inputId = 'pith',
                        label = ifelse (input$pithInImage, 'Pith','Oldest ring'))

    # return
    #------------------------------------------------------------------------------------
    return ()
  })
  
  # update max value of wavelength for spline
  #--------------------------------------------------------------------------------------
  observeEvent (input$detrendingMethod, {

    # write log
    #------------------------------------------------------------------------------------
    printLog ('input$detrendingMethod changed wavelength sliderInput')

    # get number of normal and pith labels
    #------------------------------------------------------------------------------------
    n <- nrow (rv$markerTable [type %in% c ('Normal','Pith')])
    
    # update the label on the pith/oldest ring action button
    #------------------------------------------------------------------------------------
    updateSliderInput (session = session,
                       inputId = 'detrendingWavelength',
                       max = n,
                       value = 0.67 * n)

    # return
    #------------------------------------------------------------------------------------
    return ()
  })
  

  # update oldest ring/pith action button label depending on whether the pith is in the image or not
  #--------------------------------------------------------------------------------------
  observeEvent (input$demoMode, {
    
    # write log
    #------------------------------------------------------------------------------------
    printLog ('input$demoImage switch demo mode on/off')
    
    # check whether demoMode was off
    #------------------------------------------------------------------------------------
    if (!rv$demoMode) {
    
      rv$demoMode <- TRUE
      
      # get path to image
      #----------------------------------------------------------------------------------
      rv$imgPath <- 'demoImage.jpg'
      
      # get file extension
      #----------------------------------------------------------------------------------
      rv$imgExt <- file_ext (rv$imgPath)
      
      # read image
      #----------------------------------------------------------------------------------
      rv$imgMat <- readJPEG (rv$imgPath)
      
      # set demo mode and not loaded to TRUE
      #----------------------------------------------------------------------------------
      rv$demoMode  <- TRUE
      
      # show message to alert user for demo mode
      #----------------------------------------------------------------------------------
      showModal (strong (
        modalDialog (HTML ("Warning: <br>
                           You are now entering demo mode!"),
                     easyClose = T,
                     fade = T,
                     size = 's',
                     style = 'background-color:#3b3a35; color:#f3bd48; ',
                     footer = NULL)))
      
    # else we are leaving demo mode
    } else if (rv$demoMode) {
      
      # turn demo mode off
      rv$demoMode <- FALSE
      
      # show message to alert user for end of demo mode
      #----------------------------------------------------------------------------------
      showModal (strong (
        modalDialog (HTML ("Warning: <br>
                           You are now leaving demo mode!"),
                     easyClose = T,
                     fade = T,
                     style = 'background-color:#3b3a35; color:#f3bd48; ',
                     footer = NULL)))
    }    
    
    # return
    #------------------------------------------------------------------------------------
    return ()
  })
  
  # create a demo button that changes colour
  #------------------------------------------------------------------------------------
  output$demoButton <- renderUI (
    {
      # demo mode was switched ON and we are now leaving
      if (rv$demoMode) {
        actionButton (inputId = 'demoMode', 
                      label = 'Demo ON',
                      icon = icon ('image'), 
                      class = 'btn-primary', 
                      width = '100%', 
                      style = 'font-weight: bold;
                      color: #91b9a4;')
        
      # demo mode was switched OFF and we are now entering
      } else if (!rv$demoMode) {
        actionButton (inputId = 'demoMode', 
                      label = 'Demo OFF',
                      icon = icon ('image'), 
                      class = 'btn-primary', 
                      width = '100%', 
                      style = 'font-weight: bold;
                      color: #eb99a9;')
      }
    })
  
  # download a json file with the metadata and marker locations and growth
  #--------------------------------------------------------------------------------------
  output$downloadRWI_JSON <- downloadHandler (
    
    filename = function () {
      
      printLog ('output$downloadRWI_JSON downloadHandler filename')
      
      paste0 ('detrendeddata-', 
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
      
      # write log
      printLog ('output$downloadRWI_JSON downloadHandler content')
      
      # check for demo mode
      #----------------------------------------------------------------------------------
      if (rv$demoMode) {
        showModal (strong (
          modalDialog ("Warning: You are still in demo mode! Downloads not possible!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
        
      # check that an image was loaded 
      } else if (rv$notLoaded) { 
        showModal (strong (
          modalDialog ("Warning: You have to start by loading an image!",
                       easyClose = T,
                       fade = T,
                       size = 's',
                       style = 'background-color:#3b3a35; color:#f3bd48; ',
                       footer = NULL)))
        return ()
      }
      
      # write metadata, label data and rwi series 
      detrendedData () %>%
        toJSON () %>%
        write_lines (file)
    }
  )
  
  
  printLog (finit = TRUE)
}
)


