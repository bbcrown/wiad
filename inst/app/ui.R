#######################################################################
# The UI side for the WIAD shiny app. 
# 
# The Core Development Team: Bijan Seyednasrollah, Tim Rademacher and David Basler.
#
# WIAD is the Wood Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/wiad
#######################################################################

library (wiad)
library (data.table)
library (DT)
library (plotly)
library (shiny)
library (shinyjs)
library (shinythemes)

# load as a fluid page
fluidPage(
  
  # loading the "slate" theme
  theme = shinytheme('slate'),
  
  # adding JS functionalities
  shinyjs::useShinyjs(),
  
  # UI header
  tags$head(
    
    tags$style(HTML('.shiny-output-error-validation {color: red;}')),
    
    # change colour of links
    tags$style('
    .link {color: #91b9a4;} 
    .link {float: right;} 
    .link:hover {color: #a41034;}')),
  
  
    # change colour of 
    tags$style(HTML('.dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }
                    ### ADD THIS HERE ###
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}

                    ###To change text and background color of the `Search` box ###
                    .dataTables_filter input {color: #ffffff;background-color: #0E334A}

                    thead {color: #ffffff;}
                    tbody {color: #ffffff;}')),
  
  # title of the page
  titlePanel('WIAD: Wood Image Analysis and Dataset'),
  
  # the tabset containts four tab panels
  tabsetPanel(
    
    # main tab panel
    tabPanel('Toolbox',
             # section breaker
             br(),
              
             # sidebar panel
             sidebarPanel(
               
               # the file input only accepts jpeg, png or tiff.
               fileInput(inputId = 'image', 
                         label = 'Choose image file',
                         multiple = FALSE,
                         accept = c('image/jpeg',
                                    'image/png',
                                    'image/tiff')),
                
               # the file input only accepts csv and json.
               fileInput(inputId = 'labelUpload', 
                         label = 'Upload label file',
                         multiple = FALSE,
                         accept = c('text/csv', 
                                    'text/json',
                                    '.json')),
                
               # the file input only accepts csv and json.
               fileInput(inputId = 'metadataUpload', 
                         label = 'Upload metadata or enter it manually below',
                         multiple = FALSE,
                         accept = c('text/csv',
                                    'text/json',
                                    '.json',
                                    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
                
               # bring the link closer to the metadata upload
               div(style = "margin-top:-15px; margin-bottom:25px"),
                
               # download link to retrieve metadata template
               downloadLink(outputId = 'downloadTemplate',
                            label = 'Download metadata template', 
                            class = 'link'),
                
               # Horizontal line -----------------------------------------------
               tags$hr(),
                
               # asking the owner name
               textInput(inputId = 'ownerName', 
                         label = 'Name', 
                         placeholder = 'Your name'),
                
                # the owners' email
                textInput (inputId = 'ownerEmail', 
                           label = 'Email address', 
                           placeholder = 'Email address'),
                
                # species 
                textInput (inputId = 'species', 
                           label = 'Species', 
                           placeholder = 'What genus/species?'),
                
                # the date on which the sample was collected
                dateInput (inputId = 'sampleDate', 
                           label = 'Sample date'),
                
                # radio buttons to check whether growing season had started in sample year
                radioButtons (inputId = 'sampleYearGrowingSeason', 
                              label = 'Growing season had', 
                              choices = list ('not started','only started','already ended'),
                              selected = 'not started',
                              inline = TRUE),
                
                # check for Schulman Shift according to Edmund Schulman (1956) "Dendroclimatic changes in semiarid America"
                checkboxInput (inputId = 'SchulmanShift', 
                               label = 'Schulman Shift', 
                               value = FALSE),
                
                # resolution of the sample
                numericInput (inputId = 'sampleDPI', 
                              label = 'Scan resolution (DPI)', 
                              value = NULL),
                
                # name of the location where the sample was collected
                textInput (inputId = 'siteLoc',
                           label = 'Site location', 
                           placeholder = 'Where was the sample collected?'),
                
                
                # bounding latitudes of site location
                strong ('Bounding latitudes (decimal \u00B0)'), # \u00B0 is HTML for degree symbol
                splitLayout (
                  numericInput (inputId = 'siteLatN',
                                label = 'Northern', 
                                value = NULL,
                                min = -90,
                                max = 90),
                  numericInput (inputId = 'siteLatS',
                                label = 'Southern', 
                                value = NULL,
                                min = -90,
                                max = 90)),
                
                # bounding longitudes of site locaiton
                strong ('Bounding longitudes (decimal \u00B0)'), # \u00B0 is HTML for degree symbol
                splitLayout (
                  numericInput (inputId = 'siteLonW',
                                label = 'Western', 
                                value = NULL,
                                min = -180,
                                max = 180),
                  numericInput (inputId = 'siteLonE',
                                label = 'Eastern', 
                                value = NULL,
                                min = -180,
                                max = 180)),
                
                # identifier of the location where the sample was collected
                textInput (inputId = 'siteLocID',
                           label = 'Site ID', 
                           placeholder = 'Internal site identifier.'),
                
                # identifier of the plot where the sample was collected
                textInput (inputId = 'plotID',
                           label = 'Plot ID', 
                           placeholder = 'Internal plot identifier.'),
                
                # identifier for the sample
                textInput (inputId = 'sampleID', 
                           label = 'Sample ID',
                           placeholder = 'Internal sample identifier.'),
                
                # height above-ground at which the sample was taken
                numericInput (inputId = 'sampleHeight', 
                              label = 'Sample height (m)',
                              value = 1.5),
                
                # azimuth angle at which the sample was taken
                numericInput (inputId = 'sampleAzimuth', 
                              label = 'Sample azimuth (\u00B0)', # \u00B0 is HTML for degree symbol
                              value = NA),
                
                # any additional input metadata that the user might want to record
                textInput (inputId = 'sampleNote', 
                           label = 'Sample note',
                           placeholder = 'Any additional notes? Height of sample.'),
                
                # name of the collection
                textInput(inputId = 'collection', 
                          label = 'Collection',
                          placeholder = 'Name of the collection'),
                
                # name of the contributor
                textInput (inputId = 'contributor', 
                           label = 'Contributor',
                           placeholder = 'Who is the main contributor of the dataset?'),
                
                # horizontal line 
                hr (),
                
                # the user is asked to confirm the metadata each time for verification purposes
                actionButton (inputId = 'confirmMeta', 
                              label = 'Confirm metadata', 
                              inline = TRUE)
                
              ),
              
              # main panel with control, image and data table
              mainPanel (
                
                # three buttons in a single row
                fluidRow (
                  
                  # select true color RGB 
                  column (4, 
                          actionButton (inputId = 'selRGB', 
                                        label = 'True Color',
                                        width = '100%', 
                                        icon = icon ('image'))),
                  
                  # select monochoromic total brightness
                  column (4, 
                          actionButton (inputId = 'selTotBr', 
                                        label = 'Brightness',
                                        width = '100%', 
                                        icon = icon ('sun'))),
                  
                  # to show only the blue channel
                  column (4, 
                          actionButton (inputId = 'selBlue', 
                                        label = 'Blue', 
                                        width = '100%', 
                                        icon = icon ('tint')))
                ),
                
                # horizontal bar breaker
                hr (),
                
                # a fluid row that carries rotation button and zoom level bar
                fluidRow (
                  
                  # rotation button
                  column (2, actionButton (inputId = 'rotate180',
                                           label = NULL,
                                           width = '100%',
                                           icon = icon ('sync'),
                                           style = 'color: white; background-color: gray; border-color: black;')),
                  
                  # zoom level bar
                  column (10, sliderInput (inputId = 'zoomlevel',
                                           label = NULL, 
                                           min = 400, 
                                           max = 20000, 
                                           step = 1,
                                           value = 800, 
                                           ticks = FALSE,
                                           width = '100%'))
                ),
                
                # section breaker
                br (),
                
                # main image plot to show the processed image, the raw image is only stored
                # TR - Need to change this to only render the main image once.
                column(12, (div(id = 'container',
                                style = 'position:relative;width:60vw;overflow-x:auto;overflow-y:auto;',
                                div(plotOutput(outputId = 'imageRender',
                                               inline   = TRUE),
                                    style = 'position:relative; top:0; left:0;'),
                                div(plotOutput(outputId = 'imageProc', 
                                               click    = 'normal_point',
                                               dblclick = 'misc_point',
                                               inline   = TRUE),
                                    style = 'position:absolute; top:0; left:0;')))),
                
                # Checkbox input in a single fluid row
                fluidRow (
                  
                  # checkbox to check whether measuring starts at the bark
                  column (2, 
                          checkboxInput (inputId = 'barkFirst', 
                                         label = 'Bark first', 
                                         value = TRUE)),
                  
                  # checkbox to check whether pith is contained in image
                  column (2, 
                          checkboxInput (inputId = 'pithInImage', 
                                         label = 'Pith in image', 
                                         value = FALSE)),
                  
                  # checkbox to check whether years should be displayed
                  column (2,
                          checkboxInput (inputId = 'displayYears',
                                         label = 'Show years',
                                         value = TRUE)),
                  
                  # checkbox to check whether labels should be displayed
                  column (2,
                          checkboxInput (inputId = 'displayLabels',
                                         label = 'Show labels',
                                         value = TRUE)),
                  
                  
                  # checkbox to check whether label numbers should be displayed
                  column (3,
                          checkboxInput (inputId = 'displayLabelIDs',
                                         label = 'Show label numbers',
                                         value = FALSE))
                  
                ),
                
                # horizontal bar breaker
                hr (),
                
                # Four buttons in a single fluid row
                fluidRow (
                  
                  # Clear all the points
                  column (2, 
                          actionButton (inputId = 'clearCanvas', 
                                        label = 'Erase', 
                                        icon = icon ('eraser'), 
                                        class='btn-primary',
                                        width = '100%',
                                        style = 'font-weight: bold;')),
                  
                  
                  # Undo the last click
                  column (2,  
                          actionButton (inputId = 'undoCanvas', 
                                        label = 'Undo',
                                        icon = icon ('undo'), 
                                        class = 'btn-primary', 
                                        width = '100%', 
                                        style = 'font-weight: bold;')),
                  
                  # On or off the linker status
                  column (2, 
                          actionButton (inputId = 'linkerPoint', 
                                        label = 'Link',
                                        icon = icon ('link'), 
                                        class = 'btn-primary', 
                                        width = '100%', 
                                        style = 'font-weight: bold;')), 
                  
                  # Convert type to 'pith' or 'oldest ring'
                  column(3, 
                         actionButton(inputId = 'pith', 
                                      label = 'Oldest ring',
                                      icon = icon ('bullseye'), 
                                      class = 'btn-primary', 
                                      width = '100%', 
                                      style = 'font-weight: bold;')),
                  
                  # Button to switch to demo mode and load a demo image
                  column(3, uiOutput(outputId = 'demoButton'))
                ),
                
                # section breaker
                br(),
                
                # horizontal bar breaker
                hr(),
                
                # show the growth table 
                DT::dataTableOutput(outputId = 'growth_table')
                
              ), # end of fluid row with datatable
              
              # Two download buttons in a single fluid row
              fluidRow(
                
                # to download the ring table in CSV
                downloadButton(outputId = 'downloadCSV', 
                               label    = 'Download CSV'),
                
                # to download the ring table in JSON format, this will include metadat
                downloadButton (outputId = 'downloadJSON', 
                                label    = 'Download JSON') 
                
              ) 
    ),
    
    # tabpanel for plitting the growth curve
    tabPanel ('Plot board',
              
              # add some space
              br (), 
              
              # sidebar panel
              sidebarPanel (
                
                # input panel for method of detrending for which we use the dplR package
                selectInput (inputId = 'detrendingMethod',
                             label = 'Detrending method',
                             choices = c ('Spline',
                                          'Modified negative exponential',
                                          'Mean',
                                          'Prewhitening',
                                          'Friedman',
                                          'Modified Hugershoff'),
                             selected = 'Mean'),
                
                # conditional panel for spline detrending
                conditionalPanel (condition = "input.detrendingMethod == 'Spline'",
                                  
                                  # set frequency response for the spline
                                  sliderInput (inputId = 'detrendingFrequencyResponse',
                                               label = 'Frequency response',
                                               min = 0, max = 1, step = 0.05,
                                               value = 0.5,
                                               ticks = FALSE),
                                  
                                  # set the wavelength for the spline
                                  sliderInput (inputId = 'detrendingWavelength',
                                               label = 'Wavelength',
                                               min   = 0, 
                                               max   = 100, 
                                               step  = 1,
                                               value = 50,
                                               ticks = FALSE)
                ),
                
                # conditional panel for modified negative exponential detrending
                conditionalPanel (condition = "input.detrendingMethod == 'Modified negative exponential'",
                                  
                                  # check whether positive slopes are allowed for modified negative exponential
                                  checkboxInput (inputId = 'detrendingPosSlope',
                                                 label = 'Positive slopes allowed',
                                                 value = FALSE),
                                  
                                  # set constraints for NLS for modified negative exponential
                                  selectInput (inputId = 'detrendingConstrainNLS',
                                               label = 'Constrain NLS',
                                               choices = c ('Never',' When it fails','Always'),
                                               selected = 'Never')
                ),
                
                # conditional panel for prewhitening detrending using Ar model
                # conditionalPanel (condition = "input.detrendingMethod == 'Prewhitening'"),
                
                # conditional panel for Friedman's Super Smoother detrending
                conditionalPanel (condition = "input.detrendingMethod == 'Friedman'",
                                  
                                  # set smoothness
                                  sliderInput (inputId = 'detrendingBASS',
                                               label = 'Smoothness',
                                               min   = 0, 
                                               max   = 10,
                                               value = 0)
                                  
                ),
                
                # conditional panel for modified Hugershoff detrending
                conditionalPanel (condition = "input.detrendingMethod == 'Modified Hugershoff'",
                                  
                                  # check whether positive slopes are allowed for modified Hugershoff
                                  checkboxInput (inputId = 'detrendingPosSlope',
                                                 label = 'Positive slopes allowed',
                                                 value = FALSE),
                                  
                                  # set constraints for NLS for modified Hugershoff
                                  selectInput (inputId = 'detrendingConstrainNLS',
                                               label = 'Constrain NLS',
                                               choices = c ('Never',' When it fails','Always'),
                                               selected = 'Never')
                ),
                
                # choose betwee residuals being computed by substraction or divison
                selectInput (inputId  = 'detrendingDifference',
                             label    = 'Compute residuals by',
                             choices  = c ('Division','Substraction'),
                             selected = 'Division'),
                
                # download button for label table, metadata and RWI in JSON format
                downloadButton (outputId = 'downloadRWI_JSON', 
                                label    = 'Download RWI series') 
                
              ),
              
              # create main panel
              mainPanel (
                
                # plot the absolute radial growth over time
                plotlyOutput (outputId = 'growth_plot', 
                              height = "500px", 
                              width = "100%"),
                
                # horizontal bar breaker
                hr (),
                
                # plot the detrended ring width index over time
                plotlyOutput (outputId = 'detrended_growth_plot', 
                              height = "500px", 
                              width = "100%"),
                
                # add a little gray space
                br ()
                
              )
    ),
    
    # tabpabel for the about page
    tabPanel ('About',
              
              br (), 
              
              # load from the markdown document
              includeMarkdown (system.file(package = 'wiad', 'about.md'))
    ),
    
    # tabpanel for fair use and copyright policy
    tabPanel ('Fair use policy',
              
              #load from the markdown document
              includeMarkdown (system.file(package = 'wiad', 'fair-use.md'))
    )
    
  )
)

