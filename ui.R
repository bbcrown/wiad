#######################################################################
# The UI side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################


# load as a fluid page
fluidPage(
  
  # loading the "slate" theme
  theme= shinytheme('slate'),
  
  #adding JS functionalities
  shinyjs::useShinyjs(),
  
  # UI header
  tags$head(
    
    tags$style(HTML('
    .shiny-output-error-validation {
    color: red;
    }

    '))
  ),
  
  # title of the page
  titlePanel('TRIAD: Tree Ring Image Analysis and Dataset'),
  
  # the tabset containts four tab panels
  tabsetPanel(
    
    # main tab panel
    tabPanel('TRIAD Toolbox',
             
             # sidebar panel
             sidebarPanel(
               
               # the file input only accepts jpeg, png or tiff.
               fileInput('image', 'Choose the image file',
                         multiple = FALSE,
                         accept = c('image/jpeg',
                                    'image/png',
                                    'image/tiff')),
               
               # Horizontal line ----
               tags$hr(),
               
               # asking the owner name
               textInput(inputId = 'ownerName', 
                         label = 'Name', 
                         placeholder = 'Your name'),
               
               # the owners' email
               textInput(inputId = 'ownerEmail', 
                         label = 'Email address', 
                         placeholder = 'Email address'),
               
               # species 
               textInput(inputId = 'spp', 
                         label = 'Species', 
                         placeholder = 'What genus/species?'),
               
               # the date on which the sample was collected
               dateInput(inputId = 'sampleDate', 
                         label = 'Sample Date'),
               
               # this might look redundant to the "date" entry, however this is used to double check user's input
               numericInput(inputId = 'sampleYear', 
                            label = 'Sample year', 
                            min = 1800, 
                            max = year(Sys.Date()),
                            value = 2019),
               
               # resolution of the sample
               numericInput(inputId = 'sampleDPI', 
                            label = 'Scan resolution (DPI)', 
                            value = NULL),
               
               # name of the location where the sample was collected
               textInput(inputId = 'sampleLoc',
                         label = 'Sample location', 
                         placeholder = 'Where was the sample collected from?'),
               
               # any additional input metadata that the user might want to record
               textInput(inputId = 'sampleNote', 
                         label = 'Sample note',
                         placeholder = 'Any additional notes?'),
               
               # name of the collection
               textInput(inputId = 'sampleID', 
                         label = 'Sample ID',
                         placeholder = 'Internal sampel ID'),
               
               # name of the collection
               textInput(inputId = 'collection', 
                         label = 'Collection',
                         placeholder = 'Name of the collection'),
               
               # name of the contributor
               textInput(inputId = 'contributor', 
                         label = 'Contributor',
                         placeholder = 'Who is the main contributor of the dataset?'),
               
               # horizontal line 
               hr(),
               
               # the user is asked to confirm the metadata each time for verification purposes
               radioButtons(inputId = 'confirmMeta', 
                            label = 'Metadata', 
                            choices  = list('Not Confirmed', 'Confirmed'),
                            selected = 'Not Confirmed',
                            # choiceNames = list(icon("save"), icon("next")),
                            inline = TRUE)
               
             ),
             
             mainPanel(
               
               # two buttons in a single row
               fluidRow(
                 
                 # select true color RGB 
                 column(4, 
                        actionButton(inputId = 'selRGB', 
                                     label = 'True Color',
                                     width = '100%', 
                                     icon=icon('image'))
                 ),
                 
                 # select monochoromic total brightness
                 column(4, 
                        actionButton(inputId = 'selTotBr', 
                                     label = 'Brightness',
                                     width = '100%', 
                                     icon=icon('sun')) 
                 ),
                 # to show only the blue channel
                 column(4, 
                        actionButton(inputId = 'selBlue', 
                                     label = 'Blue', 
                                     width = '100%', 
                                     icon=icon('tint'))
                 )
               ),
               
               
               # horizontal bar breaker
               hr(),
               
               # a fluid row that carries rotation button and zoom level bar
               fluidRow(
                 
                 # rotation button
                 column(2,
                        actionButton(inputId = 'rotate180',
                                     label = NULL,
                                     width = '100%',
                                     icon=icon('sync'),
                                     style='color: white; background-color: gray; border-color: black;'),
                        
                        # checkbox to check whether meaasuring starts at the bark
                        checkboxInput(inputId = 'barkSide', 
                                      label = 'Bark First', 
                                      value = TRUE)
                 ),
                 
                 # zoom level bar
                 column(10, sliderInput(inputId = 'zoomlevel',
                                        label = 'Width', 
                                        min = 400, 
                                        max = 10000, 
                                        step = 1,
                                        value = 800, 
                                        ticks = FALSE,
                                        width = '100%'))
               ),
               
               # section breaker
               br(),
               
               # main image plot to show the processed image, the raw image is only stored
               plotOutput(outputId = 'imageProc', 
                          click = 'ring_point',
                          # width = 'auto',
                          # height = 'auto'
                          inline = TRUE
               ),
               
               # Checkbox input in a single fluid row
               fluidRow(
                 
                 column(3,
                   helpText('Sample year growth:')),
                 
                 column(3,
                   # checkboxGroup to check whether there is some, none or full growth for the sample year
                   radioButtons(inputId = 'sampleYearGrowth', 
                                label = NULL, 
                                choices = list ('none', 'some', 'all'),
                                selected = 'none',
                                inline = TRUE)
                 )
               ),

               # horizontal bar breaker
               hr(),
               
               # Four buttons in a single fluid row
               fluidRow(
                 
                 # Clear all the points
                 column(3, 
                        actionButton(inputId = 'clearCanvas', 
                                     label = 'Erase', 
                                     icon = icon('eraser'), 
                                     class='btn-primary',
                                     width = '100%',
                                     style='font-weight: bold;')),
                 
                 
                 # Undo the last click
                 column(3,  
                        actionButton(inputId = 'undoCanvas', 
                                     label = 'Undo',
                                     icon = icon('undo'), 
                                     class='btn-primary', 
                                     width = '100%', 
                                     style='font-weight: bold;')
                 ),
                 
                 # On or off the linker status
                 column(3, 
                        actionButton(inputId = 'linkerPoint', 
                                     label = 'Link',
                                     icon = icon('link'), 
                                     class='btn-primary', 
                                     width = '100%', 
                                     style='font-weight: bold;')
                 ), 
                 
                 # Convert type to 'pith'
                 column(3, 
                        actionButton(inputId = 'pith', 
                                     label = 'Pith',
                                     icon = icon('bullseye'), 
                                     class='btn-primary', 
                                     width = '100%', 
                                     style='font-weight: bold;')
                 ) 
               ),
               
               # section breaker
               br(),
               
        
               # horizontal bar breaker
               hr(),
               
               # show the ring table 
               dataTableOutput(outputId = 'ring_table'),
               
               # to download the ring table in CSV
               downloadButton(outputId = 'downloadCSV', 
                              label = 'Download CSV'),
               
               # to download the ring table in JSON format, this will include metadat
               downloadButton(outputId = 'downloadJSON', 
                              label = 'Download JSON') 
               
             ) 
    ),
    
    # tabpanel for plitting the growth curve
    tabPanel('TRIAD Plot Board',
             {
               mainPanel(
                 
                 # horizontal bar breaker
                 hr(),
                 
                 # plot the growth table
                 plotlyOutput(outputId = 'ring_plot', 
                              height = "500px", 
                              width = "100%")
               )
             }),
    
    # tabpabel for the about page
    tabPanel('About TRIAD',
             
             # load from the markdown document
             includeMarkdown('about.md')
    ),
    
    # tabpanel for fair use and copyright policy
    tabPanel('Fair Use Policy',
             
             #load from the markdown document
             includeMarkdown('fair-use.md')
    )
    
  )
)

