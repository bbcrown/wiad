#######################################################################
# The UI side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################


fluidPage(
  
  theme= shinytheme('slate'),
  
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML('
    .shiny-output-error-validation {
    color: red;
    }
    '))
  ),
  
  titlePanel('TRIAD: Tree Ring Image Analysis and Dataset'),
  # headerPanel('TRIAD: Tree Ring Image Analysis and Dataset'),
  tabsetPanel(
    tabPanel('TRIAD Toolbox',
             # sidebarLayout(
             
             sidebarPanel(
               
               fileInput('image', 'Choose the image file',
                         multiple = FALSE,
                         accept = c('image/jpeg',
                                    'image/png',
                                    'image/tiff')),
               
               # Horizontal line ----
               tags$hr(),
               
               textInput(inputId = 'ownerName', 
                         label = 'Name', 
                         placeholder = 'Your name'),
               
               textInput(inputId = 'ownerEmail', 
                         label = 'Email address', 
                         placeholder = 'email address'),
               
               textInput(inputId = 'spp', 
                         label = 'Species', 
                         placeholder = 'What genus/species?'),
               
               dateInput(inputId = 'sampleDate', 
                         label = 'Sample Date'),
               
               numericInput(inputId = 'sampleYear', 
                            label = 'Sample Year', 
                            min = 1800, 
                            max = year(Sys.Date()),
                            value = 2010),
               
               textInput(inputId = 'sampleLoc',
                         label = 'Sample Location', 
                         placeholder = 'Where was the sample collected from?'),
               
               textInput(inputId = 'sampleNote', 
                         label = 'Sample note',
                         placeholder = 'Any additional note?'),
               
               hr(),
               
               radioButtons(inputId = 'confirmMeta', 
                            label = 'Metadata', 
                            choices = c('Not Confirmed', 'Confirmed'), 
                            inline = TRUE),
               
               hr(),
               
               actionButton(inputId = 'saveData', 
                            label = 'Save', 
                            width = '100%', icon = icon('save'))
             ),
             
             mainPanel(
               
               fluidRow(
                 column(6, 
                        actionButton(inputId = 'selRGB', 
                                     label = 'RGB',
                                     width = '100%', 
                                     icon=icon('bell'))
                 ),
                 
                 column(6, 
                        actionButton(inputId = 'selTotBr', 
                                     label = 'Total Brightness',
                                     width = '100%', 
                                     icon=icon('bell')) 
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(4, 
                        actionButton(inputId = 'selRed', 
                                     label = 'Red', 
                                     width = '100%', 
                                     icon=icon('bitbucket'), 
                                     style='color: red; background-color: red; border-color: black;')
                 ),
                 
                 column(4,
                        actionButton(inputId = 'selHue', 
                                     label = 'Hue',
                                     width = '100%') 
                 ),
                 
                 column(4, 
                        actionButton(inputId = 'selBright', 
                                     label = 'Brightness', 
                                     width = '100%', 
                                     icon=icon('sun-o'), 
                                     style='color: yellow; background-color: yellow; border-color: black;')
                 )
               ),
               
               fluidRow(
                 column(4, 
                        actionButton(inputId = 'selGreen', 
                                     label = 'Green',
                                     width = '100%',
                                     icon=icon('bitbucket'),
                                     style='color: green; background-color: green; border-color: black;')
                 ),
                 
                 column(4, 
                        actionButton(inputId = 'selSat', 
                                     label = 'Saturation',
                                     width = '100%')
                 ),
                 
                 column(4, 
                        actionButton(inputId = 'selDark', 
                                     label = 'Darkness', 
                                     width = '100%', 
                                     icon=icon('moon-o'), 
                                     style='color: black; background-color: black; border-color: black;')
                 )
               ),
               
               fluidRow(
                 column(4, 
                        actionButton(inputId = 'selBlue', 
                                     label = 'Blue', 
                                     width = '100%', 
                                     icon=icon('bitbucket'),
                                     style='color: blue; background-color: blue; border-color: black;')
                 ),
                 
                 column(4, 
                        actionButton(inputId = 'selValue', 
                                     label = 'Value', 
                                     width = '100%')
                 ),
                 
                 column(4, actionButton(inputId = 'selContrast', 
                                        label = 'Contrast',
                                        width = '100%', 
                                        icon=icon('moon'), 
                                        style='color: gray; background-color: gray; border-color: black;')
                 )
               ),
               
               hr(),
               
               plotOutput(outputId = 'imageProc', 
                          click = 'ring_point', 
                          width = '100%', 
                          height = '100%'),
               
               hr(),
               fluidRow(
                 column(4, 
                        actionButton(inputId = 'clearCanvas', 
                                     label = 'Erase', 
                                     icon = icon('eraser'), 
                                     class='btn-primary',
                                     width = '100%',
                                     style='font-weight: bold;')),
                 
                 column(4,  
                        actionButton(inputId = 'undoCanvas', 
                                     label = 'Undo',
                                     icon = icon('undo'), 
                                     class='btn-primary', 
                                     width = '100%', 
                                     style='font-weight: bold;')
                 ),
                 
                 column(4, 
                        actionButton(inputId = 'linkerPoint', 
                                     label = 'Linker On/Off',
                                     icon = icon('link'), 
                                     class='btn-primary', 
                                     width = '100%', 
                                     style='font-weight: bold;')
                 )
               ),
               
               br(),
               
               radioButtons(inputId = 'barkSide', 
                            label = NULL, 
                            choices = c('Bark First', 'Bark Last'), 
                            inline = TRUE, 
                            width = '100%'),
               
               hr(),
               
               dataTableOutput(outputId = 'ring_table'),
               
               downloadButton(outputId = 'downloadCSV', 
                              label = 'Download CSV'),
               
               downloadButton(outputId = 'downloadJSON', 
                              label = 'Download JSON')
               
             )
    ),
    
    tabPanel('TRIAD Plot Board',
             {
               mainPanel(
                 hr(),
                 plotlyOutput(outputId = 'ring_plot', 
                              height = "500px", 
                              width = "100%")
               )
             }),
    
    tabPanel('About TRIAD',{
      includeHTML( textConnection('<div id="readme" class="readme blob instapaper_body">
                                    <article class="markdown-body entry-content" itemprop="text">
                                    <br/>
                                    <p>In order to extract meaningful data from digital images of wood, one needs to : <br/>
                                    &nbsp; &nbsp; &nbsp; 1) collect and prepare the wood sample; <br/>
                                    &nbsp; &nbsp; &nbsp; 2) measure variables such as ring width; and<br/>
                                    &nbsp; &nbsp; &nbsp; 3) cross-date the resulting time series of ring width.<br/> <br/>
                                    However, these steps are painstaking, lack transparency and reproducibility, thus could hugely profit from traceable automation.</p> 
                                    <p>In its first version, the TRIAD toolbox provides an simple interactive web interface to facilitate the measurement of tree ring widths, make it reproducible and shareable. The tool is freely avaiable online and stores the images and data series for easy sharing with collaborators and the public access after a five year embargo period.</p>
                                    <p>While images are accummulating in the TRIAD database, novel techniques to automate data extraction and cross-dating are continuously being developed by the TRIAD core development team. Features such as automatic tree ring detection and automatic cross-dating suggestions will be added as soon as they have been thoroughly tested.</p>
                                    <p>The mission of the TRIAD team is to facilitate tree-ring science through automation and by providing a platform for collaboration, and ultimately to move tree ring science from a domain of small to big data.</p>
                                    <br/>
                                    
                                    </article>
                                    </div>'))
    }
    )
  )
)


# <p>The web interface is developed and maintained by <a href="https://github.com/bnasr" target="_blank">Bijan Seyednarollah</a>.</p>
  