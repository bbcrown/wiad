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
                         placeholder = 'Email address'),
               
               textInput(inputId = 'spp', 
                         label = 'Species', 
                         placeholder = 'What genus/species?'),
               
               dateInput(inputId = 'sampleDate', 
                         label = 'Sample Date'),
               
               numericInput(inputId = 'sampleYear', 
                            label = 'Sample year', 
                            min = 1800, 
                            max = year(Sys.Date()),
                            value = 2010),
               
               numericInput(inputId = 'sampleDPI', # TTR add option to measure scale bar in the image as pop-up dialogue, maybe?
                            label = 'Scan resolution (DPI)', 
                            value = NULL),
               
               textInput(inputId = 'sampleLoc',
                         label = 'Sample location', 
                         placeholder = 'Where was the sample collected from?'),
               
               textInput(inputId = 'sampleNote', 
                         label = 'Sample note',
                         placeholder = 'Any additional notes?'),
               
               hr(),
               
               radioButtons(inputId = 'confirmMeta', # TTR change to button saying 'Not confimred' until to click on it and it says 'confirmed'. Maybe with a disk icon?
                            label = 'Metadata', 
                            choices = c('Not Confirmed', 'Confirmed'), 
                            inline = TRUE)
               
               # hr(),
               
               # actionButton(inputId = 'saveData', 
               #              label = 'Save', 
               #              width = '100%', icon = icon('save'))
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
                        actionButton(inputId = 'selGreen', 
                                     label = 'Green',
                                     width = '100%',
                                     icon=icon('bitbucket'),
                                     style='color: green; background-color: green; border-color: black;')
                 ),
                 
                 column(4, 
                        actionButton(inputId = 'selBlue', 
                                     label = 'Blue', 
                                     width = '100%', 
                                     icon=icon('bitbucket'),
                                     style='color: blue; background-color: blue; border-color: black;')
                 )
               ),
               
               # column(4,
               #        actionButton(inputId = 'selHue', 
               #                     label = 'Hue',
               #                     width = '100%') 
               # ),
               
               # column(4, 
               #        actionButton(inputId = 'selBright', 
               #                     label = 'Brightness', 
               #                     width = '100%', 
               #                     icon=icon('sun-o'), 
               #                     style='color: yellow; background-color: yellow; border-color: black;')
               # )
               # column(4, 
               #        actionButton(inputId = 'selSat', 
               #                     label = 'Saturation',
               #                     width = '100%')
               # ),
               # 
               # column(4, 
               #        actionButton(inputId = 'selDark', 
               #                     label = 'Darkness', 
               #                     width = '100%', 
               #                     icon=icon('moon-o'), 
               #                     style='color: black; background-color: black; border-color: black;')
               # )
               
               
               # column(4, 
               #        actionButton(inputId = 'selValue', 
               #                     label = 'Value', 
               #                     width = '100%')
               # ),
               
               # column(4, actionButton(inputId = 'selContrast', 
               #                        label = 'Contrast',
               #                        width = '100%', 
               #                        icon=icon('moon'), 
               #                        style='color: gray; background-color: gray; border-color: black;')
               # )
               
               hr(),
               fluidRow(
                 column(2,
                        actionButton(inputId = 'rotate180',
                                     label = NULL,
                                     width = '100%',
                                     icon=icon('sync'),
                                     style='color: white; background-color: gray; border-color: black;')
                 ),
                 column(10, sliderInput(inputId = 'zoomlevel',
                                        label = 'Width', 
                                        min = 400, 
                                        max = 10000, 
                                        step = 1,
                                        value = 800, 
                                        ticks = FALSE,
                                        width = '100%'))
                 # column(3,
                 #        actionButton(inputId = 'zoomout',
                 #                     label = NULL,
                 #                     width = '100%',
                 #                     icon=icon('search-minus'),
                 #                     style='color: white; background-color: gray; border-color: black;')
                 # ),
                 # column(3,
                 #        actionButton(inputId = 'zoomin',
                 #                     label = NULL,
                 #                     width = '100%',
                 #                     icon=icon('search-plus'),
                 #                     style='color: white; background-color: gray; border-color: black;')
                 # )
                 
                 
               ),
               
               br(),
               plotOutput(outputId = 'imageProc', 
                          click = 'ring_point',
                          # width = 'auto',
                          # height = 'auto'
                          inline = TRUE
                          ),
               
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
                 ) # TTR Maybe we also want to add a false ring marker.
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
                              label = 'Download JSON') # TTR Once downloaded the download dialogue window stays open.
               
             ) # TTR There is a bug that when you increase the zoom the table extends all the way across, but it does not contract when the user zooms out again.
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
                                    &nbsp;<p style="margin-left: 40px">In order to extract meaningful data from digital images of wood, one needs to : <br/>
                                    &nbsp; &nbsp; &nbsp; 1) collect and prepare the wood sample; <br/>
                                    &nbsp; &nbsp; &nbsp; 2) measure variables such as ring width; and<br/>
                                    &nbsp; &nbsp; &nbsp; 3) cross-date the resulting time series.<br/> <br/>
                                    However, these steps are painstaking, and lack transparency and reproducibility, if performed with a linear table, thus could hugely profit from traceable automation.</p> 
                                    <p style="margin-left: 40px">In its first version, the TRIAD toolbox provides an simple interactive web interface to facilitate the measurement of tree ring widths from scanned images and save the results in the online repository. The tool is freely avaiable online and stores the images and data series for easy sharing with collaborators and the public access after an embargo period of five years.</p>
                                    <p style="margin-left: 40px">While images are accummulating in the TRIAD database, novel techniques to automate data extraction and cross-dating are continuously being developed by the TRIAD core development team. Features such as automatic tree ring detection and automatic cross-dating will be added to the free online tool as soon as they have been thoroughly tested.</p>
                                    <p style="margin-left: 40px">The mission of the TRIAD team is to advance tree-ring science by providing a free tool and repository that will enable unforeseen explorations and new methods analysis, while equally facilitating data sharing and access. TRIAD will provide the tools to make tree ring science transparent and reproducible.</p>
                                    <br/>
                                    <p style="margin-left: 80px">Your TRIAD core development team.</p>
                                    </article>
                                    </div>'))
    }
    )
  )
)


# <p>The web interface is developed and maintained by <a href="https://github.com/bnasr" target="_blank">Bijan Seyednarollah</a>.</p>
