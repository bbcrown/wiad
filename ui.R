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
  
  sidebarLayout(
    
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
                   value = 2000),
      
      textInput(inputId = 'sampleLoc',
                label = 'Sample Location', 
                placeholder = 'Where was the sample collected from?'),
      
      textInput(inputId = 'sampleNote', 
                label = 'Sample note',
                placeholder = 'Any additional note?'),
      
      hr(),
      
      radioButtons(inputId = 'confirmMeta', 
                   label = NULL, 
                   choices = c('Metadata Not Confirmed!', 'Metadata Confirmed!')),
      
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
  )
)