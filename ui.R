#######################################################################
# The UI side for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################


fluidPage(
  
  theme= shinytheme('slate'),
  
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    }
    "))
  ),
  
  titlePanel("TRIAD: Tree Ring Image Analysis and Dataset"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("image", "Choose the image file",
                multiple = FALSE,
                accept = c("image/jpeg",
                           "image/png",
                           "image/tiff")),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput('ownerName', "Name", placeholder = 'Your name'),
      
      textInput('ownerEmail', "Email address", placeholder = 'email address'),
      
      textInput('spp', "Species", placeholder = 'What genus/species?'),
      
      textInput('sampleDate', "Sample Date", placeholder = 'When was the sample collected?'),
      dateInput('sampleDate', "Sample Date"),
      
      textInput('sampleLoc', "Sample Location", placeholder = 'Where was the sample collected from?'),
      
      textInput('sampleNote', "Sample note", placeholder = 'Any additional note?'),
      
      
      hr(),
      radioButtons('confirmMeta', label = NULL, choices = c('Metadata Not Confirmed!', 'Metadata Confirmed!')),
      hr(),
      actionButton('saveData', 'Save', width = '100%', icon = icon('save'))
    ),
    
    mainPanel(
      
      fluidRow(
        column(6, actionButton('selRGB', 'RGB', width = '100%', icon=icon("bell"))),
        column(6, actionButton('selTotBr', 'Total Brightness', width = '100%', icon=icon("bell")) )
      ),
      
      br(),
      
      fluidRow(
        column(4, actionButton('selRed', 'Red', width = '100%', icon=icon("bitbucket"), 
                               style="color: red; background-color: red; border-color: black;")),
        
        column(4, actionButton('selHue', 'Hue', width = '100%') ),
        
        column(4, actionButton('selBright', 'Brightness', width = '100%', icon=icon("sun-o"), 
                               style="color: yellow; background-color: yellow; border-color: black;"))
      ),
      
      fluidRow(
        column(4, actionButton('selGreen', 'Green', width = '100%', icon=icon("bitbucket"),
                               style="color: green; background-color: green; border-color: black;")),
        
        column(4, actionButton('selSat', 'Saturation', width = '100%')),
        
        column(4, actionButton('selDark', 'Darkness', width = '100%', icon=icon("moon-o"), 
                               style="color: black; background-color: black; border-color: black;"))
      ),
      
      fluidRow(
        column(4, actionButton('selBlue', 'Blue', width = '100%', icon=icon("bitbucket"),
                               style="color: blue; background-color: blue; border-color: black;")),
        
        column(4, actionButton('selValue', 'Value', width = '100%')),
        
        column(4, actionButton('selContrast', 'Contrast', width = '100%', icon=icon("moon"), 
                               style="color: gray; background-color: gray; border-color: black;")
        )
      ),
      
      hr(),
      
      plotOutput("imageProc", click = "ring_point", width = '100%', height = '100%'),
      
      hr(),
      fluidRow(
        column(4, actionButton("clearCanvas", "Erase", 
                               icon = icon('eraser'), 
                               class="btn-primary", width = "100%",
                               style='font-weight: bold;')),
        
        column(4,  actionButton("undoCanvas", "Undo",
                                icon = icon('undo'), 
                                class="btn-primary", 
                                width = "100%", 
                                style='font-weight: bold;')),
        
        column(4,  actionButton("linkerPoint", "Linker On/Off",
                                icon = icon('link'), 
                                class="btn-primary", 
                                width = "100%", 
                                style='font-weight: bold;'))
      ),
      
      br(),
      radioButtons('barkSide', label = NULL, choices = c('First Bark', 'Last Bark'), inline = TRUE, width = '100%'),
      hr(),
      dataTableOutput('ring_table'),
      downloadButton('downloadCSV', label = 'Download CSV'),
      downloadButton('downloadJSON', label = 'Download JSON')
    )
  )
)