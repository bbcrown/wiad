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
                           "image/tiff",
                           "image/tiff")),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput('ownerName', "Name", placeholder = 'Your name'),
      
      textInput('ownerEmail', "Email address", placeholder = 'email address'),
      
      textInput('spp', "Species", placeholder = 'What genus/species?'),
      
      textInput('sampleDate', "Sample Date", placeholder = 'When was the sample collected?'),
      
      textInput('sampleLoc', "Sample Location", placeholder = 'Where was the sample collected from?'),
      
      textInput('sampleNote', "Sample note", placeholder = 'Any additional note?'),
      
      hr(),
      
      actionButton('saveData', 'Save', width = '100%', icon = icon('save'))
    ),
    
    mainPanel(
      # Output: Data file ----
      plotOutput("imageOrig", click = "orgPoint"),
      
      hr(),
      
      fluidRow(
        column(3, 
               actionButton('selRed', 'Red', width = '100%', icon=icon("bitbucket"), style="color: red; background-color: red; border-color: black;")
        ),
        
        column(3, 
               actionButton('selHue', 'Hue', width = '100%')
        ),
        
        column(3,
               actionButton('selBright', 'Brightness', width = '100%', icon=icon("sun-o"), style="color: yellow; background-color: yellow; border-color: black;")
        ),
        
        column(3,
               actionButton('selTotBr', 'Total Br.', width = '100%', icon=icon("bell"))
        )
      ),
      
      fluidRow(
        column(3, 
               actionButton('selGreen', 'Green', width = '100%', icon=icon("bitbucket"), style="color: green; background-color: green; border-color: black;")
        ),
        
        column(3, 
               actionButton('selSat', 'Saturation', width = '100%')
        ),
        
        column(3, 
               actionButton('selDark', 'Darkness', width = '100%', icon=icon("moon-o"), style="color: black; background-color: black; border-color: black;")
        ),
        
        column(3, 
               actionButton('selCluster', 'Clus', width = '100%', icon=icon("bell"))
        )
      ),
      
      fluidRow(
        column(3,
               actionButton('selBlue', 'Blue', width = '100%', icon=icon("bitbucket"), style="color: blue; background-color: blue; border-color: black;")
        ),
        
        column(3, 
               actionButton('selValue', 'Value', width = '100%')
        ),
        
        column(3, 
               actionButton('selContrast', 'Contrast', width = '100%', icon=icon("moon"), style="color: gray; background-color: gray; border-color: black;")
        ),
        
        column(3, 
               actionButton('selInverse', 'Inv', width = '100%', icon=icon("bell"))
        )
      ),
      
      br(),
      
      plotOutput("imageProc", click = "prcPoint")
      
    )
    
  )
)