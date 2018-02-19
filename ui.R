fluidPage(
  theme= shinytheme('slate'),
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
                multiple = TRUE,
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
      plotOutput("imageOrig"),
      hr(),
      fluidRow(column(1, actionButton('Red', 'R', width = '100%')),
               column(1, actionButton('Green', 'G', width = '100%')),
               column(1, actionButton('Blue', 'B', width = '100%')),
               column(1, actionButton('Hue', 'H', width = '100%')),
               column(1, actionButton('Sat', 'S', width = '100%')),
               column(1, actionButton('Value', 'V', width = '100%')),
               column(1, actionButton('Bright', 'Bri', width = '100%')),
               column(1, actionButton('Dark', 'Drk', width = '100%')),
               column(1, actionButton('totBr', 'tBR', width = '100%')),
               column(1, actionButton('Clusted', 'Clus', width = '100%')),
               column(1, actionButton('Inverse', 'Inv', width = '100%')),
               column(1, actionButton('Inverse', 'Inv', width = '100%'))
               
      ),
      br(),
      plotOutput("imageProc")
      
    )
    
  )
)