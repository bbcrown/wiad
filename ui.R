fluidPage(
  theme= shinytheme('slate'),
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    }
    "))
  ),
  titlePanel("Tree Ring Image Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("image", "Choose JPG File",
                multiple = TRUE,
                accept = c("image/jpeg",
                           ".jpg")),
      
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
      plotOutput("image")
      
    )
    
  )
)