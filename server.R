shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(imagepath = NULL,
                       img = NULL,
                       outName = NULL)
  
  
  observeEvent(input$image,{
    rv$outName <- gsub(as.character(Sys.time()), pattern = ' |:', replacement = '-')
    rv$imagepath <- input$image$datapath
    rv$img <- readJPEG(rv$imagepath)
    writeJPEG(rv$img, paste0('images/', rv$outName, '.jpg'))
    
  })
  
  metaData <- reactive({
    meta <- list(ownerName = input$ownerName, 
                 ownerEmail = input$ownerEmail,
                 species = input$spp,
                 sampleDate = input$sampleDate,
                 sampleLoc = input$sampleLoc,
                 sampleNote = input$sampleNote
                
    )
    toJSON(meta)
  })
  
  observeEvent(input$saveData,{
    write(metaData(), paste0('images/', rv$outName, '.json'))
    showModal(strong(modalDialog('Sample and metadata were stored!', 
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 footer = NULL, easyClose = T, size = 'm')))  })
  
  output$image <- renderPlot({
    if(is.null(rv$imagepath)) return()
    dummy =0
    
    plotJPEG(rv$imagepath)
  })
  
}
)