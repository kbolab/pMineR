
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  fileProposto <- c()

  observeEvent(input$importButton, {
    # showNotification( pMineR.IO.shiny.dataLoader.list$nomeDelFile )
    pMineR.IO.shiny.dataLoader.list$sep <<- input$separator
    pMineR.IO.shiny.dataLoader.list$IDColumnName <<- input$IDColumnName
    pMineR.IO.shiny.dataLoader.list$eventColumnName <<- input$eventColumnName
    pMineR.IO.shiny.dataLoader.list$dateColumnName <<- input$dateColumnName
    pMineR.IO.shiny.dataLoader.list$formatoData <<- input$formatoData
    pMineR.IO.shiny.dataLoader.list$badDateSuppressing <<- input$badDateSuppressing
    pMineR.IO.shiny.dataLoader.list$UTF8ForceConversion <<- input$UTF8ForceConversion
    pMineR.IO.shiny.dataLoader.list$esito <<- "carica"
    stopApp() 
  })
  
  observeEvent(input$quitButton, {
    # showNotification( pMineR.IO.shiny.dataLoader.list$nomeDelFile )
    pMineR.IO.shiny.dataLoader.list$sep <<- input$separator
    pMineR.IO.shiny.dataLoader.list$IDColumnName <<- input$IDColumnName
    pMineR.IO.shiny.dataLoader.list$eventColumnName <<- input$eventColumnName
    pMineR.IO.shiny.dataLoader.list$dateColumnName <<- input$dateColumnName
    pMineR.IO.shiny.dataLoader.list$formatoData <<- input$formatoData
    pMineR.IO.shiny.dataLoader.list$badDateSuppressing <<- input$badDateSuppressing
    pMineR.IO.shiny.dataLoader.list$esito <<- "quit"
    stopApp() 
  })  

  observeEvent(input$separator, {
    
    output$contents <- renderTable({
      fileProposto <<-  read.csv(pMineR.IO.shiny.dataLoader.list$nomeDelFile, header = T,nrows = 10,sep = input$separator)
      output$contents <-   renderTable(  fileProposto )
      updateSelectInput(session, "IDColumnName", choices = names(fileProposto)  )
      updateSelectInput(session, "eventColumnName", choices = names(fileProposto)  )
      updateSelectInput(session, "dateColumnName", choices = names(fileProposto)  )
    })
  })
})
