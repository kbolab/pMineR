parent.ID <- pMineR.IO.shiny.confCheck_easy.list$parent.ID
nomeOggetto <- register.getObjName(ID = parent.ID)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  observeEvent(input$doIt, {
    # # showNotification( pMineR.IO.shiny.dataLoader.list$nomeDelFile )
    # pMineR.IO.shiny.dataLoader.list$sep <<- input$separator
    # pMineR.IO.shiny.dataLoader.list$IDColumnName <<- input$IDColumnName
    # pMineR.IO.shiny.dataLoader.list$eventColumnName <<- input$eventColumnName
    # pMineR.IO.shiny.dataLoader.list$dateColumnName <<- input$dateColumnName
    # pMineR.IO.shiny.dataLoader.list$formatoData <<- input$formatoData
    # pMineR.IO.shiny.dataLoader.list$badDateSuppressing <<- input$badDateSuppressing
    # pMineR.IO.shiny.dataLoader.list$esito <<- "quit"
    # stopApp()
    showNotification("Message text1")
  })
  
  observeEvent(input$quitButton, {
    # pMineR.IO.shiny.dataLoader.list$esito <<- "quit"
    stopApp()
  })
  
  output$diagram <- renderGrViz({
    stringa.comando <- str_c( "grafo<-",nomeOggetto,"$plot(giveBack.grVizScript = TRUE, plotIt = FALSE)")
    eval(expr = parse(text = stringa.comando))
    
    grViz({
      grafo
    })
  })
  
  
  output$Kaplan<-renderText({ 
    if (input$stato.from.1 == "") stringa <- "please, select at least one couple FROM => TO"
    else stringa <- ""
    stringa
  })
  output$Kaplan.plot<-renderPlot({
    arr.from <- c(); arr.to <- c()
    if( str_trim(input$stato.from.1)!="" & str_trim(input$stato.to.1)!="" ) {
      arr.from<-c(arr.from,input$stato.from.1); arr.to<-c(arr.to,input$stato.to.1)
    }
    if( str_trim(input$stato.from.2)!="" & str_trim(input$stato.to.2)!="" ) {
      arr.from<-c(arr.from,input$stato.from.2); arr.to<-c(arr.to,input$stato.to.2)
    }    
    if( str_trim(input$stato.from.3)!="" & str_trim(input$stato.to.3)!="" ) {
      arr.from<-c(arr.from,input$stato.from.3); arr.to<-c(arr.to,input$stato.to.3)
    }    
    if( length(arr.from)>0 & length(arr.to)>0) { 
      stringa.comando <- str_c( "grafo<-",nomeOggetto,"$KaplanMeier(states.from=arr.from,states.to=arr.to)")
      eval(expr = parse(text = stringa.comando))
    }
  })
    

})
