# Prende l'ID dell'oggetto che mi ha invocato
parent.ID <- pMineR.IO.shiny.confCheck_easy.list$parent.ID
nomeOggetto <- register.getObjName(ID = parent.ID)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # Se ha scelto di quittare
  observeEvent(input$quitButton, {
    stopApp()
  })
  
  # occupati del render del Grafico  
  output$diagram <- renderGrViz({
    
    # Costruisci il comando per invocare il metodo dell'oggetto 
    # confCheck_easy che mi ha invocato
    stringa.comando <- str_c( "grafo<-",nomeOggetto,"$plot(giveBack.grVizScript = TRUE, plotIt = FALSE)")
    eval(expr = parse(text = stringa.comando))
    # plotta il grafico
    grViz({
      grafo
    })
  })
  
  # rendering della message box nella tab della Kaplan Meier
  output$Kaplan<-renderText({ 
    if (input$stato.from.1 == "") stringa <- "please, select at least one couple FROM => TO"
    else stringa <- ""
    stringa
  })
  
  # rendering della Kaplan Meier (il grafico)
  output$Kaplan.plot<-renderPlot({
    
    # Prendi i vari stati dagli input e popola gli array FROM e TO
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
    
    # Se ne e' stato selezionato qualcuno, plotta
    if( length(arr.from)>0 & length(arr.to)>0) {
    stringa.comando <- str_c( "grafo<-",nomeOggetto,"$KaplanMeier(states.from=arr.from,states.to=arr.to)")
      eval(expr = parse(text = stringa.comando))
    }
    
    # render del p.value
    output$Kaplan<-renderText({ 
      stringa <- str_c("\nChisq= ",grafo$log.rank$chisq," on ",length(x = grafo$log.rank$n)-1," degrees of freedom, p.value = ",pchisq(grafo$log.rank$chisq,length(x = grafo$log.rank$n)-1,lower.tail = FALSE))
      stringa
    })    
    
  })

})
