#' class for parsing, checking and executing Petri Network
#' 
#' @description  bla bla bla bla
#'               ri bla bir bla bir bal
#' @export
#' @import XML 
petriNetworkModel<-function() {
  XML.model<-''
  loadModel.xml<-function( xmlModel ) {
    fa = newXMLNode("Main")
    parseXMLAndAdd(txt = xmlModel, parent = fa )
    XML.model<<-fa
  }
  getListaXML.link<-function(stringa, listaXML, what2Get) {
    res<-c()
    for( i in seq(1,length(listaXML$xml$graphStructure$links))) {
      if(listaXML$xml$graphStructure$links[[i]][ what2Get ] == stringa )  res<-c(res,i)
    }
    return(res)
  }
  replay<-function( arrayStatus , breakOn="error") {
    # fai una copia del modello XML, giusto per non correre 
    # il rischio di sminchiare l'attributo
    listaXML<-xmlToList( node =  XML.model )
    
    # setta il primo token: BEGIN
    tokenDisponibili<- c("BEGIN")
    
    # cicla su tutti gli stati della "parola"
    for( stato.ID in seq(1,length(arrayStatus))) {
      stato.Name <- arrayStatus[ stato.ID ]
      
      # cerca tutti i prerequisiti per lo stato corrente 'arrayStatus[stato.ID]'
      queryRes<-getListaXML.link( stringa = arrayStatus[stato.ID], listaXML = listaXML, what2Get = "to")

      # se è a lunghezza '0' significa che il nodo non può esser raggiunto da nessun token!
      if(length(queryRes) == 0 ) {
        return( list( "status"="error","msg"=paste(c(arrayStatus[stato.ID],' cannot be reached from any node!'),collapse='')) )
      }
      
      # Bene, ci sono dei results: verifica allora che la lista dei token che possono attivare il 'fire' sia 
      # effettivamente attiva
      for( preTokenXML.ID in queryRes) {
        preTokenXML.name<-listaXML$xml$graphStructure$links[[preTokenXML.ID]]["from"]
        
        if(!(preTokenXML.name %in% tokenDisponibili)) {
          return( list( "status"="error","msg"=paste(c(arrayStatus[stato.ID],' has not satisfied all the needed token'),collapse='')) )            
        }
        #  consuma i token necessari all'attivazione          
        tokenDisponibili<- tokenDisponibili[  which(tokenDisponibili!=preTokenXML.name) ]  
      }
      
      # Se sono qui significa che i constraints sui token necessari sono soddisfatti.
      # Cerca i token consguenti allo stato da "sparare"
      # cerca tutti i prerequisiti per lo stato corrente 'arrayStatus[stato.ID]'
      queryRes<-getListaXML.link( stringa = arrayStatus[stato.ID], listaXML = listaXML, what2Get = "from")
      # se è a lunghezza '0' significa che non è in grado di attivare alcun TOKEN!
      if(length(queryRes) == 0 ) {
        return( list( "status"="error","msg"=paste(c(arrayStatus[stato.ID],' has no nodes to activate!'),collapse='')) )
      }      
      
      # Bene, ci sono dei results: Aggiungi allora ogni TOKEN estratto all'array dei TOKEN che attivi
      for( postTokenXML.ID in queryRes) {
        postTokenXML.name<-listaXML$xml$graphStructure$links[[postTokenXML.ID]]["to"]
        tokenDisponibili<- c(tokenDisponibili, postTokenXML.name)
      }      
      # togli i duplicati, per favore
      tokenDisponibili<-unique(tokenDisponibili);
      
      # Ottimo, ora verifica: c'è il TOKEN in END??? Se sì, chiudi l'elaborazione
      if ( "END" %in% tokenDisponibili) {
        if( stato.ID != length(arrayStatus) ) 
          return( list( "status"="error","msg"=paste(c('Reached the END with other words to parse'),collapse='')) )
        else
          return( list( "status"="ok","msg"=paste(c('the word \"',arrayStatus,'\" is a word of the grammar'),collapse='')) )
      }
    }
    return( list() )
  }  
  new.replay<-function( arrayStatus , breakOn="error") {
    # fai una copia del modello XML, giusto per non correre 
    # il rischio di sminchiare l'attributo
    strutturaXML<-XML.model
    
    # setta il primo token: BEGIN
    tokenDisponibili<- c("BEGIN")
    cat("\n1")
    browser();
    # cicla su tutti gli stati della "parola"
    for( stato.ID in seq(1,length(arrayStatus))) {
      stato.Name <- arrayStatus[ stato.ID ]
      
      # cerca tutti i prerequisiti per lo stato corrente 'arrayStatus[stato.ID]'
      stringaQuery<-paste( c('/Main/xml/graphStructure/links/link[@to="',arrayStatus[stato.ID],'"]'), collapse='') 
      cat("\n2.1")
      queryRes<-getNodeSet(strutturaXML,stringaQuery)
      cat("\n2.2")
      # se è a lunghezza '0' significa che il nodo non può esser raggiunto da nessun token!
      if(length(queryRes) == 0 ) {
        return( list( "status"="error","msg"=paste(c(arrayStatus[stato.ID],' cannot be reached from any node!'),collapse='')) )
      }
      
      # Bene, ci sono dei results: verifica allora che la lista dei token che possono attivare il 'fire' sia 
      # effettivamente attiva
      for( preTokenXML.ID in seq(1,length(queryRes))) {
          cat("\n3.1")
          preTokenXML.name<-xmlGetAttr(queryRes[[preTokenXML.ID]],"from")   
          cat("\n3.2")
          if(!(preTokenXML.name %in% tokenDisponibili)) {
              return( list( "status"="error","msg"=paste(c(arrayStatus[stato.ID],' has not satisfied all the needed token'),collapse='')) )            
          }
          #  consuma i token necessari all'attivazione          
          tokenDisponibili<- tokenDisponibili[  which(tokenDisponibili!=preTokenXML.name) ]  
      }
      
      # Se sono qui significa che i constraints sui token necessari sono soddisfatti.
      # Cerca i token consguenti allo stato da "sparare"
      # cerca tutti i prerequisiti per lo stato corrente 'arrayStatus[stato.ID]'
      stringaQuery<-paste( c('/Main/xml/graphStructure/links/link[@from="',arrayStatus[stato.ID],'"]'), collapse='') 
      cat("\n4.1")
      queryRes<-getNodeSet(strutturaXML,stringaQuery)     
      cat("\n4.2")
      # se è a lunghezza '0' significa che non è in grado di attivare alcun TOKEN!
      if(length(queryRes) == 0 ) {
        return( list( "status"="error","msg"=paste(c(arrayStatus[stato.ID],' has no nodes to activate!'),collapse='')) )
      }      
      
      # Bene, ci sono dei results: Aggiungi allora ogni TOKEN estratto all'array dei TOKEN che attivi
      for( postTokenXML.ID in seq(1,length(queryRes))) {
        cat("\n5.1")
        postTokenXML.name<-xmlGetAttr(queryRes[[postTokenXML.ID]],"to")   
        cat("\n5.2")
        tokenDisponibili<- c(tokenDisponibili, postTokenXML.name)
      }      
      # togli i duplicati, per favore
      tokenDisponibili<-unique(tokenDisponibili);
      
      # Ottimo, ora verifica: c'è il TOKEN in END??? Se sì, chiudi l'elaborazione
      if ( "END" %in% tokenDisponibili) {
        return( list( "status"="ok","msg"=paste(c('the word \"',arrayStatus,'\" is a word of the grammar'),collapse='')) )
      }
    }

    return( list() )
  }
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    XML.model<<-''
  }
  costructor();
  #================================================================================= 
  return(list(
    "loadModel.xml"=loadModel.xml,
    "replay"=replay
  ))
}