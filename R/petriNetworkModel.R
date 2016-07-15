petriNetworkModel<-function() {
  XML.model<-''
  obj.log<-''
  
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
  play<-function( ) {

    array.stati.fired<-c( ) 
    
    # fai una copia del modello XML, giusto per non correre 
    # il rischio di sminchiare l'attributo (e anche perche' senno' mi ritrovo il 
    # bacherozzo che non so' come affrontare)
    listaXML<-xmlToList( node =  XML.model )    

    m.link<-c(); m.tipoNodo<-c()
    for(indiceLink in seq(1,length(listaXML$xml$graphStructure$links))) {
      m.link<-rbind(m.link,c( listaXML$xml$graphStructure$links[[indiceLink]] ))
    }
    colnames(m.link)<-c("from","to")
    for(indiceNodo in seq(1,length(listaXML$xml$graphStructure$nodeList))) {
      m.tipoNodo<-rbind(m.tipoNodo,c( listaXML$xml$graphStructure$nodeList[[indiceNodo]] ))
    }    
    m.link<-cbind( m.link , rep(0,nrow(m.link)), rep(0,nrow(m.link)) )
    for( i in seq(1,nrow(m.link)) ) {
      if( length(which(m.tipoNodo[,1]==m.link[i,1])) >0 ) {
        m.link[i,3]<-m.tipoNodo[  which(m.tipoNodo[,1]==m.link[i,1]) ,2 ]
        m.link[i,4]<-m.tipoNodo[  which(m.tipoNodo[,1]==m.link[i,2]) ,2 ]
      }
    }
    colnames(m.link)<-c("from","to","tipoFrom","tipoTo")    

    # Ora cicla sui token per fare il play vero e proprio
    tokenAttuali<-c("BEGIN");    canBeSpent<-TRUE    ;
    ct<-1
    # cicla fino a che ci sono token e fino a che non c'e' un token in END
    while(canBeSpent == TRUE   ) {
      # elenca i possibili stati destinazione
      to.possibili<-unique(m.link[ which(m.link[,"tipoTo"]=="state"),   "to" ])
      # per ogni stato estrai la lista dei tokenPlace necessari
      listaNecessari<-list()
      for ( statoDaVerificare in to.possibili ) {
        listaNecessari[[statoDaVerificare]]<-m.link[  which(m.link[,"to"] == statoDaVerificare), "from"]
      }
      # bene, fra essi vediamo ora quale puo' essere "fired"!
      array.possibili.fire<-c()
      for( nomeStato  in names(listaNecessari) ) {
        quantiSoddisfatti<-sum(listaNecessari[[nomeStato]] %in% tokenAttuali  )
        if(quantiSoddisfatti == length(listaNecessari[[nomeStato]])) array.possibili.fire<-c(array.possibili.fire,nomeStato) 
      }      
      # ho in 'array.possibili.fire' gli stati che si potrebbero lanciare. Ne piglio uno a caso e provvedo
      lancio.di.dado<-as.integer(runif(n = 1,min = 1,max = (length(quantiSoddisfatti)+1)  ) )
      
      stato.fired<-array.possibili.fire[ lancio.di.dado ]
      if ( length(array.possibili.fire)  == 0 ) {
        canBeSpent = FALSE
        obj.log$sendLog( msg ='Almeno un token e\' restato in deadlock' , type='WRN')
      }
      else {
        # aggiorna la lista di stati fired
        array.stati.fired<-c(array.stati.fired, stato.fired ) 
  #      if(ct==3) browser()
        # A questo punto consumo i token coinvolti
        token.rimasti<- tokenAttuali[! (tokenAttuali%in% listaNecessari[[stato.fired]]  )]
        # ed aggiungo i nuovi token (i conseguenti)
        token.da.aggiungere<-m.link[ which(m.link[,"from"] == stato.fired),"to"  ]
        # aggiorno i token attuali
        tokenAttuali<-unique( c(token.rimasti,token.da.aggiungere)  )
        
        if(length(tokenAttuali) == 0) {
          canBeSpent<-FALSE
          obj.log$sendLog( msg ='Token Esauriti' , type='WRN')
        }
      }
    }
    
    # niente semilavorati: se non e' arrivato in fondo, cassa
    if( canBeSpent == FALSE) array.stati.fired<-c()
    if (!("END" %in% tokenAttuali  ) ) {
      obj.log$sendLog( msg ='A fine computazione non c\'e\' un token in END' , type='WRN')
    }
    
    return(array.stati.fired)
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

      # se e' a lunghezza '0' significa che il nodo non puo' esser raggiunto da nessun token!
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
      # se e' a lunghezza '0' significa che non e' in grado di attivare alcun TOKEN!
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
      
      # Ottimo, ora verifica: c'e' il TOKEN in END??? Se si', chiudi l'elaborazione
      if ( "END" %in% tokenDisponibili) {
        if( stato.ID != length(arrayStatus) ) 
          return( list( "status"="error","msg"=paste(c('Reached the END with other words to parse'),collapse='')) )
        else
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
    obj.log<<-logHandler();
  }
  costructor();
  #================================================================================= 
  return(list(
    "loadModel.xml"=loadModel.xml,
    "replay"=replay,
    "play" = play
  ))
}