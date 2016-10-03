#' a conformance checking module
#' 
#' @description  A first module for conformance checking
#' @import stringr XML            
#' @export
#' @examples \dontrun{
#' 
#' obj.L<-dataLoader();   # create a Loader
#' 
#' # Load a .csv using "DES" and "ID" as column names to indicate events 
#' # and Patient's ID
# obj.L$load.csv(nomeFile = "./otherFiles/test_02.csv",IDName = "ID",
# EVENTName = "DES")
#'
#' #  Create a Conformance Checker obj
#' obj.cc <- confCheck_easy()
#' 
#' # Load an XML with the workflow to check
#' obj.cc$loadWorkFlow( WF.fileName='../otherFiles/import_01/FranciWF.xml' )
#' 
#' # load data into Conformance Checker obj
#' obj.cc$loadDataset( obj.L$getData() );
#' 
#' }
confCheck_easy<-function() {
  WF.xml <- c()                 # The XML with the WF
  WF.xml.fileName <- c()        # Just the XML filename
  dataLog <- c()                # The data structure with the LOGs
  WF.struct <- list()           # the WF structure
  notebook <- list()            # internal notebook for events-log
  tmpAttr <- list()

  #=================================================================================
  # clearAttributes
  # If we want to 'reset' the obj, this can clear all the attributes
  #=================================================================================    
  clearAttributes<-function() {
    costructor();
  }
  #=================================================================================
  # loadWorkFlow
  # It allows to load an XML with a WF
  #=================================================================================   
  loadWorkFlow<-function( WF.fileName ) {
    WF.xml <<- xmlInternalTreeParse( WF.fileName )
    WF.xml.fileName <<- WF.fileName
    
    # dopo aver caricato l'XML negli attributi, costruisci la struttura in memoria 
    build.Workflow.model()
  }
  #===========================================================  
  # build.Workflow.model
  # it parses the XML to build in memory the WF model
  #===========================================================    
  build.Workflow.model<-function() {
    lista.trigger<-list()
    lista.stati<-list()
    
    # carica la lista degli stati e dei triggers    
    array.stati<-unlist(xpathApply(WF.xml,'//xml/workflow/node',xmlGetAttr, "name"))
    array.trigger<-unlist(xpathApply(WF.xml,'//xml/workflow/trigger',xmlGetAttr, "name"))

    # Per ogni stato carica gli attributi (ad es. se plottabile)
    for(state.name in array.stati) {
      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      st.type<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"type")[[1]]
      
      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "") 

      if(length(st.type)==0) st.type="normal"
      else st.type = str_replace_all(string = st.type,pattern = "'",replacement = "") 
      
      lista.stati[[ state.name ]]<-list()
      lista.stati[[ state.name ]][["plotIt"]]<-plotIt
      lista.stati[[ state.name ]][["type"]]<-st.type
    }    
    
    # Per ogni trigger, carica la condizione, i set e gli unset  
    for(trigger.name in array.trigger) {
      condizione<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/condition'),collapse = ""),xmlValue)[[1]]
      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      arr.set<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/set'),collapse = ""),xmlValue)
      arr.unset<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/unset'),collapse = ""),xmlValue)
      
      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "")

      lista.trigger[[ trigger.name ]]<-list()
      lista.trigger[[ trigger.name ]][["condition"]]<-condizione
      lista.trigger[[ trigger.name ]][["set"]]<-arr.set
      lista.trigger[[ trigger.name ]][["unset"]]<-arr.unset
      lista.trigger[[ trigger.name ]][["plotIt"]]<-plotIt
    }
    
    # popola l'attributo della classe
    WF.struct[[ "info" ]]<<- list()
    WF.struct[[ "info" ]][[ "stati" ]] <<- lista.stati
    WF.struct[[ "info" ]][[ "trigger" ]] <<- lista.trigger
  }    
  #===========================================================  
  # playLoadedData
  # esegue il conformanche checking con l'insieme dei LOG precedentemente caricati
  #===========================================================    
  playLoadedData<-function( number.perc = 1) {
    ct<-1
    addNote(msg = "\n<xml>")
    for( indice in names(dataLog$wordSequence.raw)) {
      cat("\n Doing:",indice)
      addNote(msg = str_c("\n\t<computation n='",ct,"' IDPaz='",indice,"'>"))
      res <- playSingleSequence( sequenza = dataLog$wordSequence.raw[[ indice ]]  )
      addNote(msg = "\n\t\t<atTheEnd>")
      for(i in res$st.ACTIVE) addNote(msg = str_c("\n\t\t\t<finalState name=",i,"></finalState>"))
      addNote(msg = "\n\t\t</atTheEnd>")
      addNote(msg = "\n\t</computation>")
      ct <- ct + 1
      if(   (ct/length(dataLog$wordSequence.raw)) > number.perc  ) break;
    }
    addNote(msg = "\n</xml>")
  }   
  getPlayedSequencesStat.00<-function( ) {
    list.fired.trigger<-list()
    list.final.states<-list()    
    doc <- xmlInternalTreeParse(file = getXML(),asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))

    for( i in arr.Computazioni) {
      arr.step<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step'),collapse = ""),xmlGetAttr,"n"))
      array.fired.trigger<-c()
      # Scorri tutti gli step di quella computazione
      for( s  in arr.step) {
        trg<-xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step[@n="',s,'"]'),collapse = ""),xmlGetAttr,"trg")[[1]]
        if(trg == "TRUE") {
          # Prendi i trigger attivati
          fired.trigger<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step[@n="',s,'"]/fired.trigger'),collapse = ""),xmlGetAttr,"name"))
          array.fired.trigger<-c( array.fired.trigger , fired.trigger )

          final.states<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step[@n="',s,'"]/st.ACTIVE.POST'),collapse = ""),xmlGetAttr,"name"))
        }
      }
      list.fired.trigger[[i]] <-array.fired.trigger
      list.final.states[[i]] <- final.states

    }

    return(list(
      "list.fired.trigger"=list.fired.trigger,
      "list.final.states"=list.final.states
    ))
  }
  #===========================================================  
  # playSingleSequence
  # esegue il conformanche checking con una specifica sequenza 
  # di LOG (di un paziente)
  #===========================================================     
  playSingleSequence<-function( sequenza ) {
    
    # Cerca lo stato che viene triggerato dal BEGIN
    st.LAST<-""
    st.DONE<-c("")
    st.ACTIVE<-c("'BEGIN'")
    ct <- 0
    error<-""

    # Analizza TUTTI gli eventi della sequenza
    for( ev.NOW in sequenza ) {
      ct <- ct + 1
      # gestisci il log
      newNote();
      note.setStep(number = ct)
      note.setEvent(eventType = ev.NOW)
      note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
      
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = ev.NOW, st.DONE = st.DONE, st.ACTIVE = st.ACTIVE  )
      
      # Se c'e' un errore, ferma tutto
      if(newHop$error==TRUE) {
        note.set.error(error = newHop$error)
        note.flush()
        return( list( "st.ACTIVE"=st.ACTIVE,"error"=error ) );
      }
      
      # Se hai rilevato dei trigger attivi
      if(length(newHop$active.trigger)!=0) {
        note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
        st.ACTIVE <- newHop$st.ACTIVE
      } else { 
        # altrimenti segnala che NON ci sono trigger attivi
        note.set.fired.trigger(array.fired.trigger = '')
        # E i nuovi stati validi sono esattamente i vecchi
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = st.ACTIVE)
      }
      # Fai il flush 
      note.flush()
      
      # Ora ripeti la ricerca dei trigger senza passare alcun evento, giusto per 
      # vedere i trigger che si possono eventualmente attivare a seguito di 
      # pregressi triggers.
      devo.restare.in.trigger.loop<-TRUE
      # Continua a loopare fino a che e' vero che qualche trigger e' scattato
      while(  devo.restare.in.trigger.loop == TRUE  ) {
 
        newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = "", st.DONE = st.DONE, st.ACTIVE = st.ACTIVE  )

        # inzializza il log in caso di errore o in caso di trigger
        if(newHop$error==TRUE | length(newHop$active.trigger)!=0) {
          ct <- ct + 1
          newNote();
          note.setStep(number = ct)
          note.setEvent(eventType = '')
          note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
        }
        
        # Se c'e' un errore, ferma tutto
        if(newHop$error==TRUE) {
          note.set.error(error = newHop$error)
          note.flush()
          return( list( "st.ACTIVE"=st.ACTIVE,"error"=error ) )
        }
        
        # Se hai rilevato qualche trigger attivo
        if(length(newHop$active.trigger)!=0)  {
          note.setEvent(eventType = '')
          note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
          note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
          note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
          st.ACTIVE <- newHop$st.ACTIVE
          # e fai il flush
          note.flush()          
        }
        # altrimenti (se non ci sono stati trigger, vedi di uscire dal loop)
        else devo.restare.in.trigger.loop<-FALSE
      }
    }
    return( list( "st.ACTIVE"=st.ACTIVE,"error"=error ) );
  }  
  #===========================================================  
  # attiva.trigger
  # is the "core": it finds out the trigger that should be fired
  #===========================================================    
  attiva.trigger<-function( st.LAST, ev.NOW, st.DONE, st.ACTIVE  ) {
    # inizializza
    new.st.DONE<-st.DONE;
    new.st.LAST<-c()
    new.st.ACTIVE<-st.ACTIVE
    active.trigger<-c()
    global.array.to.set<-c(); global.array.to.unset<-c();
    errore <- FALSE;   errMsg<-"";
    
    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct[[ "info" ]][[ "trigger" ]]) ) {
      
      # Prendi la condizione
      precondizione <- WF.struct[["info"]][["trigger"]][[trigger.name]]$condition
      stringa.to.eval<-precondizione
      
      # Inizia a costruire la stringa da parsare, rimpiazzando gli array
      rimpiazzo.ev.NOW<-paste( c("'",ev.NOW,"'") ,collapse='');
      rimpiazzo.st.ACTIVE<- paste(c("c(",paste(c(st.ACTIVE),collapse=","),")" ),collapse='')
      
      stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$ev.NOW\\$",replacement = rimpiazzo.ev.NOW)
      stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$st.ACTIVE\\$",replacement = rimpiazzo.st.ACTIVE)
      
      stringa.to.eval<- str_replace_all(string = stringa.to.eval,pattern = " OR ",replacement = " | ")
      stringa.to.eval<- str_replace_all(string = stringa.to.eval,pattern = " AND ",replacement = " & ")
      
      # Parsa la stringa
      if(stringa.to.eval=="") risultato <- TRUE
      else risultato <- eval(expr = parse(text = stringa.to.eval))
      
      # Se la condizione e' soddisfatta, aggiorna le variabili
      if( risultato == TRUE ) {
        array.to.set<-unlist((WF.struct[["info"]][["trigger"]][[trigger.name]]$set))
        array.to.unset<-unlist((WF.struct[["info"]][["trigger"]][[trigger.name]]$unset))
        
        # Prendi la lista dei set
        new.st.ACTIVE <- unique(c(new.st.ACTIVE,array.to.set))
        # E togli eventuali unset
        new.st.ACTIVE <- unique(new.st.ACTIVE[!(new.st.ACTIVE %in% array.to.unset)])
        # st.LAST
        new.st.LAST <- unique(c(new.st.LAST,array.to.set))
        # st.DONE
        new.st.DONE <- unique(c(new.st.DONE,array.to.unset))
        # Trigger attivi
        active.trigger <- c(active.trigger,trigger.name)
        # Aggiorna gli array con i set e gli unset di tutti i trigger
        # (dato che piu' d'uno potrebbe essere attivo
        global.array.to.set <- unique(c(global.array.to.set,array.to.set))
        global.array.to.unset <- unique(c(global.array.to.unset,array.to.unset))
      }
    }
    
    presenza.conflitti.set.unset <- sum(global.array.to.set %in% global.array.to.unset)
    if(presenza.conflitti.set.unset>0) { 
      errMsg <- "ERROR: set e unset conflittuali. L'esito dipende dall'ordine dei trigger e rischia pertanto di essere indipendente dalle desiderata dell'utente";
      errore = TRUE;
    }
    
    # Ritorna i nuovi stati e la lista dei trigger attivati
    return(list(
      "st.ACTIVE"=new.st.ACTIVE,      # lista nuovi stati ACTIVE 
      "st.LAST"=new.st.LAST,          # lista nuovi stati LAST
      "st.DONE"=new.st.DONE,          # lista nuovi stati DONE
      "active.trigger"=active.trigger,# lista dei trigger attivati per il LOG passato
      "error" = errore,
      "errorMsg" = errMsg
    ))
  }  
  #===========================================================  
  # getXML
  # it returns the XML file
  #===========================================================  
  getXML<-function(notebook.name='computationLog'){
    return(notebook[[notebook.name]])
  }
  #===========================================================  
  # getPatientLog
  # it returns the LOG of a single patient
  #===========================================================  
  getPatientLog<-function( patientID ){
    return(dataLog$wordSequence.raw[[patientID]] )
  }  
  #===========================================================  
  # getPatientXML
  # it returns the XML of a single patient
  #===========================================================  
  getPatientXML<-function( patientID ){
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    valore<- xpathApply(doc,paste(c('//xml/computation[@IDPaz="',patientID,'"]'),collapse = ""))[[1]]
    return(valore)
  }    
  #===========================================================  
  # plotGraph
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  #===========================================================   
  plotGraph<-function(  ) {
    arr.st.plotIt<-c("'BEGIN'");  arr.nodi.end<-c()
    arr.stati.raggiungibili<-c();
    arr.trigger.rappresentabili<-c();
    stringa.nodo.from<-c()
    stringa.nodo.to<-c()   
    # Costruisci subito la lista dei nodi plottabili (cosi' non ci penso piu')
    # Faccio anche la lista dei nodi END
    for(nomeStato in names(WF.struct$info$stati)) {
      if( WF.struct$info$stati[[nomeStato]]$plotIt == TRUE) {
        arr.st.plotIt<-c(arr.st.plotIt,str_c("'",nomeStato,"'"))
      }
      if( WF.struct$info$stati[[nomeStato]]$type == 'END') {
        arr.nodi.end<-c(arr.nodi.end,str_c("'",nomeStato,"'"))
      }      
    }
    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct$info$trigger) ) {
      # Se il trigger e' plottabile
     if(WF.struct$info$trigger[[trigger.name]]$plotIt == TRUE) {

       stringa.nodo.from<-str_c( stringa.nodo.from,"\n" )
       stringa.nodo.to<-str_c( stringa.nodo.to,"\n" )
       # Prendi i nodi 'unset' (from)
       arr.nodi.from<-unlist(WF.struct$info$trigger[[trigger.name]]$unset)
       # Prendi i nodi 'set' (to)
       arr.nodi.to<-unlist(WF.struct$info$trigger[[trigger.name]]$set)
       # Considera solo i nodi plottabili (from e to)
       arr.nodi.from <- arr.nodi.from [arr.nodi.from %in% arr.st.plotIt]
       arr.nodi.to <- arr.nodi.to [arr.nodi.to %in% arr.st.plotIt]

       if(length(arr.nodi.to)>0) {
         # Aggiorna l'array degli stati raggiungibili (in generale)
         # e l'array con i nomi dei trigger rappresentabili
         arr.stati.raggiungibili <- unique(c( arr.stati.raggiungibili, arr.nodi.to, arr.nodi.from ))
         arr.trigger.rappresentabili <- c( arr.trigger.rappresentabili, str_c("'",trigger.name,"'") )
         
         # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
         for( st.nome in arr.nodi.from ) {
           stringa.nodo.from<-str_c( stringa.nodo.from," ",st.nome,"->'",trigger.name,"'" )
         }
         for( st.nome in arr.nodi.to ) {
           stringa.nodo.to<-str_c( stringa.nodo.to," ","'",trigger.name,"'->",st.nome )
         }
       }
     }
    }
    
    # Distingui fra nodi end e nodi nnormali (questione di colore)
    arr.terminazioni.raggiungibili <- arr.nodi.end[arr.nodi.end %in% arr.stati.raggiungibili]
    arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]
    
    # browser()
    a<-paste(c("digraph boxes_and_circles {
             
             # a 'graph' statement
             graph [overlap = true, fontsize = 10]
             
             # several 'node' statements
             node [shape = oval,
             fontname = Helvetica,
             style = filled]

             node [fillcolor = green] 
             'BEGIN'; 

             node [fillcolor = red] 
             ",paste(arr.terminazioni.raggiungibili,collapse=" "),"
             
             
             node [fillcolor = orange]
             ",paste(arr.stati.raggiungibili,collapse=" "),"

             node [fillcolor = white, shape = box ]
             ",paste(arr.trigger.rappresentabili,collapse=" "),"
             
             edge [arrowsize = 1 ]
             # several edge
             ",stringa.nodo.from,"
             ",stringa.nodo.to,"
    }"), collapse='') 
    # browser()
    grViz(a);
  }
  #===========================================================  
  # plotComputationResult
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  #===========================================================   
  plotComputationResult<-function( ) {
    arr.st.plotIt<-c("'BEGIN'");  arr.nodi.end<-c()
    arr.stati.raggiungibili<-c();
    arr.trigger.rappresentabili<-c();
    stringa.nodo.from<-c()
    stringa.nodo.to<-c()   
    howMany<-list()
    matrice.nodi.from<-c();    matrice.nodi.to<-c()
    # Costruisci subito la lista dei nodi plottabili (cosi' non ci penso piu')
    # Faccio anche la lista dei nodi END
    for(nomeStato in names(WF.struct$info$stati)) {
      if( WF.struct$info$stati[[nomeStato]]$plotIt == TRUE) {
        arr.st.plotIt<-c(arr.st.plotIt,str_c("'",nomeStato,"'"))
      }
      if( WF.struct$info$stati[[nomeStato]]$type == 'END') {
        arr.nodi.end<-c(arr.nodi.end,str_c("'",nomeStato,"'"))
      }      
    }
    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct$info$trigger) ) {
      # Se il trigger e' plottabile
      if(WF.struct$info$trigger[[trigger.name]]$plotIt == TRUE) {
        
        stringa.nodo.from<-str_c( stringa.nodo.from,"\n" )
        stringa.nodo.to<-str_c( stringa.nodo.to,"\n" )
        # Prendi i nodi 'unset' (from)
        arr.nodi.from<-unlist(WF.struct$info$trigger[[trigger.name]]$unset)
        # Prendi i nodi 'set' (to)
        arr.nodi.to<-unlist(WF.struct$info$trigger[[trigger.name]]$set)
        # Considera solo i nodi plottabili (from e to)
        arr.nodi.from <- arr.nodi.from [arr.nodi.from %in% arr.st.plotIt]
        arr.nodi.to <- arr.nodi.to [arr.nodi.to %in% arr.st.plotIt]
        
        if(length(arr.nodi.to)>0) {
          # Aggiorna l'array degli stati raggiungibili (in generale)
          # e l'array con i nomi dei trigger rappresentabili
          arr.stati.raggiungibili <- unique(c( arr.stati.raggiungibili, arr.nodi.to, arr.nodi.from ))
          arr.trigger.rappresentabili <- c( arr.trigger.rappresentabili, str_c("'",trigger.name,"'") )
          
          # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
          for( st.nome in arr.nodi.from ) {
            stringa.nodo.from<-str_c( stringa.nodo.from," ",st.nome,"->'",trigger.name,"'" )
            matrice.nodi.from <- rbind(matrice.nodi.from,c(st.nome,trigger.name))
          }
          for( st.nome in arr.nodi.to ) {
            stringa.nodo.to<-str_c( stringa.nodo.to," ","'",trigger.name,"'->",st.nome )
            matrice.nodi.to <- rbind(matrice.nodi.to,c(trigger.name,st.nome))
          }
#           matrice.nodi.from <- rbind(matrice.nodi.from,c(st.nome,trigger.name))
#           matrice.nodi.to <- rbind(matrice.nodi.to,c(trigger.name,st.nome))
        }
      }
    }
    
    # Distingui fra nodi end e nodi nnormali (questione di colore)
    arr.terminazioni.raggiungibili <- arr.nodi.end[arr.nodi.end %in% arr.stati.raggiungibili]
    arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]
    
    # Ora sistema le froceries grafiche
    # PER I NODI
    stringa.stati<-"node [fillcolor = Orange]"
    for(nome.stato in arr.stati.raggiungibili)  {
      nome.stato.pulito <- str_replace_all(string = nome.stato,pattern = "'",replacement = "")
      aa = giveBackComputationCounts(nomeElemento = nome.stato.pulito, tipo='stato' )
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
      penwidth<- 1
      colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
      stringa.stati <- str_c(stringa.stati,"\n\t ",nome.stato," [label = '",nome.stato.pulito,"\n",round(as.numeric(howMany),2)," %', penwidth='",penwidth,"',  pencolor='Gray",colore,"']") 
    }
    # PER I TRIGGER
    lista.freq.trigger<-list()
    stringa.trigger<-"node [fillcolor = white, shape = box ]"
    for(nome.trigger in arr.trigger.rappresentabili)  {
      nome.trigger.pulito <- str_replace_all(string = nome.trigger,pattern = "'",replacement = "")
      aa = giveBackComputationCounts(nomeElemento = nome.trigger.pulito,tipo='trigger' )
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      lista.freq.trigger[[nome.trigger]]<-(aa$howMany  / aa$totalNumber)
      penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
      penwidth<- 1
      colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
      stringa.trigger <- str_c(stringa.trigger,"\n\t ",nome.trigger," [label = '",nome.trigger.pulito,"\n",round(as.numeric(howMany),2)," %', penwidth='",penwidth,"', fontcolor='Gray",colore,"']") 
    }    
    # STRINGA NODO FROM (ARCO)
    stringa.nodo.from<-"\nedge [arrowsize = 1 ]"
    for(i in seq(1,nrow(matrice.nodi.from))) {
      val.perc<-lista.freq.trigger[[ str_c("'",matrice.nodi.from[i,2],"'") ]]
      arrowsize<- .5 + 7 * val.perc 
      colore <- as.integer(100-(30+val.perc*70))
      nuovaRiga<-str_c("\n\t",matrice.nodi.from[i,1],"->'",matrice.nodi.from[i,2],"' [label = '",round(val.perc*100,2),"%', penwidth='",arrowsize,"', fontcolor='Gray",colore,"', pencolor='Gray",colore,"'  ]")
      stringa.nodo.from<-c(stringa.nodo.from,nuovaRiga)
    }
      
    # STRINGA NODO TO (ARCO)
    stringa.nodo.to<-"\nedge [arrowsize = 1 ]"
    for(i in seq(1,nrow(matrice.nodi.to))) {
      val.perc<-lista.freq.trigger[[ str_c("'",matrice.nodi.to[i,1],"'") ]]
      arrowsize<- .5 + 7 * val.perc      
      colore <- as.integer(100-(30+val.perc*70))
      nuovaRiga<-str_c("\n\t'",matrice.nodi.to[i,1],"'->",matrice.nodi.to[i,2]," [label = '",round(val.perc*100, 2),"%', penwidth='",arrowsize,"', fontcolor='Gray",colore,"', pencolor='Gray",colore,"' ]")
      stringa.nodo.to<-c(stringa.nodo.to,nuovaRiga)
    }    
      
    
    # browser()
    a<-paste(c("digraph boxes_and_circles {
             
             # a 'graph' statement
             graph [overlap = true, fontsize = 10]
             
             # several 'node' statements
             node [shape = oval,
             fontname = Helvetica,
             style = filled]

             node [fillcolor = green] 
             'BEGIN'; 

             node [fillcolor = red] 
             ",paste(arr.terminazioni.raggiungibili,collapse=" "),"
             
             ",stringa.stati,"

             ",stringa.trigger,"
             
             edge [arrowsize = 1 ]
             # several edge
             ",stringa.nodo.from,"
             ",stringa.nodo.to,"
    }"), collapse='') 
    grViz(a);
  }  
  #===========================================================  
  # giveBackComputationCounts
  # fa la conta di quanto quello 'stato' o 'trigger' e' stato
  # toccato nella computazione.
  # perOgni indica se la conta deve avvenire per ogni istanza 
  # o per al massimo una per ogni paziente
  #===========================================================  
  giveBackComputationCounts<-function( nomeElemento, tipo='stato', perOgni='paziente' ) {
    # Carica l'XML
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))
    totalAmount<- 0 
   
    # Loopa per ogni computazione
    for(i in seq(1,length(arr.Computazioni))) {
      # Differenzia il caso in cui si vogliano contare gli stati da quello in cui si vogliano 
      # contare i trigger
      if(tipo=="stato")
        howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/st.ACTIVE.POST[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
      if(tipo=="trigger")
        howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/fired.trigger[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
      
      # Se e' null, metti a zero
      if(length(howMany)==0) howMany<-0
      else howMany<-1;
      
      totalAmount <- totalAmount + howMany
    }
    return( list( "howMany"=totalAmount,  "totalNumber"=length(arr.Computazioni))  ) 
  }  
  #===========================================================  
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) {
    dataLog <<- dataList
  }  
  #===========================================================  
  # NOTEs
  # this is a BORING serie of functions used to build the XML
  # they are a lot, so I will not comment all of them.
  #=========================================================== 
  newNote<-function( number ){
    tmpAttr<<-list()
    tmpAttr$stepNumber<<-''
    tmpAttr$boolean.fired.trigger<<-FALSE
    tmpAttr$event<<-""
  }
  note.setStep<-function( number ){
    tmpAttr$stepNumber <<- number
  }  
  note.setEvent<-function( eventType ){
    tmpAttr$event <<- eventType
  }
  note.set.st.ACTIVE.PRE<-function( array.st.ACTIVE.PRE ){
    tmpAttr$st.ACTIVE.PRE <<- array.st.ACTIVE.PRE   
  } 
  note.set.st.ACTIVE.POST<-function( array.st.ACTIVE.POST ){
    tmpAttr$st.ACTIVE.POST <<- array.st.ACTIVE.POST   
  }   
  note.set.fired.trigger<-function( array.fired.trigger ){
    if(array.fired.trigger=="''" || array.fired.trigger=="") return()
    tmpAttr$boolean.fired.trigger <<- TRUE
    tmpAttr$fired.trigger <<- array.fired.trigger   
  }   
  note.set.error<-function( error ){
    tmpAttr$error <<- error   
  }   
  note.flush<-function( ){
    testo<-str_c("\n\t\t<step n='",tmpAttr$stepNumber,"' trg='",tmpAttr$boolean.fired.trigger,"' evt='",tmpAttr$event,"'>")
    if(tmpAttr$boolean.fired.trigger==TRUE)  {
      for(i in tmpAttr$st.ACTIVE.PRE) testo<-str_c(testo,"\n\t\t\t<st.ACTIVE.PRE name=",i,"></st.ACTIVE.PRE>")
      for(i in tmpAttr$fired.trigger) testo<-str_c(testo,"\n\t\t\t<fired.trigger name='",i,"'></fired.trigger>")
      for(i in tmpAttr$st.ACTIVE.POST) testo<-str_c(testo,"\n\t\t\t<st.ACTIVE.POST name=",i,"></st.ACTIVE.POST>")
    }
    testo<-str_c(testo,"\n\t\t</step>")
    addNote(msg = testo)
  }   
  addNote<-function( msg, level='1', notebook.name='computationLog' ) { 
    # Crea la posizione, se ancora non c'è
    if(length(notebook)==0) notebook[[notebook.name]]<<-c()
    else {  if( !(notebook.name %in% names(notebook))) notebook[[notebook.name]]<<-c()  }
    # Accoda la nota
    notebook[[notebook.name]]<<-str_c(notebook[[notebook.name]],msg)
    return;
  }  
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    WF.xml <<- c()
    WF.xml.fileName <<- c()
    dataLog <<- c()
    WF.struct <<- list()
    notebook <<- list()
    tmpAttr <<- list()
  }
  costructor();
  #================================================================================= 
  return(list(
    "loadWorkFlow"=loadWorkFlow,
    "loadDataset"=loadDataset,
    "playLoadedData"=playLoadedData,
    "getXML"=getXML,
    "plotGraph"=plotGraph,
    "plotComputationResult"=plotComputationResult,
    "getPlayedSequencesStat.00"=getPlayedSequencesStat.00,
    "getPatientLog"=getPatientLog,
    "getPatientXML"=getPatientXML
  ))
}

# TO DO:
# aggiungere un <semLink> nell'XML, all'interno di <trigger> per legare fra loro dei nodi (link direzionale molti a molti)
# gestire le trap tramite i 'trigger'. ? (invece di fare in <set> può fare altro, oppure può comunque fare il <set> di alcuni nodi  di tipo "error"? )
# Aggiunto controllo set/unset
# Aggiunta esecuzione EVENTO ''