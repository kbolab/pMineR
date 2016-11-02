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
confCheck_easy<-function( verbose.mode = TRUE ) {
  WF.xml <- c()                 # The XML with the WF
  WF.xml.fileName <- c()        # Just the XML filename
  dataLog <- c()                # The data structure with the LOGs
  WF.struct <- list()           # the WF structure
  notebook <- list()            # internal notebook for events-log
  tmpAttr <- list()
  
  param.verbose <- c()
  
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
    
    # Per ogni stato carica gli attributi (ad es. 'plotIt')
    for(state.name in array.stati) {
      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      st.type<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"type")[[1]]
      
      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "") 
      
      if(length(st.type)==0) st.type="normal"
      else st.type = str_replace_all(string = st.type,pattern = "'",replacement = "") 
      
      # Carica quanto indicato nell'XML nella variabile che poi andrà copiata 
      # negli attributi globali
      lista.stati[[ state.name ]]<-list()
      lista.stati[[ state.name ]][["plotIt"]]<-plotIt
      lista.stati[[ state.name ]][["type"]]<-st.type
    }    
    
    # Per ogni trigger, carica la 'condition', i 'set' e gli 'unset'  
    # (e domani, pure altro)
    for(trigger.name in array.trigger) {
      condizione<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/condition'),collapse = ""),xmlValue)[[1]]
      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      arr.set<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/set'),collapse = ""),xmlValue)
      arr.unset<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/unset'),collapse = ""),xmlValue)
      
      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "")
      
      # Carica quanto indicato nell'XML nella variabile che poi andrà copiata 
      # negli attributi globali      
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
    
    # Chiama addNote, che via via popola una stringa 
    # che alla fine conterrà l'intero XML
    ct<-1
    addNote(msg = "\n<xml>")
    
    # Per ogni paziente
    for( indice in names(dataLog$wordSequence.raw)) {
      
      if(dim(dataLog$pat.process[[ indice ]])[1]>0) {
        
        if(param.verbose == TRUE) cat("\n Doing:",indice)
        
        addNote(msg = str_c("\n\t<computation n='",ct,"' IDPaz='",indice,"'>"))
        # res <- playSingleSequence( sequenza = dataLog$wordSequence.raw[[ indice ]]  )
        if(param.verbose == TRUE) cat(str_c("\nBeginning Pat ",indice,"..."))
        res <- playSingleSequence( matriceSequenza = dataLog$pat.process[[ indice ]], col.eventName = dataLog$csv.EVENTName, col.dateName = dataLog$csv.dateColumnName , IDPaz = indice  )
        if(param.verbose == TRUE) cat(str_c("\nPat ",indice," done;"))
        addNote(msg = "\n\t\t<atTheEnd>")
        for(i in res$st.ACTIVE) addNote(msg = str_c("\n\t\t\t<finalState name=",i,"></finalState>"))
        for(i in res$last.fired.trigger) addNote(msg = str_c("\n\t\t\t<last.fired.trigger name='",i,"'></last.fired.trigger>"))
        addNote(msg = "\n\t\t</atTheEnd>")
        addNote(msg = "\n\t</computation>")
        ct <- ct + 1
        if(   (ct/length(dataLog$wordSequence.raw)) > number.perc  ) break;
        
      }
      
    }
    # Chiudi l'XML
    addNote(msg = "\n</xml>")
  }   
  #===========================================================  
  # playLoadedData
  # Cazzo! Non ricordo più nemmeno io cosa fa questa funzione.....
  #===========================================================      
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
  playSingleSequence<-function( matriceSequenza , col.eventName, col.dateName, IDPaz) {
    
    # Cerca lo stato che viene triggerato dal BEGIN
    st.LAST<-""
    st.DONE<-c("")
    st.ACTIVE<-c("'BEGIN'")
    last.fired.trigger<-c()
    ct <- 0; riga <- 0
    error<-""
    
    sequenza <- as.array(matriceSequenza[ ,col.eventName ])
    
    # Analizza TUTTI gli eventi della sequenza
    for( ev.NOW in sequenza ) {
      if(param.verbose == TRUE) cat(str_c("\n\t processing:",ev.NOW))
      # costruisco un contatore della riga della tabella in analisi
      riga <- riga + 1
      data.ev.NOW <- matriceSequenza[ riga ,col.dateName ]
      ct <- ct + 1
      # gestisci il log
      newNote();
      note.setStep(number = ct)
      note.setEvent(eventType = ev.NOW, eventDate = data.ev.NOW)
      note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
      
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = ev.NOW, st.DONE = st.DONE, st.ACTIVE = st.ACTIVE, EOF = FALSE   )
      
      # Se c'e' un errore, ferma tutto
      if(newHop$error==TRUE) {
        note.set.error(error = newHop$error)
        note.flush()
        return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger , "date" = data.ev.NOW   ) );
      }
      
      # Se hai rilevato dei trigger attivi
      if(length(newHop$active.trigger)!=0) {
        note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
        st.ACTIVE <- newHop$st.ACTIVE
        last.fired.trigger<-newHop$active.trigger
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
        
        newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = "", st.DONE = st.DONE, st.ACTIVE = st.ACTIVE, EOF = FALSE   )
        
        # inzializza il log in caso di errore o in caso di trigger
        if(newHop$error==TRUE | length(newHop$active.trigger)!=0) {
          ct <- ct + 1
          newNote();
          note.setStep(number = ct)
          note.setEvent(eventType = '', eventDate = data.ev.NOW)
          note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
        }
        
        # Se c'e' un errore, ferma tutto
        if(newHop$error==TRUE) {
          note.set.error(error = newHop$error)
          note.flush()
          return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger"=last.fired.trigger, "date" = data.ev.NOW  ) )
        }
        
        # Se hai rilevato qualche trigger attivo
        if(length(newHop$active.trigger)!=0)  {
          note.setEvent(eventType = '', eventDate = data.ev.NOW)
          note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
          note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
          note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
          st.ACTIVE <- newHop$st.ACTIVE
          last.fired.trigger<-newHop$active.trigger
          # e fai il flush
          note.flush()          
        }
        # altrimenti (se non ci sono stati trigger, vedi di uscire dal loop)
        else devo.restare.in.trigger.loop<-FALSE
      }
    }
    
    # Now process the EOF !!
    ct <- ct + 1
    # gestisci il log
    newNote();
    note.setStep(number = ct)
    note.setEvent(eventType = ev.NOW, eventDate = data.ev.NOW)
    note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
    
    # Cerca chi ha soddisfatto le precondizioni
    newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = '', st.DONE = st.DONE, st.ACTIVE = st.ACTIVE, EOF = TRUE  )
    
    # Se c'e' un errore, ferma tutto
    if(newHop$error==TRUE) {
      note.set.error(error = newHop$error)
      note.flush()
      return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger , "date" = data.ev.NOW ) );
    }
    
    # Se hai rilevato dei trigger attivi
    if(length(newHop$active.trigger)!=0) {
      note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
      note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
      st.ACTIVE <- newHop$st.ACTIVE
      last.fired.trigger<-newHop$active.trigger
    } else { 
      # altrimenti segnala che NON ci sono trigger attivi
      note.set.fired.trigger(array.fired.trigger = '')
      # E i nuovi stati validi sono esattamente i vecchi
      note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = st.ACTIVE)
    }
    # Fai il flush 
    note.flush()    
    
    # Ritorna
    return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger, "date" = data.ev.NOW ) );
  }  
  #===========================================================  
  # attiva.trigger
  # is the "core": it finds out the trigger that should be fired
  #===========================================================    
  attiva.trigger<-function( st.LAST, ev.NOW, st.DONE, st.ACTIVE, EOF  ) {
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
      stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$EOF\\$",replacement = str_c("'",EOF,"'") )
      
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
  # plotPatientEventTimeLine
  # plot the event timeline for a ginven patient
  #===========================================================  
  plotPatientEventTimeLine<-function( patientID ) {
    
    st.POST<-list(); st.PRE<-list(); tr.fired<-list()
    txt.section<-"";
    
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.step <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"evt"))
    arr.date <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"date"))
    
    matrice<-c()
    for( riga in seq(1,length(arr.step))) {
      matrice <- rbind(matrice, cbind(  arr.date[ riga ] , arr.step[ riga ]  ) )
    }
    
    plotTimeline(matrice, format.date = "%d/%m/%Y")
  }  
  #===========================================================  
  # plotPatientComputedTimeline
  # plot the computed timeline for a given patient
  #===========================================================   
  plotPatientComputedTimeline<-function( patientID ) {  
    
    st.POST<-list(); st.PRE<-list(); tr.fired<-list()
    txt.section<-"";
    
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.step <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"n"))
    arr.date <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"date"))
    for( i in seq(1,length(arr.step)) ) {
      st.POST[[ arr.step[i] ]]<-unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step[@n="',arr.step[i],'"]/st.ACTIVE.POST'),xmlGetAttr,"name"))
      st.PRE[[ arr.step[i] ]]<-unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step[@n="',arr.step[i],'"]/st.ACTIVE.PRE'),xmlGetAttr,"name"))
      tr.fired[[ arr.step[i] ]]<-unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step[@n="',arr.step[i],'"]/fired.trigger'),xmlGetAttr,"name"))
    }
    # prendi solo le date relative ai post/trigger attivati
    arr.date<-arr.date[as.numeric(names(st.POST))]
    
    # prendi gli step da scorrere
    step.da.scorrere <- names(st.POST)
    
    num_section <- 1
    
    for( section in unlist(st.POST)) {
      # Clear date fields
      from.date <- "";    to.date <- "";  sec.run<-1;
      is.begining <- FALSE; is.ending <- FALSE;
      headLine <- str_c("\n section ",section)
      line.to.write<-"";
      # run for each computation step
      for( step2Run in seq(1,length(step.da.scorrere))){
        
        step2Run.index <- step.da.scorrere[step2Run]
        # figure out if it is a begin or an end and set the dates
        is.begining <- section %in% st.POST[[ step2Run.index ]] & !(section %in% st.PRE[[ step2Run.index ]] )
        is.ending <- section %in% st.PRE[[ step2Run.index ]] & !(section %in% st.POST[[ step2Run.index ]] )
        if(is.begining == TRUE) from.date <- arr.date[step2Run]
        if(is.ending == TRUE) to.date <- arr.date[step2Run]
        
        if( is.ending ) {
          days.delta.date<-as.character(  as.integer( difftime(as.POSIXct(to.date, format = "%d/%m/%Y"),as.POSIXct(from.date, format = "%d/%m/%Y"),units = 'days')  ))
          line.to.write <- str_c(line.to.write,"\n\t ",days.delta.date," days             :active,          ",section,",    ",from.date,", ",to.date)
          # reset the flags
          is.begining <- FALSE;  is.ending <- FALSE; sec.run <- sec.run+1
        }
      }
      
      if(sec.run>0) txt.section <- str_c(txt.section,"\n", headLine , line.to.write)
      num_section <- num_section + 1 
    }
    
    aaa<- str_c(" gantt
                dateFormat  DD/MM/YYYY
                title time-event for Patient: ",patientID,"
                
                ",txt.section)
    
    mermaid(aaa)    
  }    
  #===========================================================  
  # plotComputationResult
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  # 'avoidFinalStates' is an array containing the final states 
  #                to filter the computation
  # kindOfNumber : i numeri: 'relative' o 'absolute'
  #===========================================================   
  plotComputationResult<-function( whatToCount='activations' ,     kindOfNumber='relative', 
                                   avoidFinalStates=c(), avoidTransitionOnStates=c(), avoidToFireTrigger=c(), whichPatientID=c("*") ) {
    
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
        }
      }
    }
    
    # Distingui fra nodi end e nodi normali (questione di colore)
    arr.terminazioni.raggiungibili <- arr.nodi.end[arr.nodi.end %in% arr.stati.raggiungibili]
    arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]
    
    # Ora sistema le froceries grafiche
    # PER I NODI
    stringa.stati<-"node [fillcolor = Orange]"
    for(nome.stato in arr.stati.raggiungibili)  {
      nome.stato.pulito <- str_replace_all(string = nome.stato,pattern = "'",replacement = "")
      aa = giveBackComputationCounts(nomeElemento = nome.stato.pulito, tipo='stato', whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID)
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
      penwidth<- 1
      colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
      if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
      else numberToPrint<-str_c("# ",aa$howMany)
      stringa.stati <- str_c(stringa.stati,"\n\t ",nome.stato," [label = '",nome.stato.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"',  pencolor='Gray",colore,"']") 
    }
    # PER I TRIGGER
    lista.freq.trigger<-list()
    stringa.trigger<-"node [fillcolor = white, shape = box ]"
    for(nome.trigger in arr.trigger.rappresentabili)  {
      nome.trigger.pulito <- str_replace_all(string = nome.trigger,pattern = "'",replacement = "")
      aa = giveBackComputationCounts(nomeElemento = nome.trigger.pulito, tipo='trigger', whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID )
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      lista.freq.trigger[[nome.trigger]]<-(aa$howMany  / aa$totalNumber)
      penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
      penwidth<- 1
      colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
      if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
      else numberToPrint<-str_c("# ",aa$howMany)
      stringa.trigger <- str_c(stringa.trigger,"\n\t ",nome.trigger," [label = '",nome.trigger.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"', fontcolor='Gray",colore,"']") 
    }    
    # STRINGA NODO FROM (ARCO)
    stringa.nodo.from<-"\nedge [arrowsize = 1 ]"
    for(i in seq(1,nrow(matrice.nodi.from))) {
      val.perc<-lista.freq.trigger[[ str_c("'",matrice.nodi.from[i,2],"'") ]]
      arrowsize<- .5 + 7 * val.perc 
      colore <- as.integer(100-(30+val.perc*70))
      labelArco <- str_c(round(val.perc*100,2),"%")
      labelArco<-''
      nuovaRiga<-str_c("\n\t",matrice.nodi.from[i,1],"->'",matrice.nodi.from[i,2],"' [label = '",labelArco,"', penwidth='",arrowsize,"', fontcolor='Gray",colore,"', pencolor='Gray",colore,"'  ]")
      stringa.nodo.from<-c(stringa.nodo.from,nuovaRiga)
    }
    
    # STRINGA NODO TO (ARCO)
    stringa.nodo.to<-"\nedge [arrowsize = 1 ]"
    for(i in seq(1,nrow(matrice.nodi.to))) {
      val.perc<-lista.freq.trigger[[ str_c("'",matrice.nodi.to[i,1],"'") ]]
      arrowsize<- .5 + 7 * val.perc      
      colore <- as.integer(100-(30+val.perc*70))
      labelArco <- str_c(round(val.perc*100,2),"%")
      labelArco<-''
      nuovaRiga<-str_c("\n\t'",matrice.nodi.to[i,1],"'->",matrice.nodi.to[i,2]," [label = '",labelArco,"', penwidth='",arrowsize,"', fontcolor='Gray",colore,"', pencolor='Gray",colore,"' ]")
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
  # INPUT
  # tipo = 'stato' o 'trigger' in funzione di cosa sia
  # whatToCount = 'activations'  se si vogliono contare le attivazioni,
  #               'terminations' se si vogliono contare quante volte vi è terminato
  # avoidFinalStates = un array che contiene gli stati che non devono essere 
  #                    finali per i pazienti da considerare (serve per fare un filtro)
  # avoidTransitionOnStates = un array che indica gli stati che non devono essre
  #                           transitati dai pazienti da considerare (è un filtro)
  # avoidToFireTrigger = badalì, indovina un po'?
  #===========================================================  
  giveBackComputationCounts<-function( nomeElemento, tipo, whatToCount, avoidFinalStates, avoidTransitionOnStates, avoidToFireTrigger , whichPatientID) {
    # Carica l'XML
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))
    totalAmount <- 0 
    totalNumber <- 0
    
    # Loopa per ogni computazione
    for(i in seq(1,length(arr.Computazioni))) {
      skipComputation <- FALSE
      
      # Differenzia il caso in cui si vogliano contare gli stati da quello in cui si vogliano 
      # contare i trigger
      st.FINAL.arr <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/atTheEnd/finalState'),xmlGetAttr,"name"))
      st.TRANSITION.ON.arr <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/st.ACTIVE.POST'),xmlGetAttr,"name"))      
      tr.fired.arr <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/fired.trigger'),xmlGetAttr,"name"))
      PatientID <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]'),xmlGetAttr,"IDPaz"))
      
      if( !("*" %in% whichPatientID) & !(PatientID %in% whichPatientID)){
        next;
      }
      
      if(tipo=="stato") {
        if(whatToCount=='activations') howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/st.ACTIVE.POST[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
        if(whatToCount=='terminations') howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/atTheEnd/finalState[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
      }
      if(tipo=="trigger") {
        if(whatToCount=='activations') howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/fired.trigger[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
        if(whatToCount=='terminations')  howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/atTheEnd/last.fired.trigger[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
      }
      
      # Verifica se il finale è incluso in quelli da scartare: se sì, skippa tutto il paziente 
      # (la computazione corrente)
      if(sum(st.FINAL.arr %in% avoidFinalStates) >=1 | 
         sum(st.TRANSITION.ON.arr %in% avoidTransitionOnStates) >=1 |
         sum(tr.fired.arr %in% avoidToFireTrigger) >=1 )  {
        skipComputation<-TRUE;
      }
      
      if(skipComputation == TRUE) next;
      
      # Se e' null, metti a zero
      if(length(howMany)==0) howMany<-0
      else howMany<-1;
      
      totalAmount <- totalAmount + howMany
      totalNumber <- totalNumber + 1
    }
    return( list( "howMany"=totalAmount,  "totalNumber"=totalNumber)  ) 
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
    tmpAttr$event.date<<-""
  }
  note.setStep<-function( number ){
    tmpAttr$stepNumber <<- number
  }  
  note.setEvent<-function( eventType , eventDate){
    tmpAttr$event <<- eventType
    tmpAttr$event.date <<- eventDate
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
    testo<-str_c("\n\t\t<step n='",tmpAttr$stepNumber,"' trg='",tmpAttr$boolean.fired.trigger,"' evt='",tmpAttr$event,"' date='",tmpAttr$event.date,"'>")
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
  costructor<-function( verboseMode ) {
    WF.xml <<- c()
    WF.xml.fileName <<- c()
    dataLog <<- c()
    WF.struct <<- list()
    notebook <<- list()
    tmpAttr <<- list()
    param.verbose <<- verboseMode
  }
  costructor( verboseMode = verbose.mode);
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
    "plotPatientComputedTimeline" = plotPatientComputedTimeline,
    "getPatientXML"=getPatientXML,
    "plotPatientEventTimeLine" = plotPatientEventTimeLine
  ))
}

