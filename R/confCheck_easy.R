#' A simple conformance checking class
#' 
#' @description  A first module for making conformance checking
#' @import stringr XML DiagrammeR lubridate     
#' @param verbose.mode boolean. If TRUE some messages will appear in console, during the computation; otherwise the computation will be silent.
#' @export
#' @examples \dontrun{
#' 
#' #  Create a Conformance Checker obj
#' obj.cc <- confCheck_easy()
#' 
#' # Load an XML with the workflow to check
#' obj.cc$loadWorkFlow( WF.fileName='../otherFiles/import_01/rules.v2.xml' )
#' 
#' # plot the graph related to the XML
#' obj.cc$plot()
#' 
#' # now play 20 processes, 10 correct and 10 mismatchful 
#' # (matching and not matching can be seen thanks to the 'valido' column)
#' aaa <- obj.cc$play(number.of.cases = 20,min.num.of.valid.words = 10)
#' 
#' # Build a dataLoaderObject
#' objDL <- dataLoader()
#' 
#' # load the previously genearated data.frame
#' objDL$load.data.frame(mydata = aaa$valid.data.frame,IDName = "patID",
#' EVENTName = "event",dateColumnName = "date")
#' 
#' # now load the data into the obj
#' obj.cc$loadDataset(dataList = objDL$getData())
#' # replay the loaded data
#' obj.cc$replay()
#' 
#' # plot the result, showing the terminations in absolute values
#' obj.cc$plot.replay.result(whatToCount = "terminations",
#' kindOfNumber = "absolute")
#' # plot the result, showing the transitions in relative values
#' obj.cc$plot.replay.result(whatToCount = "activations",
#' kindOfNumber = "relative")
#' 
#' # get the XML of the replay
#' xmlText <- obj.cc$get.XML.replay.result()
#' # or the same data in form of list
#' list.result <- obj.cc$get.list.replay.result()
#' 
#' # plot the timeline of the first patient
#' # and the timeline computed during the re-play
#' obj.cc$plotPatientEventTimeLine(patientID = "1")
#' obj.cc$plotPatientReplayedTimeline(patientID = "1")
#' 
#' }
confCheck_easy<-function( verbose.mode = TRUE ) {
  WF.xml <- c()                 # The XML with the WF
  WF.xml.fileName <- c()        # Just the XML filename
  dataLog <- c()                # The data structure with the LOGs
  WF.struct <- list()           # the WF structure
  notebook <- list()            # internal notebook for events-log
  tmpAttr <- list()
  play.output.format.date <- ""
  list.computation.matrix<-list()    # lista che contiene i risultati di una computazione
  
  param.verbose <- c()
  obj.LogHandler<-c()               # gestore dei messaggi
  
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
  loadWorkFlow<-function( WF.fileName=NA, WF.text=NA) {
    if(is.na(WF.fileName) & is.na(WF.text)) stop("\n\n\n ERRORE: o 'WF.fileName o 'WF.text' deve essere diverso da 'NA'")
    
    if(!is.na(WF.fileName)){
      WF.xml <<- xmlInternalTreeParse( WF.fileName )
      WF.xml.fileName <<- WF.fileName
    }
    if(!is.na(WF.text)){
      WF.xml <<- xmlInternalTreeParse(WF.text, asText=TRUE)
      WF.xml.fileName <<- ''
    }    
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
      
      # Carica quanto indicato nell'XML nella variabile che poi andra' copiata 
      # negli attributi globali
      lista.stati[[ state.name ]]<-list()
      lista.stati[[ state.name ]][["plotIt"]]<-plotIt
      lista.stati[[ state.name ]][["type"]]<-st.type
    }    
    
    # Per ogni trigger, carica la 'condition', i 'set' e gli 'unset'  
    # (e domani, pure altro)
    for(trigger.name in array.trigger) {

      condizione.lst<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/condition'),collapse = ""),xmlValue)
      if(length(condizione.lst)>0) condizione<-condizione.lst[[1]]
      else condizione<-NA

      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      arr.set<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/set'),collapse = ""),xmlValue)
      arr.unset<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/unset'),collapse = ""),xmlValue)
      arr.unsetAll<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/unsetAll'),collapse = ""),xmlValue)
      pri<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"pri")[[1]]
      if(is.null(pri)) pri<-0;
      pri <- as.numeric(pri)
      
      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "")
      
      if(length(arr.unsetAll)==0) arr.unsetAll <- FALSE
      else arr.unsetAll <- TRUE
      
      # Carica quanto indicato nell'XML nella variabile che poi andra' copiata 
      # negli attributi globali      
      lista.trigger[[ trigger.name ]]<-list()
      lista.trigger[[ trigger.name ]][["condition"]]<-condizione
      lista.trigger[[ trigger.name ]][["set"]]<-arr.set
      lista.trigger[[ trigger.name ]][["pri"]]<-pri
      lista.trigger[[ trigger.name ]][["unset"]]<-arr.unset
      lista.trigger[[ trigger.name ]][["unsetAll"]]<-arr.unsetAll
      lista.trigger[[ trigger.name ]][["plotIt"]]<-plotIt
    }

    # Costruisci la lista dei nodi 'END'
    arr.nodi.end<-c()
    for(nomeStato in names(lista.stati)) {
      if( lista.stati[[ nomeStato ]]$type == 'END') {
        arr.nodi.end<-c(arr.nodi.end,str_c("'",nomeStato,"'"))
      }      
    }

    # popola l'attributo della classe
    WF.struct[[ "info" ]]<<- list()
    WF.struct[[ "info" ]][[ "stati" ]] <<- lista.stati
    WF.struct[[ "info" ]][[ "trigger" ]] <<- lista.trigger
    WF.struct[[ "info" ]][[ "arr.nodi.end" ]] <<- arr.nodi.end
  }    
  #===========================================================  
  # replay (ex playLoadedData)
  # esegue il conformanche checking con l'insieme dei LOG precedentemente caricati
  #===========================================================    
  replay<-function( number.perc = 1 , event.interpretation = "soft", UM="days") {
    
    # Chiama addNote, che via via popola una stringa 
    # che alla fine conterra' l'intero XML
    ct<-1
    csv.date.format <- dataLog$csv.date.format
    # browser()
    # if(timeUM != "dmy") obj.LogHandler$sendLog(msg = "'timeUM can be only set to 'dmy', in this version of confCheck_easy::replay();", type="NMI")
    
    # browser()
    # Ricrea la struttura di matrici che conterranno l'esito della computazione
    arr.nodi.end <- str_replace_all(string = WF.struct$info$arr.nodi.end, pattern  = "'",replacement = "")
    list.computation.matrix$trigger <<- matrix(0,ncol = length(names(WF.struct$info$trigger)),nrow = length(names(dataLog$pat.process)))
    list.computation.matrix$stati.transizione <<- matrix(0, ncol=length(names(WF.struct$info$stati)),nrow = length(names(dataLog$pat.process)))
    # list.computation.matrix$stati.finali <<- matrix(0,ncol = length(WF.struct$info$arr.nodi.end), nrow = length(names(dataLog$pat.process)))
    list.computation.matrix$stati.finali <<- matrix(0, ncol=length(names(WF.struct$info$stati)),nrow = length(names(dataLog$pat.process)))
    
    colnames(list.computation.matrix$stati.transizione)<<-names(WF.struct$info$stati)
    colnames(list.computation.matrix$trigger)<<-names(WF.struct$info$trigger)
    # colnames(list.computation.matrix$stati.finali)<<-arr.nodi.end
    colnames(list.computation.matrix$stati.finali)<<-names(WF.struct$info$stati)
    rownames(list.computation.matrix$stati.transizione) <<- names(dataLog$pat.process)
    rownames(list.computation.matrix$trigger) <<- names(dataLog$pat.process)
    rownames(list.computation.matrix$stati.finali) <<- names(dataLog$pat.process)
    

    
    
    # clear the notebook
    notebook <<- list()
    # and begin to note!
    addNote(msg = "\n<xml>")
    # Per ogni paziente
    for( indice in names(dataLog$wordSequence.raw)) {

      if(dim(dataLog$pat.process[[ indice ]])[1]>0) {
        
        if(param.verbose == TRUE) cat("\n Doing:",indice)
        
        addNote(msg = str_c("\n\t<computation n='",ct,"' IDPaz='",indice,"'>"))
        if(param.verbose == TRUE) cat(str_c("\nBeginning Pat ",indice,"..."))
        res <- playSingleSequence( matriceSequenza = dataLog$pat.process[[ indice ]], 
                                   col.eventName = dataLog$csv.EVENTName, 
                                   col.dateName = dataLog$csv.dateColumnName , 
                                   IDPaz = indice,
                                   event.interpretation = event.interpretation,
                                   date.format = dataLog$csv.date.format, UM = UM, store.computation.matrix= TRUE )
        # browser()
        if(param.verbose == TRUE) cat(str_c("\nPat ",indice," done;"))
        addNote(msg = "\n\t\t<atTheEnd>")
        # browser()
        ultimi.stati.computatzione <- unique(str_replace_all(string = res$st.ACTIVE,pattern = "'",replacement = ""))
        
        list.computation.matrix$stati.finali[ indice, ultimi.stati.computatzione ] <<- 1
        
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
  # get.list.replay.result (ex getPlayedSequencesStat.00)
  # Ops! Non ricordo piu' nemmeno io cosa fa questa funzione.....
  #===========================================================      
  get.list.replay.result<-function( csv.EventLog.inAddition = FALSE  ) {
    list.fired.trigger<-list()
    list.final.states<-list() 
    termination.END.states<-list()
    arr.nodi.end<-c()
    csv.EventLog<-c()

    # prendo la lista dei nodi END
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
    arr.nodi.end <- str_replace_all(string = arr.nodi.end, pattern  = "'",replacement = "")
    
    doc <- xmlInternalTreeParse(file = get.XML.replay.result(),asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))
    array.fired.trigger<-c()
    final.states<-c()
    fired.trigger<-c()
  # browser()
    for( i in arr.Computazioni) {
      arr.step<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step'),collapse = ""),xmlGetAttr,"n"))
      
      array.fired.trigger<-c()
      final.states<-c()
      fired.trigger<-c()    
      
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
      
      # browser()
      if(sum(final.states %in% arr.nodi.end)>0) { termination.END.states[[i]] <- TRUE }
      else { termination.END.states[[i]] <- FALSE }
    }
    
    if( csv.EventLog.inAddition == TRUE ) {
      for( nomePaziente in names(list.computation.matrix$stati.timeline)) {
        csv.EventLog <- rbind(csv.EventLog,
                              cbind(rep(nomePaziente,nrow(list.computation.matrix$stati.timeline[[nomePaziente]])),list.computation.matrix$stati.timeline[[nomePaziente]] ))
      }
      colnames(csv.EventLog)<-c("idPatient","event","event.status","eventDateTime","deltaTimeFromBegin")
    }
    
    return(list(
      "list.fired.trigger"=list.fired.trigger,
      "list.final.states"=list.final.states,
      "termination.END.states"=termination.END.states,
      "list.computation.matrix"=list.computation.matrix,
      "csv.EventLog"=csv.EventLog
    ))
  }
  # incrementa.tempo<-function( tempo , timeDetail )   {
  #   if(timeDetail=="days") tempo.new <- tempo + days(1)
  #   if(timeDetail=="hours") tempo.new <- tempo + hours(1)
  #   if(timeDetail=="weeks") tempo.new <- tempo + weeks(1)
  #   if(timeDetail=="months") tempo.new <- tempo + months(1)  
  #   return(tempo.new);
  # }
  #===========================================================  
  # playSingleSequence
  # esegue il conformanche checking con una specifica sequenza 
  # di LOG (di un paziente)
  #===========================================================    
  playSingleSequence<-function( matriceSequenza , col.eventName, col.dateName, IDPaz, 
                                event.interpretation="soft" , date.format="%d/%m/%Y %H:%M:%S", UM="days",
                                store.computation.matrix = FALSE) {
    # Cerca lo stato che viene triggerato dal BEGIN
    st.LAST<-""
    st.DONE<-c("")
    st.ACTIVE<-c("'BEGIN'")
    st.ACTIVE.time<-c()
    last.fired.trigger<-c()
    ct <- 0; riga <- 0
    error<-""
    computation.result<-"normally terminated"
    history.hop<-list()
    sequenza <- as.array(matriceSequenza[ ,col.eventName ])
    stop.computation <- FALSE
    lista.stati.possibili <- names(WF.struct$info$stati)
    
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]] 
    
    # popola l'array dei tempi di uptime degli stati (di attivazione)
    st.ACTIVE.time<-rep( 0 , length(lista.stati.possibili) )
    names(st.ACTIVE.time) <- lista.stati.possibili
    # e crea anche il cumulativo
    st.ACTIVE.time.cum <- st.ACTIVE.time
    
    # Analizza TUTTI gli eventi della sequenza
    for( indice.di.sequenza in seq(1,length(sequenza) )) {
      
      # due variabili comode per dopo
      ev.NOW <- sequenza[indice.di.sequenza]
      indice.di.sequenza.ch <- as.character(indice.di.sequenza)
      history.hop[[indice.di.sequenza.ch]]<-list()
      fired.trigger.in.this.iteration <- FALSE
      
      # Azzera il tempo di eventuali stati che sono stati resettati
      stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
      stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
      st.ACTIVE.time [ stati.da.resettare ] <- 0 
      # ORA scorri i giorni che passano fra l'evento precedente e quello in esame
      # Va fatto PRIMA di attivare il controllo sull'effetto dell'evento
      if( indice.di.sequenza > 1 ) {
        
        data.iniziale <- matriceSequenza[ ,col.dateName ][ indice.di.sequenza - 1 ]
        data.finale <- matriceSequenza[ ,col.dateName ][ indice.di.sequenza ]

        data.attuale <- data.iniziale
        fired.trigger.in.this.iteration <- TRUE
        
        # Cicla per tutti i giotni/ore/settimane dall'inizio alla fine
        # per vedere che non partano trigger legati a qualche DURATA
        while(  as.numeric(difftime(as.POSIXct(data.finale, format = date.format),
                                    as.POSIXct(data.attuale, format = date.format),units = 'mins')) > 0 ) {

          # cat(c("\n\t\t data.attuale = ",data.attuale))
          if(UM=="mins")
            data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + min(1),format = date.format)
          if(UM=="days")
            data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + days(1),format = date.format)
          if(UM=="hours")
            data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + hours(1),format = date.format)
          if(UM=="weeks")
            data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + weeks(1),format = date.format)
          
            newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = '', st.DONE = st.DONE, 
                                      st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                      st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                      EOF = FALSE , UM = UM  )
            # Se c'e' un errore, ferma tutto
            if(newHop$error==TRUE) {
              note.set.error(error = newHop$error)
              note.flush()
              return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger , "date" = data.ev.NOW   ) );
            }    
            # Se hai rilevato dei trigger attivi
            if(length(newHop$active.trigger)!=0) {
              ct <- ct + 1
              
              # gestisci il log
              newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
              note.setStep(number = ct)              
              note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
              note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
              note.setEvent(eventType = '', eventDate = as.character(data.attuale) )
              note.flush()              
              
              # Aggiorna le variabili
              st.ACTIVE <- newHop$st.ACTIVE
              last.fired.trigger<-newHop$active.trigger
              fired.trigger.in.this.iteration <- TRUE
              
              # Azzera il tempo di eventuali stati che sono stati resettati
              stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
              stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
              st.ACTIVE.time [ stati.da.resettare ] <- 0  

            }            
            # Aggiungi il delta data a quelli da aggiornare
            st.ACTIVE.time.cum [ stati.da.uppgradare ] <- st.ACTIVE.time.cum [ stati.da.uppgradare ] + 1
            st.ACTIVE.time [ stati.da.uppgradare ] <- st.ACTIVE.time [ stati.da.uppgradare ] + 1
            
            # Azzera il tempo di eventuali stati che sono stati resettati
            # si intende il "tempo" da cui sono ATTIVI (per valutare nel prossimo giro, se le durate superano
            # una certa threshold)
            stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
            stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
            st.ACTIVE.time [ stati.da.resettare ] <- 0 
        }
      }

      # Azzera il tempo di eventuali stati che sono stati resettati
      stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
      stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
      st.ACTIVE.time [ stati.da.resettare ] <- 0      

      if(param.verbose == TRUE) cat(str_c("\n\t processing:",ev.NOW))
      
      # costruisco un contatore della riga della tabella in analisi
      riga <- riga + 1
      data.ev.NOW <- matriceSequenza[ riga ,col.dateName ]
      ct <- ct + 1
      # gestisci il log
      newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
      note.setStep(number = ct)
      if("pMineR.internal.ID.Evt" %in% colnames(matriceSequenza))
      {
        note.setEvent(eventType = ev.NOW, eventDate =  data.ev.NOW , pMineR.internal.ID.Evt = matriceSequenza[riga,"pMineR.internal.ID.Evt"])
      }
      else  {
        note.setEvent(eventType = ev.NOW, eventDate = data.ev.NOW )
      }
      note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = ev.NOW, st.DONE = st.DONE, 
                                st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                EOF = FALSE  , UM = UM )
      history.hop[[indice.di.sequenza.ch]]$active.trigger<-newHop$active.trigger
      history.hop[[indice.di.sequenza.ch]]$ev.NOW<-ev.NOW
      history.hop[[indice.di.sequenza.ch]]$st.ACTIVE<-newHop$st.ACTIVE
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
        fired.trigger.in.this.iteration <- TRUE
      } else { 
        # altrimenti segnala che NON ci sono trigger attivi
        note.set.fired.trigger(array.fired.trigger = '')
        # E i nuovi stati validi sono esattamente i vecchi
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = st.ACTIVE)
      }
      # Fai il flush 
      note.flush()
      
      # Azzera il tempo di eventuali stati che sono stati resettati
      stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
      stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
      st.ACTIVE.time [ stati.da.resettare ] <- 0     
      
      # Ora ripeti la ricerca dei trigger senza passare alcun evento, giusto per 
      # vedere i trigger che si possono eventualmente attivare a seguito di 
      # pregressi triggers.
      devo.restare.in.trigger.loop<-TRUE
      list.to.avoid.inifinte.loop <- list()
      index.inf.loop <- 1
      sono.in.un.loop.infinito <- FALSE
      # cat("\n ---- ORPO")
      # Continua a loopare fino a che e' vero che qualche trigger e' scattato
      while(  devo.restare.in.trigger.loop == TRUE & sum(arr.nodi.end %in% st.ACTIVE) == 0 ) {
        

        # if( IDPaz =="PSHAP00120160004501") browser()
        
        # Azzera il tempo di eventuali stati che sono stati resettati
        stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
        stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
        st.ACTIVE.time [ stati.da.resettare ] <- 0
        
        # Verifica se c'è un trigger
        newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = "", st.DONE = st.DONE, 
                                  st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                  st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                  EOF = FALSE  , UM = UM )
        # inzializza il log in caso di errore o in caso di trigger
        if(newHop$error==TRUE | length(newHop$active.trigger)!=0) {
          ct <- ct + 1
          newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
          note.setStep(number = ct)
          note.setEvent(eventType = '', eventDate = data.ev.NOW, pMineR.internal.ID.Evt = '')
          note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
          # list.to.avoid.inifinte.loop[ index.inf.loop ] <- list( "st.ACTIVE"=st.ACTIVE, "")
          # index.inf.loop <- index.inf.loop + 1
        }
        # Se c'e' un errore, ferma tutto
        if(newHop$error==TRUE) {
          note.set.error(error = newHop$error)
          note.flush()
          return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger"=last.fired.trigger, "date" = data.ev.NOW  ) )
        }
        # Se hai rilevato qualche trigger attivo
        if(length(newHop$active.trigger)!=0)  {
          # verifica che la lista dei nodi attivi ed trigger non siano già avvenuto Se no, rischio un loop infinito
          # Se non è null, significa che è già scattato in passato
          if(!is.null(list.to.avoid.inifinte.loop[[ last.fired.trigger ]] )) {
              for(tmptmptmp in names(list.to.avoid.inifinte.loop[[ last.fired.trigger ]])) {
                # se la lunghezza è uguale
                if(length(list.to.avoid.inifinte.loop[[ last.fired.trigger ]][[tmptmptmp]]) == length(newHop$st.ACTIVE)) {
                  # e se sono TUTTI uguali
                  if(sum(newHop$st.ACTIVE %in% list.to.avoid.inifinte.loop[[ last.fired.trigger ]][[tmptmptmp]]) == length(newHop$st.ACTIVE)) {
                    sono.in.un.loop.infinito <- TRUE
                  }
                }
              }
          }
          if( sono.in.un.loop.infinito == FALSE ) {
            note.setEvent(eventType = '', eventDate = data.ev.NOW, pMineR.internal.ID.Evt = '' )
            note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
            note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
            note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
            st.ACTIVE <- newHop$st.ACTIVE
            last.fired.trigger<-newHop$active.trigger
            # e fai il flush
            note.flush()     
            
            # aggiorna la lista che contiene i trigger eseguiti e le condizioni si stato così da poterla poi confrontare
            # in futuro. Se non lo facessi, rischiei un loop infinito
            
            if( is.null(list.to.avoid.inifinte.loop[[ last.fired.trigger ]] )) list.to.avoid.inifinte.loop[[ last.fired.trigger ]]<-list()
            list.to.avoid.inifinte.loop[[ last.fired.trigger ]][[as.character(length(list.to.avoid.inifinte.loop[[ last.fired.trigger ]])+1) ]] <-  st.ACTIVE
          }
          else devo.restare.in.trigger.loop<-FALSE
        }
        # altrimenti (se non ci sono stati trigger, vedi di uscire dal loop)
        else devo.restare.in.trigger.loop<-FALSE
      }
      # Se i vincoli di interpretazione degli event log sono "hard" allora non posso accettare
      # di passare ad un altro evento, se un evento non ha scatenato trigger!
      if(event.interpretation == "hard" & fired.trigger.in.this.iteration == FALSE)  {
        computation.result <- "event not predicted in hard checking"
        stop.computation <- TRUE
        break;
      }
      
      # Se ho beccato uno stato END, esci
      if(sum(arr.nodi.end %in% st.ACTIVE) != 0) {
        break;
      }
      
    }
    
    # Se la computazione non e', per qualche motivo, interrotta
    if( stop.computation == FALSE  & sum(arr.nodi.end %in% st.ACTIVE) == 0) {
      # Now process the EOF !!
      ct <- ct + 1
      # gestisci il log
      newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
      note.setStep(number = ct)
      note.setEvent(eventType = ev.NOW, eventDate = data.ev.NOW , pMineR.internal.ID.Evt = 'EOF')
      note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = '', st.DONE = st.DONE, 
                                st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                EOF = TRUE  )
      
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
      
      # Se ho uno stato di 'END' attivo, chiudi la computazione (fai giusto l'EOF)
      if( sum(arr.nodi.end %in% st.ACTIVE) != 0 ) { 
        stop.computation <- TRUE 
      }
    }

    # Ritorna
    return( list( "st.ACTIVE"=st.ACTIVE,
                  "error"=error,
                  "last.fired.trigger" = last.fired.trigger, 
                  "date" = data.ev.NOW,
                  "history.hop" = history.hop,
                  "computation.result" = computation.result) );
  }   
  #===========================================================  
  # attiva.trigger
  # is the "core": it finds out the trigger that should be fired
  # INPUT:
  #   st.LAST - l'array degli stati attivati nel tempo (almeno una volta)
  #   ev.NOW  - l'evento letto ora.
  #   st.ACTIVE - l'array degli stati attivi prima di attivare l'evento attuale
  #   st.ACTIVE.time - il tempo di 'uptime' degli stati (prima di un 'unset', che rimette a zero)
  #   st.ACTIVE.time.cum - il tempo di 'uptime' degli stati (cumulativo)
  #   EOF - 'TRUE' indica che la computazione è finita
  #===========================================================    
  attiva.trigger<-function( st.LAST, ev.NOW, st.DONE, st.ACTIVE, st.ACTIVE.time, st.ACTIVE.time.cum, EOF , UM="days" ) {
    # inizializza
    new.st.DONE<-st.DONE;
    new.st.LAST<-c()
    new.st.ACTIVE<-st.ACTIVE
    active.trigger<-c()
    global.array.to.set<-c(); global.array.to.unset<-c();
    errore <- FALSE;   errMsg<-"";
    tabella.set.unset <- c()

    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct[[ "info" ]][[ "trigger" ]]) ) {

      # Prendi la condizione
      precondizione <- WF.struct[["info"]][["trigger"]][[trigger.name]]$condition
      stringa.to.eval<-precondizione
      
      # Agisci solo nel caso in cui una CONDITION sia stata definita, 
      # per quel trigger (alcuni trigger potrebbero NON avere una CONDITION)
      if(!is.na(precondizione)) {
      
        # Inizia a costruire la stringa da parsare, rimpiazzando gli array
        rimpiazzo.ev.NOW<-paste( c("'",ev.NOW,"'") ,collapse='');
        rimpiazzo.st.ACTIVE<- paste(c("c(",paste(c(st.ACTIVE),collapse=","),")" ),collapse='')
        
        stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$ev.NOW\\$",replacement = rimpiazzo.ev.NOW)
        stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$st.ACTIVE\\$",replacement = rimpiazzo.st.ACTIVE)
        stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$EOF\\$",replacement = str_c("'",EOF,"'") )
        
        stringa.to.eval<- str_replace_all(string = stringa.to.eval,pattern = " OR ",replacement = " | ")
        stringa.to.eval<- str_replace_all(string = stringa.to.eval,pattern = " AND ",replacement = " & ")
        
        stringa.to.eval <- parse.for.temporal.conditions(
                                  stringa = stringa.to.eval,
                                  st.ACTIVE.time = st.ACTIVE.time,
                                  st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                  UM = UM) 
        
        # Parsa la stringa
        if(stringa.to.eval=="") risultato <- TRUE
        else risultato <- eval(expr = parse(text = stringa.to.eval))
        
        # Se la condizione e' soddisfatta, aggiorna le variabili
        if( risultato == TRUE ) {
          # prendi la priorità
          pri <- WF.struct[["info"]][["trigger"]][[trigger.name]]$pri
          
          array.to.set<-unlist((WF.struct[["info"]][["trigger"]][[trigger.name]]$set))
          array.to.unset<-unlist((WF.struct[["info"]][["trigger"]][[trigger.name]]$unset))
          
          # Se devi fare un unsetAll, provvedi, prendendo per esplicito la lista degli stati di cui fare l'unset
          if(   WF.struct[["info"]][["trigger"]][[trigger.name]]$unsetAll == TRUE) {
            aaa <- names(WF.struct$info$stati)[ !(names(WF.struct$info$stati) %in% str_replace_all(string = array.to.set,pattern = "'",""))   ]
            array.to.unset <- unique(c(array.to.unset, aaa))
            for(i in seq(1,length(array.to.unset))) {
              array.to.unset[ i ] <- paste(c("'",array.to.unset[ i ],"'"),collapse = '' )
            }
          }
          
          # aggiungi le righe alla matrice che definisce le azioni
          # browser()
          for(i in seq(1,length(array.to.set))) {
            if(!is.null(array.to.set)) {
              tabella.set.unset <- rbind( tabella.set.unset, c( trigger.name,array.to.set[i],"set",pri  )   )
            }
          }
          for(i in seq(1,length(array.to.unset))) {
            if(!is.null(array.to.unset)) {
              tabella.set.unset <- rbind( tabella.set.unset, c( trigger.name,array.to.unset[i],"unset",pri  )   )
            }
          }          

        }
      }
    }
    
    # Se esista la tabella, verifica i conflitti di set/unset
    if(!is.null(tabella.set.unset)) {
      
      res <- check.conflitti.set.unset( tabella = tabella.set.unset )
      
      # Ora non dimenticare eventuali stati già presenti!
      new.st.ACTIVE <- c(res$to.set,st.ACTIVE)
      new.st.ACTIVE <- unique(new.st.ACTIVE[!(new.st.ACTIVE %in% res$to.unset)])
      active.trigger <- res$triggers.name
      new.st.LAST <- res$to.set
      new.st.DONE <- c(st.DONE,res$to.set)
      errMsg <- res$errorMsg
      errore <- res$errore
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
  # check.conflitti.set.unset
  # verifica che nella tabella set/unset non ci siano conflitti
  #===========================================================      
  check.conflitti.set.unset<- function( tabella ) {
    
    global.array.to.set <- c()
    global.array.to.unset <- c()
    
    # metti dei nomi decenti alle colonne
    colnames(tabella)<-c("trigger","stato","action","pri")
    # prendi la lista delle priorità censite
    arr.priorita.censite <- unique(tabella[,"pri"])
    errMsg <- ""; errore<-FALSE;
    
    # Per ogni livello di priorità
    for( i in sort(arr.priorita.censite)) {
      
      # Verifica che a questo livello di priorità non ci siano contraddizioni
      local.array.to.set <- unique( tabella[which( tabella[,"action"]=="set" & tabella[,"pri"]==as.character(i)),"stato"]   )
      local.array.to.unset <- unique( tabella[which( tabella[,"action"]=="unset" & tabella[,"pri"]==as.character(i) ),"stato"]   )
      
      local.conflitto <- sum(local.array.to.set %in% local.array.to.unset)
      
      # se ci sono conflitti
      if(local.conflitto>0) {
        conflicting.Triggers <- tabella[which( tabella[,"pri"]==as.character(i) ),"trigger"]
        errMsg <- c("Set e unset conflicting. Check the following triggers: ",paste( conflicting.Triggers,collapse=','  ));
        errore = TRUE;
        obj.LogHandler$sendLog(msg = errMsg, type="NMI")        
      }
      # calcola gli array set/uset da proporre ai livelli di priorità più alti
      # Togli eventuali set/unset di livello inferiore
      global.array.to.set <- global.array.to.set[ !(global.array.to.set %in% local.array.to.unset)  ]
      global.array.to.unset <- global.array.to.unset[ !(global.array.to.unset %in% local.array.to.set)  ]
      # Accoda i nuovi contributi di set/unset
      global.array.to.set <- unique(c(global.array.to.set,local.array.to.set))
      global.array.to.unset <- unique(c(global.array.to.unset,local.array.to.unset))
    }
    triggers.name <- unique(tabella[,"trigger"])
    return(list(
      "to.set" = global.array.to.set,
      "to.unset" = global.array.to.unset,
      "triggers.name" = triggers.name,
      "errMsg" = errMsg,
      "errore" = errore
    ))
  }
  #===========================================================  
  # parse.for.temporal.conditions
  # esegue il parse per eventuali 'condition' con aspetti temporali. Li metto su una
  # funzione a parte per facilitare la visibilità della funzione chiamante (ben 
  # più importante)
  #===========================================================    
  parse.for.temporal.conditions <- function(stringa , st.ACTIVE.time , st.ACTIVE.time.cum , UM = "days") {

    stringa.run <- stringa
    
    lista.comandi.condition <- list(
      "afmth" = ".afmth\\([0-9]+\\)",
      "afmeth" = ".afmeth\\([0-9]+\\)",      
      "afmtd" = ".afmtd\\([0-9]+\\)",
      "afmetd" = ".afmetd\\([0-9]+\\)",
      "afmtw" = ".afmtw\\([0-9]+\\)",
      "afmetw" = ".afmetw\\([0-9]+\\)",      
      "afmtm" = ".afmtm\\([0-9]+\\)",
      "afmetm" = ".afmetm\\([0-9]+\\)",      
      "aflth" = ".aflth\\([0-9]+\\)",
      "afleth" = ".afleth\\([0-9]+\\)",
      "afltd" = ".afltd\\([0-9]+\\)",
      "afletd" = ".afletd\\([0-9]+\\)",
      "afltw" = ".afltw\\([0-9]+\\)",
      "afletw" = ".afletw\\([0-9]+\\)",      
      "afltm" = ".afltm\\([0-9]+\\)",
      "afletm" = ".afletm\\([0-9]+\\)"      
    )
    
    # Passa il contenuto di st.ACTIVE.time e st.ACTIVE.time.cum in minuti
    
    if( UM == "mins")  moltiplicatore <- 1
    if( UM == "hours") moltiplicatore <- 60
    if( UM == "days")  moltiplicatore <- 60 * 24
    if( UM == "weeks") moltiplicatore <- 60 * 24 * 7
    
    st.ACTIVE.time <- st.ACTIVE.time * moltiplicatore
    st.ACTIVE.time.cum <- st.ACTIVE.time.cum * moltiplicatore
    
    for( comandoToCheck in names(lista.comandi.condition) ) {

      # poni un default per l'esito
      esito = FALSE
      
      stringaToMatch <- lista.comandi.condition[[ comandoToCheck ]]
      pos.par <- str_locate(string = stringaToMatch, pattern = "\\(")[1]-1

      if( length(str_locate_all(string = stringa.run, pattern = stringaToMatch )[[1]]) > 0  ) {
        matrice.match <- str_locate_all(string = stringa.run, pattern = stringaToMatch )[[1]]
        for(riga in nrow(matrice.match)) {
          
          # poni un default per l'esito
          esito = FALSE
          
          # estrai la quantità fra le parentesi
          quantita <- str_sub(string = stringa.run,start = matrice.match[riga, "start"]+pos.par,end = matrice.match[riga, "end"]-1)
          quantita <- as.numeric(quantita)
    
          # estrai il nome dello stato
          nomeStato <- str_sub(string = stringa.run,end = matrice.match[riga, "start"]-1)
          subMatrix.nomeStato <- str_locate_all(string = nomeStato, pattern = "'" )[[1]]
          nomeStato <- str_sub(string = nomeStato,
                               start = subMatrix.nomeStato[ nrow(subMatrix.nomeStato)-1, "start"]+1,
                               end = subMatrix.nomeStato[ nrow(subMatrix.nomeStato), "start"]-1)
          # browser()
          # Ora fai due riflessioni...
          if(comandoToCheck == "afmtm") { esito = st.ACTIVE.time[ nomeStato ] > (quantita) }
          if(comandoToCheck == "afmetm") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita) }          
          if(comandoToCheck == "afmth") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 ) }
          if(comandoToCheck == "afmeth") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 ) }
          if(comandoToCheck == "afmtd") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 * 24) }
          if(comandoToCheck == "afmetd") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 * 24) }
          if(comandoToCheck == "afmtw") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 * 24 * 7) }
          if(comandoToCheck == "afmetw") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 * 24 * 7) }
          # if(comandoToCheck == "afmtm") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 * 24 * 30) }
          # if(comandoToCheck == "afmetm") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 * 24 * 30) }

          if(comandoToCheck == "afltm") { esito = st.ACTIVE.time[ nomeStato ] < (quantita) }
          if(comandoToCheck == "afletm") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita) }    
          if(comandoToCheck == "aflth") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 ) }
          if(comandoToCheck == "afleth") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 ) }
          if(comandoToCheck == "afltd") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 * 24) }
          if(comandoToCheck == "afletd") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 * 24) }
          if(comandoToCheck == "afltw") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 * 24 * 7) }
          if(comandoToCheck == "afletw") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 * 24 * 7) }  
          # if(comandoToCheck == "afltm") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 * 24 * 30) }
          # if(comandoToCheck == "afletm") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 * 24 * 30) }    

          # cat("\n ",st.ACTIVE.time[ nomeStato ], "  quantita = ",quantita )
          # if( esito == TRUE ) browser()
          
          # stringa.run <- paste( c( str_sub(stringa.run,end = matrice.match[riga, "start"]-1),' == \'',nomeStato,'\' ',str_sub(stringa.run,start = matrice.match[riga, "end"]+1) )    , collapse='')
          # browser()
          stringa.run <- paste(c( str_sub(stringa.run,end = subMatrix.nomeStato[ nrow(subMatrix.nomeStato)-1, "start"]-1), "'",esito,"'",str_sub(stringa.run,start = matrice.match[riga, "end"] +1) ), collapse='')
          # if(esito==TRUE){
          #   stringa.run <- paste( c( str_sub(stringa.run,end = matrice.match[riga, "start"]-1),' == \'',nomeStato,'\' ',str_sub(stringa.run,start = matrice.match[riga, "end"]+1) )    , collapse='')
          # } else {
          #   stringa.run <- paste( c( str_sub(stringa.run,end = matrice.match[riga, "start"]-1),' != \'',nomeStato,'\' ',str_sub(stringa.run,start = matrice.match[riga, "end"]+1) )    , collapse='')
          # }
        }
      }
    }
    stringa.new <- stringa.run
    return(stringa.new);
  }
  
  #===========================================================  
  # get.XML.replay.result (ex getXML)
  # it returns the XML file
  #===========================================================  
  get.XML.replay.result<-function(notebook.name='computationLog', writeToFile = FALSE, fileName="./output.xml"){
    
    # if the output is not on file
    if( writeToFile == FALSE )    return(notebook[[notebook.name]])

    # otherwise, write a file
    fileConn<-file(fileName)
    writeLines(notebook[[notebook.name]], fileConn)
    close(fileConn)
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
  # plot (ex plotGraph)
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  #===========================================================   
  plot<-function(  ) {
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
    }
    # prendo i nodi end
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
    
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
    grViz(a);
}
  #===========================================================  
  # plotPatientEventTimeLine
  # plot the event timeline for a ginven patient
  #===========================================================  
  plotPatientEventTimeLine<-function( patientID , cex.axis = 0.6, cex.text = 0.7) {
    
    st.POST<-list(); st.PRE<-list(); tr.fired<-list()
    txt.section<-"";
    
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.step <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"evt"))
    arr.date <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"date"))
    
    matrice<-c()
    for( riga in seq(1,length(arr.step))) {
      matrice <- rbind(matrice, cbind(  arr.date[ riga ] , arr.step[ riga ]  ) )
    }
    
    plotTimeline(matrice, dataLog$csv.date.format, cex.axis = cex.axis, cex.text = cex.text)
  }  
  
  #===========================================================  
  # plotPatientReplayedTimeline (ex plotPatientComputedTimeline)
  # plot the computed timeline for a given patient
  #===========================================================   
  plotPatientReplayedTimeline<-function( patientID ) {  
    
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
  # plot.replay.result  ( ex plotComputationResult)
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  # 'avoidFinalStates' is an array containing the final states 
  #                to filter the computation
  # kindOfNumber : i numeri: 'relative' o 'absolute'
  #===========================================================   
  plot.replay.result<-function( whatToCount='activations' ,     kindOfNumber='relative', 
                                   avoidFinalStates=c(), avoidTransitionOnStates=c(), avoidToFireTrigger=c(), whichPatientID=c("*"), plot.unfired.Triggers = TRUE ) {
    
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
    }
    # prendo i nodi end
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
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
    # arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]

  
    
    # Ora sistema le froceries grafiche
    # PER I NODI
    stringa.stati<-"node [fillcolor = Orange]"
    stringa.stati.finali <- "node [fillcolor = Red]";
    for(nome.stato in arr.stati.raggiungibili)  {
      nome.stato.pulito <- str_replace_all(string = nome.stato,pattern = "'",replacement = "")
      aa = giveBackComputationCounts(nomeElemento = nome.stato.pulito, tipo='stato', whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID)
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
      penwidth<- 1
      colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
      if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
      else numberToPrint<-str_c("# ",aa$howMany)
      # Se non è uno stato finale
      if(!(nome.stato %in% arr.nodi.end)) {
        stringa.stati <- str_c(stringa.stati,"\n\t ",nome.stato," [label = '",nome.stato.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"',  pencolor='Gray",colore,"']") 
      }  
      else {
        stringa.stati.finali <- str_c(stringa.stati.finali,"\n\t ",nome.stato," [label = '",nome.stato.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"',  pencolor='Gray",colore,"']")         
      }
    }
    # PER I TRIGGER
    lista.freq.trigger<-list()
    stringa.trigger<-"node [fillcolor = white, shape = box ]"
    for(nome.trigger in arr.trigger.rappresentabili)  {
      nome.trigger.pulito <- str_replace_all(string = nome.trigger,pattern = "'",replacement = "")
      aa = giveBackComputationCounts(nomeElemento = nome.trigger.pulito, tipo='trigger', whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID )
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      if( plot.unfired.Triggers == TRUE | (plot.unfired.Triggers==FALSE & howMany>0)) {
        lista.freq.trigger[[nome.trigger]]<-(aa$howMany  / aa$totalNumber)
        penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
        penwidth<- 1
        colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
        if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
        else numberToPrint<-str_c("# ",aa$howMany)
        stringa.trigger <- str_c(stringa.trigger,"\n\t ",nome.trigger," [label = '",nome.trigger.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"', fontcolor='Gray",colore,"']") 
      }
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
    
    a<-paste(c("digraph boxes_and_circles {
               
               # a 'graph' statement
               graph [overlap = true, fontsize = 10]
               
               # several 'node' statements
               node [shape = oval,
               fontname = Helvetica,
               style = filled]
               node [fillcolor = green] 
               'BEGIN'; 
               ",stringa.stati.finali,"
               
               ",stringa.stati,"
               ",stringa.trigger,"
               
               edge [arrowsize = 1 ]
               # several edge
               ",stringa.nodo.from,"
               ",stringa.nodo.to,"
  }"), collapse='') 
    grViz(a);
  }  
  old.plot.replay.result<-function( whatToCount='activations' ,     kindOfNumber='relative', 
                                avoidFinalStates=c(), avoidTransitionOnStates=c(), avoidToFireTrigger=c(), whichPatientID=c("*"), plot.unfired.Triggers = TRUE ) {
    
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
    }
    # prendo i nodi end
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
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
      if( plot.unfired.Triggers == TRUE | (plot.unfired.Triggers==FALSE & howMany>0)) {
        lista.freq.trigger[[nome.trigger]]<-(aa$howMany  / aa$totalNumber)
        penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
        penwidth<- 1
        colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
        if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
        else numberToPrint<-str_c("# ",aa$howMany)
        stringa.trigger <- str_c(stringa.trigger,"\n\t ",nome.trigger," [label = '",nome.trigger.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"', fontcolor='Gray",colore,"']") 
      }
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
  #               'terminations' se si vogliono contare quante volte vi e' terminato
  # avoidFinalStates = un array che contiene gli stati che non devono essere 
  #                    finali per i pazienti da considerare (serve per fare un filtro)
  # avoidTransitionOnStates = un array che indica gli stati che non devono essre
  #                           transitati dai pazienti da considerare (e' un filtro)
  # avoidToFireTrigger = badali', indovina un po'?
  #===========================================================  
  giveBackComputationCounts<-function( nomeElemento, tipo, whatToCount, avoidFinalStates, avoidTransitionOnStates, avoidToFireTrigger , whichPatientID) {
    # Carica l'XML
    # browser()
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))
    totalAmount <- 0 
    totalNumber <- 0
    array.patID <- c()
    
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
      
      # Verifica se il finale e' incluso in quelli da scartare: se si', skippa tutto il paziente 
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
      array.patID <- c(array.patID,PatientID)
    }
    return( list( "howMany"=totalAmount,  "totalNumber"=totalNumber ,"array.patID" = array.patID)  ) 
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
  newNote<-function( number , store.computation.matrix = FALSE , idPatient = NA){
    tmpAttr<<-list()
    tmpAttr$stepNumber<<-''
    tmpAttr$boolean.fired.trigger<<-FALSE
    tmpAttr$event<<-""
    tmpAttr$event.date<<-""
    tmpAttr$store.computation.matrix <<- store.computation.matrix
    tmpAttr$idPatient <<- idPatient
  }
  note.setStep<-function( number ){
    tmpAttr$stepNumber <<- number
  }  
  note.setEvent<-function( eventType , eventDate , pMineR.internal.ID.Evt=''){
    tmpAttr$event <<- eventType
    tmpAttr$event.date <<- eventDate
    tmpAttr$pMineR.internal.ID.Evt <<- pMineR.internal.ID.Evt
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
    testo<-str_c("\n\t\t<step n='",tmpAttr$stepNumber,"' trg='",tmpAttr$boolean.fired.trigger,"' evt='",tmpAttr$event,"' date='",tmpAttr$event.date,"' pMineR.internal.ID.Evt='",tmpAttr$pMineR.internal.ID.Evt,"'>")
    if(tmpAttr$boolean.fired.trigger==TRUE)  {
      for(i in tmpAttr$st.ACTIVE.PRE) testo<-str_c(testo,"\n\t\t\t<st.ACTIVE.PRE name=",i,"></st.ACTIVE.PRE>")
      for(i in tmpAttr$fired.trigger) testo<-str_c(testo,"\n\t\t\t<fired.trigger name='",i,"'></fired.trigger>")
      for(i in tmpAttr$st.ACTIVE.POST) testo<-str_c(testo,"\n\t\t\t<st.ACTIVE.POST name=",i,"></st.ACTIVE.POST>")
    }
    testo<-str_c(testo,"\n\t\t</step>")
    addNote(msg = testo)
    
    # if( tmpAttr$idPatient == "PSHAP00120160004501") browser()
    
    # Vedi se devi aggiornare anche la lista delle matrici 
    # con i log della computazione
    if( tmpAttr$store.computation.matrix == TRUE ) {
      tmp.stati.post <- str_replace_all(string = tmpAttr$st.ACTIVE.POST, pattern  = "'",replacement = "")
      tmp.stati.pre <- str_replace_all(string = tmpAttr$st.ACTIVE.PRE, pattern  = "'",replacement = "")  
      tmp.stati.pre.da.abbassare <- tmp.stati.pre[ which( !(tmp.stati.pre %in% tmp.stati.post))   ] 
      tmp.stati.post <- tmp.stati.post[!(tmp.stati.post %in% tmp.stati.pre)]
      list.computation.matrix$trigger[tmpAttr$idPatient, unique(tmpAttr$fired.trigger)]<<- list.computation.matrix$trigger[tmpAttr$idPatient,unique(tmpAttr$fired.trigger)] +1
      list.computation.matrix$stati.transizione[tmpAttr$idPatient, unique(tmp.stati.post)]<<- list.computation.matrix$stati.transizione[tmpAttr$idPatient,unique(tmp.stati.post)] +1
      
      if(length(list.computation.matrix$stati.timeline)==0) { 
        list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]]<<-list()
        list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]]<<-c()
      }
      
      # if( tmpAttr$idPatient == "PSHAP00120160004530") browser()
      
      
      # Dichiara gli inizi di stato      
      for(nome.stato in tmp.stati.post ) {
        # browser()
        if(tmpAttr$pMineR.internal.ID.Evt=="") tempo.dall.inizio = "NA"
        else tempo.dall.inizio <- dataLog$pat.process[[ tmpAttr$idPatient  ]][ which( dataLog$pat.process[[ tmpAttr$idPatient  ]][ ,"pMineR.internal.ID.Evt"]==tmpAttr$pMineR.internal.ID.Evt )  ,"pMineR.deltaDate"]

        if(length( list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] )==0) {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<-  c(nome.stato,"begin",tmpAttr$event.date, tempo.dall.inizio  )
        }
        else { 
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<- rbind(
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]],
          c(nome.stato,"begin",tmpAttr$event.date, tempo.dall.inizio  ))
        }
        
      }
      # Dichiara la fine di stato
      for(nome.stato in tmp.stati.pre.da.abbassare  ) {
        # browser()
        if(tmpAttr$pMineR.internal.ID.Evt=="") tempo.dall.inizio = "NA"
        else tempo.dall.inizio <- dataLog$pat.process[[ tmpAttr$idPatient  ]][ which( dataLog$pat.process[[ tmpAttr$idPatient  ]][ ,"pMineR.internal.ID.Evt"]==tmpAttr$pMineR.internal.ID.Evt )  ,"pMineR.deltaDate"]
        
        if(length( list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] ) ==0) {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<- c(nome.stato,"end",tmpAttr$event.date,tempo.dall.inizio)
        }
        else  {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<- rbind(
            list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]],
            c(nome.stato,"end",tmpAttr$event.date,tempo.dall.inizio))
        }          
      }
      
    }
    
  }   
  addNote<-function( msg, level='1', notebook.name='computationLog' ) { 
    # Crea la posizione, se ancora non c'e'
    if(length(notebook)==0) notebook[[notebook.name]]<<-c()
    else {  if( !(notebook.name %in% names(notebook))) notebook[[notebook.name]]<<-c()  }
    # Accoda la nota
    notebook[[notebook.name]]<<-str_c(notebook[[notebook.name]],msg)
    return;
  }  

  
  
  #=================================================================================
  # play (ex play.easy)
  #   number.of.cases : numero di casi da generare
  #   min.num.of.valid.words : numero minimo di parole valide
  #   max.word.length : numero massimo di eventi per parola
  #=================================================================================  
  play<-function(number.of.cases, min.num.of.valid.words=NA, 
                      max.word.length=100, howToBuildBad="resample", 
                      toReturn="csv", debug.mode = FALSE, output.format.date = "%d/%m/%Y",
                      typeOfRandomDataGenerator="dayAfterDay") {
    # cat("\n (1)")
    # Aggiorna il formato data dell'ultimp PLAY
    play.output.format.date <<- output.format.date
    
    if(is.na(min.num.of.valid.words)) min.num.of.valid.words = number.of.cases
    quante.da.sbagliare <- number.of.cases - min.num.of.valid.words

    # Se e' stato chiesto di generare anche delle sequenze NON VALIDE, genera delle 
    # sequenze NON VALIDE (scusate il nome di sta fava 'play.easy.impreciso')
    if(min.num.of.valid.words>0) {
      a <- play.easy.impreciso(number.of.cases = min.num.of.valid.words,min.num.of.valid.words = min.num.of.valid.words,
                               max.word.length = max.word.length, howToBuildBad = howToBuildBad,
                               output.format.date = output.format.date , typeOfRandomDataGenerator=typeOfRandomDataGenerator)
    }
    # cat("\n (2)")
    # browser()
    if(quante.da.sbagliare>0) {
      totalizzati = 0
      while(totalizzati < quante.da.sbagliare) {
        b <- play.easy.impreciso(number.of.cases = 1,min.num.of.valid.words = 0,
                                 max.word.length = max.word.length, howToBuildBad = howToBuildBad,
                                 output.format.date = output.format.date, typeOfRandomDataGenerator=typeOfRandomDataGenerator)
        if(b$arr.matching.parola==FALSE) {
          if(min.num.of.valid.words==0 & totalizzati==0) {a <- b}
          else  { a <- join.giving.new.ID(a,b) }
          totalizzati <- totalizzati + 1
        }
      }
    }
    # cat("\n (3)")
    # Restituisci cio' che serve venga restituito
    # nel caso del CSV
    if(toReturn=="csv") {
      daRestituire <- a
    }
    # nel caso del dataLoader
    if(toReturn=="dataLoader") {
      # Istanzia un oggetto dataLoader che eridita il parametro "verbose"
      daRestituire<-dataLoader(verbose.mode = param.verbose)
      daRestituire$load.data.frame(mydata = a$valid.data.frame,
                                   IDName = "patID",EVENTName = "event",
                                   dateColumnName = "date" , 
                                   format.column.date = output.format.date)
    }
    if(toReturn!="csv" & toReturn!="dataLoader") stop("ERRORE: 'toReturn' non valorizzata correttamente")
    return(daRestituire)
    
  }
  join.giving.new.ID<-function( a , b ) {
    new.b <- as.character(max(as.numeric(names(a$lista.parole$list.nodes)))+1)
    attuale.b<- names(b$lista.parole$list.LOGs)
    
    a$lista.parole$list.LOGs[[new.b]] <- b$lista.parole$list.LOGs[[attuale.b]]
    a$lista.parole$list.nodes[[new.b]] <- b$lista.parole$list.nodes[[attuale.b]]
    a$arr.matching.parola <- c(a$arr.matching.parola,b$arr.matching.parola)
    b$valid.data.frame$patID <- rep(new.b,length(b$valid.data.frame$patID))
    a$valid.data.frame<-rbind(a$valid.data.frame,b$valid.data.frame)    
    
    return(a)
  }
  play.easy.impreciso<-function(number.of.cases, min.num.of.valid.words=NA, max.word.length=100, 
                                howToBuildBad="resample", output.format.date = "%d/%m/%Y %H:%M:%S", 
                                date.format = "%d/%m/%Y %H:%M:%S", UM="days",
                                typeOfRandomDataGenerator="dayAfterDay") {
    obj.utils <- utils()
    if(is.na(min.num.of.valid.words)) min.num.of.valid.words = as.integer(number.of.cases/2)
    arr.matching.parola<-c()
    
    # Genera un buon numero di parole valide
    lista.res <- genera.parola.valida(number.of.cases = number.of.cases,
                                      max.word.length = max.word.length )
    
    # browser()
    # Ora prendine la meta' e fai uno shuffle
    quante.da.mescolare <- number.of.cases - min.num.of.valid.words
    aaa <- lista.res$list.LOGs
 
    if(quante.da.mescolare>0) {
      for(indice.parola in seq(1,quante.da.mescolare)) {
        if(param.verbose == TRUE)  cat("\n ",indice.parola)
          if(howToBuildBad=="resample") {
            lista.res$list.LOGs[[ indice.parola ]] <- sample(x = lista.res$list.LOGs[[ indice.parola ]] ,
                                                             size = length(lista.res$list.LOGs[[ indice.parola ]]))
          }
        if(howToBuildBad=="subtle") {

          if(length(lista.res$list.LOGs[[ indice.parola ]])>=5){
            dado <- as.integer(runif(n = 1,min = 2,max = length(lista.res$list.LOGs[[ indice.parola ]])-2))
            tmp.val<-lista.res$list.LOGs[[ indice.parola ]][ dado ]
            lista.res$list.LOGs[[ indice.parola ]][ dado ] <- lista.res$list.LOGs[[ indice.parola ]][ dado+1 ]
            lista.res$list.LOGs[[ indice.parola ]][ dado+1 ] <- tmp.val
          }
        }
      }
      # Ora devo controllare, di quelle che ho "shuffellato", quante sono ancora valide!
      for(indice.parola in seq(1,quante.da.mescolare)) {
        # Costruisci la matrice per consentire l'eseguibilita'
        
        marice.dati<-c()
        numeroGiorno<-1
        for(index.car in seq(1,length(lista.res$list.LOGs[[ indice.parola ]]))) {
          sing.car <- lista.res$list.LOGs[[ indice.parola ]][index.car]
          nuovaDatatmp <- as.Date("01/01/2000",format="%d/%m/%Y") + numeroGiorno
          nuovaDatatmp <- as.character(format( nuovaDatatmp, format = output.format.date ))
          nuovaRiga<-c(nuovaDatatmp,sing.car)
          # if(typeOfRandomDataGenerator=="dayAfterDay") giorni.da.sommare <-1
          # if(typeOfRandomDataGenerator=="randomWeek1-4") giorni.da.sommare <- as.integer(runif(n = 1,min=1,max=4) * 7)
          # if(typeOfRandomDataGenerator=="randomMonth1-4") giorni.da.sommare <- as.integer(runif(n = 1,min=1,max=4) * 30)

          numeroGiorno<-numeroGiorno+1
          
          marice.dati <- rbind(marice.dati,nuovaRiga)
        }
        colnames(marice.dati)<-c("data","evento")
        
        res <- playSingleSequence( matriceSequenza = marice.dati, 
                                   col.eventName = "evento", 
                                   col.dateName = "data" , 
                                   IDPaz = indice.parola,
                                   date.format = date.format, UM = UM )
        # scorri tutta la storia alla ricerca di qualche hop che non ha 
        # scatenato un trigger. Se lo trovi, la parola e' sbagliata!
        parola.corretta <- TRUE
        for(indice.hops in names(res$history.hop)) {
          if(length(res$history.hop[[ indice.hops ]]$active.trigger)==0) parola.corretta<-FALSE
          # tuttavia se quanto analizzato ora e' relativo ad un nodo END, non proseguire oltre
          # (cio' che c'e' dopo, ipotizzo che non mi interessi)
          arr.nodi.attivati <- res$history.hop[[ indice.hops ]]$st.ACTIVE
          stop.search.END<-FALSE
          for( tmp.run in arr.nodi.attivati){
            tmp.run<-str_replace_all(string = tmp.run,pattern = "'",replacement = "")
            if(tmp.run!="BEGIN"){
              if(WF.struct$info$stati[[ tmp.run ]]$type=="END") stop.search.END<-TRUE
            }
          }
          if(stop.search.END==TRUE) break;
        }
        arr.matching.parola<-c(arr.matching.parola,parola.corretta)
      }
    }

    # dichiara certamente vere quelle iniziali, quelle non shuffellate

    arr.matching.parola<-c(arr.matching.parola,rep(TRUE,number.of.cases-quante.da.mescolare))
    valid.csv<-obj.utils$format.data.for.csv(listaProcessi = lista.res$list.LOGs,
                                             lista.validi = arr.matching.parola, 
                                             typeOfRandomDataGenerator= typeOfRandomDataGenerator)
    valid.data.frame<-as.data.frame(valid.csv)
    
    
    # E mo' ritorna tutto il BOLO!
    return(list( "lista.parole"=lista.res,
                 "arr.matching.parola" = arr.matching.parola,
                 "valid.data.frame" = valid.data.frame
    ))
  } 
  #=================================================================================
  # genera.parola.valida
  #=================================================================================   
  genera.parola.valida<-function(number.of.cases, max.word.length=100, parola.valida = TRUE) {
    stringhe<- unlist(xpathApply(WF.xml,str_c('//xml/workflow/trigger/condition'),xmlValue  )  )
    arr.parole<-get.possible.words.in.WF.easy()
    # Inizializza gli array
    list.LOGs<-list()
    list.nodes<-list()

    # popola l'array dei tempi di uptime degli stati (di attivazione)
    lista.stati.possibili <- names(WF.struct$info$stati)
    st.ACTIVE.time<-rep( 0 , length(lista.stati.possibili) )
    names(st.ACTIVE.time) <- lista.stati.possibili
    # e crea anche il cumulativo
    st.ACTIVE.time.cum <- st.ACTIVE.time
    
    
    # Genera il numero desiderato di parole
    for( num.parola in seq(1,number.of.cases)) {
      # initialization
      st.LAST<-"";  st.DONE<-c(""); st.ACTIVE<-c("'BEGIN'")
      last.fired.trigger<-c(); 
      arr.low.level<-c()
      list.high.level<-list()
      terminate.run = FALSE
      # cat("\n --------------------- ")
      # genera il numero di sequenze desiderate
      for( indice in seq(1, max.word.length)) {
        # costruisci l'array delle parole possibili, rimescolato
        # (per evitare starvation)
        arr.parole.sampled <- sample(x = arr.parole,size = length(arr.parole))
        
        # loopa su tutte la parole disponibili, cercando di uscirne   
        # ( in realta' loop infiniti sono teoricamente possibili)
        trovato.qualcosa <- FALSE
        
        for( ev.NOW in arr.parole.sampled){
          newHop <- attiva.trigger( st.LAST = st.LAST, 
                                    ev.NOW = ev.NOW, 
                                    st.DONE = st.DONE, 
                                    st.ACTIVE = st.ACTIVE, 
                                    st.ACTIVE.time = st.ACTIVE.time,
                                    st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                    EOF = FALSE   )
          if( !is.null(newHop$active.trigger ) & newHop$error == FALSE) {
            trovato.qualcosa <- TRUE
            # cat("\n ",ev.NOW)
            break;
          }
        }
        
        if(trovato.qualcosa==FALSE){
          stop("ERROR: non ci sono eventi che consentono di far evolvere la stringa")
        }        

        # ora dovrei avere la nuova parola
        arr.low.level <- c(arr.low.level,ev.NOW)
        list.high.level[[ as.character(length(arr.low.level)) ]] <- newHop$st.ACTIVE
        st.ACTIVE <- newHop$st.ACTIVE
        st.LAST <- newHop$st.LAST
        st.DONE <- newHop$st.DONE
        # Se uno degli stati raggiunti e' di END, ferma la corsa 
        # (la parola e' finita)
        for(nomeStato in st.ACTIVE) {
          nomeStato<-str_replace_all(string = nomeStato,pattern = "'",replacement = "")
          if(WF.struct$info$stati[[ nomeStato ]]$type=="END") terminate.run <- TRUE
        }
        
        if(length(arr.low.level)>max.word.length) terminate.run <- TRUE
        if(terminate.run==TRUE) break;
      }
      list.LOGs[[ as.character(num.parola) ]] <- arr.low.level
      list.nodes[[ as.character(num.parola) ]] <- list.high.level
    }
    
    # restituisci la stringa valida
    return(  
      list("list.LOGs"   = list.LOGs,
           "list.nodes" = list.nodes)
    )
  }

  #=================================================================================
  # get.possible.words.in.WF.easy
  # Parsa l'XML dello PWF gia' in memoria per costruire un array dei possibili 
  # $ev.NOW$ (gli eventi dell'event log).
  #=================================================================================   
  get.possible.words.in.WF.easy<-function() {
    stringhe<- unlist(xpathApply(WF.xml,str_c('//xml/workflow/trigger/condition'),xmlValue  )  )
    arr.parole<-c()
    for(stringa in stringhe) {
      matched = str_locate_all(stringa,pattern = "(?<=ev.NOW.==').*?(?=')")
      # Se hai trovato almeno una occorrenza
      if(dim(matched[[1]])[1]>0){
        # allora estrai la sottostringa
        for(i in 1:dim(matched[[1]])[1]){
          tmp.1 <- str_sub(string = stringa,start = matched[[1]][i,1],end = matched[[1]][i,2])
          if(!is.na(tmp.1)) {
            # e popola l'array dei possibili eventi
            if( !(tmp.1 %in% arr.parole)) arr.parole<-c(arr.parole,tmp.1)
          }
        }
      }
      
    }

    return(arr.parole)
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
    play.output.format.date <<- "%d/%m/%Y";
    obj.LogHandler <<- logHandler();
    list.computation.matrix <<-list();
    list.computation.matrix$stati.transizione <<- c()
    list.computation.matrix$trigger <<- c()
    list.computation.matrix$stati.finali <<- c()    
    list.computation.matrix$stati.timeline <<- list()
  }
  costructor( verboseMode = verbose.mode);
  #================================================================================= 
  return(list(
    "loadWorkFlow"=loadWorkFlow,
    "loadDataset"=loadDataset,
    "play"=play,  # rimpiazza la play.easy
    "replay"=replay, # rimpiazza la playLoadedData
    "plot"=plot, # rimpiazza la plotGraph
    
    "plot.replay.result"=plot.replay.result, # rimpiazza la plotComputationResult

    "get.list.replay.result"=get.list.replay.result, # rimpiazza la getPlayedSequencesStat.00
    "get.XML.replay.result"=get.XML.replay.result, # rimpiazza la getXML
    # "plotPatientEventTimeLine" = plotPatientEventTimeLine,
    # "plotPatientReplayedTimeline" = plotPatientReplayedTimeline, # rimpiazza la plotPatientComputedTimeline
    
    "getPatientLog"=getPatientLog,
    
    "getPatientXML"=getPatientXML,
    "giveBackComputationCounts"=giveBackComputationCounts
    
    # "play.easy"=play.easy
    # "playLoadedData"=playLoadedData,
    # "plotGraph"=plotGraph,
    # "plotComputationResult"=plotComputationResult
    # "getPlayedSequencesStat.00"=getPlayedSequencesStat.00,
    # "getXML"=getXML,
    # "plotPatientComputedTimeline" = plotPatientComputedTimeline,
  ))
}