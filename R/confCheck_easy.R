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
  playLoadedData<-function() {
    ct<-1
    for( indice in names(dataLog$wordSequence.raw)) {
      addNote(msg = str_c("\n\t<computation n='",ct,"'>"))
      playSingleSequence( sequenza = dataLog$wordSequence.raw[[ indice ]]  )
      addNote(msg = "\n\t</computation>")
      ct <- ct + 1
    }
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

    # Analizza TUTTI gli eventi della sequenza
    for( ev.NOW in sequenza ) {
      ct <- ct + 1
      addNote(msg = str_c("\n\t\t<step n='",ct,"'>"))
      addNote(msg = str_c("\n\t\t\t<event type='",ev.NOW,"'></event>"))
      for(i in st.ACTIVE) addNote(msg = str_c("\n\t\t\t<st.ACTIVE.PRE name=",i,"></st.ACTIVE.PRE>"))
      
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = ev.NOW, st.DONE = st.DONE, st.ACTIVE = st.ACTIVE  )
      
      # Se c'e' un errore, ferma tutto
      if(newHop$error==TRUE) {
        addNote(msg = str_c("\n\t\t\t<error>",newHop$error,"</error>"))
        addNote(msg = "\n\t\t</step>")
        return;
      }
      
      # Se hai rilevato dei trigger attivi
      if(length(newHop$active.trigger)!=0) {
        for(i in newHop$active.trigger) addNote(msg = str_c("\n\t\t\t<fired.trigger name='",i,"'></fired.trigger>"))
        for(i in newHop$st.ACTIVE) addNote(msg = str_c("\n\t\t\t<st.ACTIVE.POST name=",i,"></st.ACTIVE.POST>"))
        st.ACTIVE <- newHop$st.ACTIVE
      } else { 
        # altrimenti segnala che NON ci sono trigger attivi
        addNote(msg = str_c("\n\t\t\t<fired.trigger name=''></fired.trigger>"))
      }
      
      # Ora ripeti la ricerca dei trigger senza passare alcun evento, giusto per 
      # vedere i trigger che si possono eventualmente attivare a seguito di 
      # pregressi triggers.
      trigger.trovato<-TRUE
      # Continua a loopare fino a che e' vero che qualche trigger e' scattato
      while(  trigger.trovato == TRUE  ) {
        newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = "", st.DONE = st.DONE, st.ACTIVE = st.ACTIVE  )
        
        # Se c'e' un errore, ferma tutto
        if(newHop$error==TRUE) {
          ddNote(msg = str_c("\n\t\t\t<error>",newHop$error,"</error>"))
          addNote(msg = "\n\t\t</step>")
          return;
        }
        
        # Se hai rilevato qualche trigger attivo
        if(length(newHop$active.trigger)!=0)  {
          addNote(msg = "\n\t\t\t<event type=''></event>")
          for(i in st.ACTIVE) addNote(msg = str_c("\n\t\t\t<st.ACTIVE.PRE name=",i,"></st.ACTIVE.PRE>"))
          for(i in newHop$active.trigger) addNote(msg = str_c("\n\t\t\t<fired.trigger name='",i,"'></fired.trigger>"))
          for(i in newHop$st.ACTIVE) addNote(msg = str_c("\n\t\t\t<st.ACTIVE.POST name=",i,"></st.ACTIVE.POST>"))
          st.ACTIVE <- newHop$st.ACTIVE
        }
        # altrimenti (se non ci sono stati trigger, vedi di uscire dal loop)
        else trigger.trovato<-FALSE;
      }
      addNote(msg = "\n\t\t</step>")
    }
    return;
  }      
  #===========================================================  
  # addNote
  # it helps to note every noteable result
  #===========================================================    
  addNote<-function( msg, level='1', notebook.name='computationLog' ) { 
    
    # Crea la posizione, se ancora non c'è
    if(length(notebook)==0) notebook[[notebook.name]]<<-c()
    else {  if( !(notebook.name %in% names(notebook))) notebook[[notebook.name]]<<-c()  }
    # Accoda la nota
    notebook[[notebook.name]]<<-str_c(notebook[[notebook.name]],msg)
    return;
  }
  getNotebook<-function(notebook.name='computationLog'){
    return(notebook[[notebook.name]])
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
  plotGraph<-function() {
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

     # if(trigger.name=="attiva Imaging di Rivalutazione") browser()
       
       stringa.nodo.from<-str_c( stringa.nodo.from,"\n" )
       stringa.nodo.to<-str_c( stringa.nodo.to,"\n" )
       # Prendi i nodi 'unset' (from)
       arr.nodi.from<-unlist(WF.struct$info$trigger[[trigger.name]]$unset)
       # Prendi i nodi 'set' (to)
       arr.nodi.to<-unlist(WF.struct$info$trigger[[trigger.name]]$set)
       # Considera solo i nodi plottabili (from e to)
       arr.nodi.from <- arr.nodi.from [arr.nodi.from %in% arr.st.plotIt]
       arr.nodi.to <- arr.nodi.to [arr.nodi.to %in% arr.st.plotIt]

       
       # if(length(arr.nodi.from)>0 & length(arr.nodi.to)>0) {
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
  # getXML
  #===========================================================  
  getXML<-function( dataList ) {
    matrice <- obj.cc$getNotebood()
    testoXML <- "<WF.Analysis.results>"
    step.number <- 0
    for(riga in seq(1,nrow(matrice))) {
      stringa <- unlist(aa[riga,1])
      # BEGIN
      if(substr(x = stringa,start = 1,stop = str_length("BEGIN:"))=="BEGIN:") {
        computation <- numeric(str_replace_all(string = stringa,pattern = "BEGIN:",replacement = ""))
        testoXML<-str_c(testoXML,"\n\t<computation n='",computation,"'>")  
        step.number<-1
      }
      # END:
      if(substr(x = stringa,start = 1,stop = str_length("END:"))=="END:") {
        testoXML<-str_c(testoXML,"\n\t</computation>")  
      }    
      # END:
      if(substr(x = stringa,start = 1,stop = str_length("EVENT:"))=="EVENT:") {
        computation <- numeric(str_replace_all(string = stringa,pattern = "EVENT:",replacement = ""))
        testoXML<-str_c(testoXML,"\n\t</computation>")  
      }        
      
    }
    
    
  }    
  #===========================================================  
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) {
    dataLog <<- dataList
  }  
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    WF.xml <<- c()
    WF.xml.fileName <<- c()
    dataLog <<- c()
    WF.struct <<- list()
    notebook <<- list();
  }
  costructor();
  #================================================================================= 
  return(list(
    "loadWorkFlow"=loadWorkFlow,
    "loadDataset"=loadDataset,
    "playLoadedData"=playLoadedData,
    "getNotebook"=getNotebook,
    "getXML"=getXML,
    "plotGraph"=plotGraph
  ))
}

# TO DO:
# aggiungere un <semLink> nell'XML, all'interno di <trigger> per legare fra loro dei nodi (link direzionale molti a molti)
# gestire le trap tramite i 'trigger'. ? (invece di fare in <set> può fare altro, oppure può comunque fare il <set> di alcuni nodi  di tipo "error"? )
# Aggiunto controllo set/unset
# Aggiunta esecuzione EVENTO ''