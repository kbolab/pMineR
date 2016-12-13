#' firstOrderMarkovModel class
#' 
#' @description  This is an implementation of the First Order Markov Model (FOMM) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'                \itemize{
#'                \item \code{firstOrderMarkovModel( ...) } is the costructor
#'                \item \code{loadDataset( ...) } loads data into a FOMM object
#'                \item \code{trainModel() } trains a FOMM model
#'                \item \code{getModel() } returns the model (XML or graphical way, via grViz script)
#'                \item \code{replay( ... )} submits a set of given sequences and returns the fitting (not yet implemented for this algorithm)
#'                \item \code{play( ... )} generates a wished number of sequences
#'                \item \code{plot() } plots the graph 
#'                \item \code{distanceFrom( ... )} allows to calculate the distance between two different FOMM objects
#'                \item \code{plot.delta.graph( ... ) } plots a distance graph between two given FOMM objects
#'                \item \code{get.transition.Prob( ... ) } calculate the probability to reach a state in K transitions at least
#'                \item \code{getTimeProb( ... ) } calculate the probability to reach a state in K days
#'                }
#'              There are two ways to use this class: directly using the methods previously 
#'              listed or via wrapping functions (called PM.<method name>). In the examples section you will find an example of both.
#' @param parameters.list a list containing possible parameters to tune the model. The admitted element of the input list are:
#'   \itemize{
#'    \item \code{threshold } a number between 0 and 1 (default is 0). In the graph, arcs with a probability under the threshold will be removed;
#'    \item \code{considerAutoLoop } a boolean parameter (default is \code{TRUE}). If \code{FALSE} the arcs outcoming and incoming in the same node will be removed.
#'   }
#' @useDynLib pMineR    
#' @export
#' @examples \dontrun{
#' # ----------------------------------------------- 
#' #  USING THE METHODS of the class
#' # ----------------------------------------------- 
#' obj.L<-dataLoader();   # create a Loader
#' 
#' # Load a .csv using "DES" and "ID" as column names to indeicate events 
#' # and Patient's ID
#' obj.L$load.csv(nomeFile = "./otherFiles/test_02.csv",IDName = "ID",
#' EVENTName = "DES")
#' 
#' # now create an object firstOrderMarkovModel
#' obj.MM<-firstOrderMarkovModel();    
#' 
#' # load the data into MM model
#' obj.MM$loadDataset( obj.L$getData() );  
#' 
#' # train the model
#' obj.MM$trainModel();  
#' 
#' # plot the model 
#' obj.MM$plot();  
#' 
#' # -----------------------------------------------
#' ##  USING THE WRAPPER Functions
#' # -----------------------------------------------
#' # Instantiate a 'firstOrderMarkovModel' model
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "./otherFiles/test_02.csv",
#'    IDName = "ID",EVENTName = "DES")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' 
#' # Load the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#'
#' # train it
#' PM.trainModel(PM.obj = obj.PM)
#' 
#' # plot the model 
#' PM.plot(PM.obj = obj.PM)
#' 
#' }
firstOrderMarkovModel<-function( parameters.list = list() ) {
  MMatrix<-''
  footPrint<-''
  model.grViz<-'';
  model.XML<-'';
  is.dataLoaded<-FALSE  
  parameters<-list()
  MMatrix.perc<-NA
  MMatrix.perc.noLoop<-NA
  MMatrix.mean.time<-NA
  MMatrix.density.list<-NA  
  istanceClass<-list()
  obj.log<-NA
  
  # ***************************************************************************************************
  # WRAPPING METHODS
  # ***************************************************************************************************
  #===========================================================
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) { 
    transMatrix<-dataList$MMatrix
#     footPrintTable<-dataList$footPrint
    MMatrix<<-transMatrix

    # calcola la matrice delle percentuali e quella delle percentuali senza i loop
    # MMatrix.perc
    MM<-MMatrix;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
    MMatrix.perc<<-MM
    
    # MMatrix.perc.noLoop
    MM<-MMatrix;
    diag(MM)<-0;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
    MMatrix.perc.noLoop<<-MM     
    
    # MM.mean.time and MM.density.list
    MMatrix.mean.time<<-dataList$MM.mean.time
    MMatrix.density.list<<-dataList$MM.density.list
    
    
#     if(!is.null(parameters$threshold)) threshold<-parameters$threshold
#     else threshold<-0
#     
#     if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
#     else considerAutoLoop <- TRUE
#     
    # dichiara che i dati sono stati caricati
    is.dataLoaded<<-TRUE
  }  
  #===========================================================
  # play
  #===========================================================
  play<-function(numberOfPlays = 1, min.num.of.valid.words = NA) {
    obj.utils <- utils()
    res<-list()
    for(i in seq(1,numberOfPlays)) {
      res[[as.character(i)]]<-play.Single()
    }
    
    # kkk
    # Se devi generare alcune sequenze invalida, provvedi
    arr.quanti.invalidi <- c()
    if(!is.na(min.num.of.valid.words)) {
      
      sequenze.da.invalidare <- numberOfPlays - min.num.of.valid.words
      if(sequenze.da.invalidare>0) {
        
        sottomatrice <- MMatrix[ !(colnames(MMatrix) %in% c("BEGIN","END")), !(rownames(MMatrix) %in% c("BEGIN","END"))  ]
        posizione.zeri <- which(sottomatrice==0,arr.ind = TRUE)
        
        # for( i in names(res))  {
        for( i in seq(1,sequenze.da.invalidare))  {
          if( length(res[[i]])>1 ) {
            dado.innesto <- as.integer(runif(1,min=1,max =length(res[[i]])))
            dato.righe.matrice <- as.integer(runif(1,min=1,max = nrow(posizione.zeri)))
            res[[i]][dado.innesto] <- rownames(sottomatrice)[posizione.zeri[ dato.righe.matrice,1 ]]  
            res[[i]][dado.innesto+1] <- colnames(sottomatrice)[ posizione.zeri[ dato.righe.matrice,2 ]]
          }
          arr.quanti.invalidi<-c(arr.quanti.invalidi,rep(FALSE,length(res[[i]])))
        }
      }
    }
    # kkk
 # browser()      
    res <- obj.utils$format.data.for.csv(listaProcessi = res, lista.validi = rep(TRUE,numberOfPlays))
    # if(is.null(dim(res))) browser()
    # browser()
    if(length(arr.quanti.invalidi)>=0 & !is.null(arr.quanti.invalidi)) res[,"valido"][1:length(arr.quanti.invalidi)]<-arr.quanti.invalidi
    
        
    if(!is.null(dim(res))) res<-as.data.frame(res)
    return(res)
  }
  #=================================================================================
  # distanceFrom - WRAPPER Function
  # Funzione WRAPPER per il calcolo delle distanze rispetto ad un oggetto omologo
  #=================================================================================   
  distanceFrom<-function( objToCheck, metric="default") {
    
    if( metric == "default" ) return( distanceFrom.default( objToCheck = objToCheck) )
    if( metric == "binaryCount" ) return( distanceFrom.binaryCount( objToCheck = objToCheck) )
    
    obj.log$sendLog(msg= "The requested metric is not yet available" , type="NMI" )
  }  
  #===========================================================
  # trainModel
  #===========================================================
  trainModel<-function(debug.mode = FALSE) {
    # setta la soglia a zero, cosi' per sport...
    if(!is.null(parameters$threshold)) threshold<-parameters$threshold
    else threshold<-0
    if(!is.null(parameters$considerAutoLoop)) {
      considerAutoLoop<-parameters$considerAutoLoop
    } 
    else 
    {
      considerAutoLoop<-TRUE
    }
  if(debug.mode==TRUE) browser()
    # copia la tabella delle  transizioni in una un po' piu' facile 
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) { MM<-MMatrix.perc; }
    else {
      diag(MMatrix)<<-0
      diag(MMatrix.mean.time) <<- Inf
      diag(MMatrix.perc)<<-0
      MM <- MMatrix.perc.noLoop
    }
    
    # sistema la threshold 
    aa<- MMatrix.perc; bb <- MMatrix
    aa[ which(aa<=threshold,arr.ind = T) ]<-0
    bb[ which(bb<=threshold,arr.ind = T) ]<-0
    for( i in seq( 1 , nrow(aa)) ) {if(sum(aa[i,])>0)  {aa[i,]<-aa[i,]/sum(aa[i,]);} } 
    MMatrix.perc<<-aa ; MMatrix<<-bb
    
    grafo<-build.graph.from.table( MM = MM, threshold  = threshold)
    
    model.grViz<<-grafo;
  }
  #===========================================================
  # replay
  #===========================================================
  replay<-function( dataList , debugMode = FALSE, col.toCheckPerformances=NA ) {
    res<-list()
    res$words<-list()
    res$success<-c()
    declared.correctness<- c()
    if(debugMode == TRUE) browser()
    for(patId in names(dataList$pat.process)) {
      parola <- unlist(dataList$pat.process[[patId]][[dataList$csv.EVENTName]])
      parola <- c("BEGIN",parola)
      success <- TRUE;
      path.attuale<-c()
      # if(patId==82) browser()
      for( caratt.i in seq(1,(length(parola)-1)) ) {
        # if( parola[ caratt.i  ] =="RX" & parola[ caratt.i+1 ] =="RX" ) browser()
        caratt.s <- parola[ caratt.i  ]
        if(!(caratt.s %in% colnames(MMatrix.perc))) { success = FALSE; break; }
        if(!(parola[ caratt.i +1 ] %in% colnames(MMatrix.perc))) { success = FALSE; break; }
        
        # jump.prob <- dataList$MMatrix.perc[ parola[ caratt.i  ], parola[ caratt.i+1 ]  ]
        
        jump.prob <- MMatrix.perc[ parola[ caratt.i  ], parola[ caratt.i+1 ]  ]
        if(jump.prob>0) path.attuale <- c(path.attuale,parola[ caratt.i  ])
        if(jump.prob==0) { success = FALSE; break; }
        caratt.s %in% colnames(MMatrix.perc)
      }
      res$words[[patId]]<-path.attuale
      res$success<-c(res$success,success)
      
      if( !is.na(col.toCheckPerformances) )  {
        declared.correctness <- c(declared.correctness,as.character(dataList$pat.process[[patId]][[col.toCheckPerformances]][1]))
      }
    }
    
    if( !is.na(col.toCheckPerformances) ) {
      conf.matrix <- table(res$success, declared.correctness)
      res$performances <- calculate.performances(conf.matrix)
    }
    return( res )
  }
  #===========================================================
  # calculate.performances 
  #===========================================================
  calculate.performances<-function( conf.matrix  ) {
    # browser()
    # calculate accuracy
    if( "TRUE" %in% rownames(conf.matrix) & "TRUE" %in% colnames(conf.matrix)) {
      TP <- conf.matrix[ "TRUE","TRUE" ]
    } else { TP <- 0 }
    if( "FALSE" %in% rownames(conf.matrix) & "FALSE" %in% colnames(conf.matrix)) {
      TN <- conf.matrix[ "FALSE","FALSE" ]
    } else { TN <- 0 }
    if( "TRUE" %in% rownames(conf.matrix) & "FALSE" %in% colnames(conf.matrix)) {
      FP <- conf.matrix[ "TRUE","FALSE" ]
    } else { FP <- 0 }
    if( "FALSE" %in% rownames(conf.matrix) & "TRUE" %in% colnames(conf.matrix)) {
      FN <- conf.matrix[ "FALSE","TRUE" ]
    } else { FN <- 0 }  
    
    return(list(
      "TP" = TP,
      "TN" = TN,
      "FP" = FP,
      "FN" = FN,
      "accuracy" = (TP + TN)/(TP + TN + FP + FN),
      "sensitivity" = (TP )/(TP + FN ),
      "specificity" = (TN )/(FP + TN ),
      "precision" = (TP )/(TP + FP ),
      "recall" = (TP )/(TP + FN ),
      "F1score" = ( 2 * TP )/( 2 * TP + FP + FN ),
      "conf.matrix"=conf.matrix
      ))
  } 
  #===========================================================
  # findReacheableNodes
  # Funzione 
  #===========================================================
  findReacheableNodes<-function( nodoDiPatenza = 'BEGIN'  ) {
    
    findReacheableNodes.recursiveLoop(
      nodoAttuale = nodoDiPatenza,
      nodi.raggiunti = c(nodoDiPatenza)
    )
    
  }   
  findReacheableNodes.recursiveLoop<-function( nodoAttuale , nodi.raggiunti  ) {
    
    lista.nodi <- colnames(MMatrix.perc)
    nodi.raggiunti <- unique(c(nodi.raggiunti,nodoAttuale))
     # browser()
    for( nodoDestinazione in lista.nodi) {
      if( !(nodoDestinazione %in% nodi.raggiunti ) & MMatrix.perc[nodoAttuale,nodoDestinazione]>0) {
        aa <- findReacheableNodes.recursiveLoop( nodoAttuale = nodoDestinazione , 
                                                 nodi.raggiunti = nodi.raggiunti)
        nodi.raggiunti <- unique(c(nodi.raggiunti , aa))
      }
    }
    return(nodi.raggiunti)
  }   
  #===========================================================
  # convert2XML - future
  #===========================================================
  convert2XML<-function(  ) {
  }   
  #===========================================================
  # getModel
  #===========================================================
  getModel<-function(kindOfOutput) {
    if(kindOfOutput=="XML") return( model.XML )
    if(kindOfOutput=="grViz") return( model.grViz )
    if(kindOfOutput=="MMatrix") return( MMatrix )
    if(kindOfOutput=="MMatrix.perc") return( MMatrix.perc )

    obj.log$sendLog(msg= "The requested model is not available yet" , type="NMI" )
  }
  #===========================================================
  # plot
  #===========================================================
  plot<-function(){
    grViz( getModel(kindOfOutput = "grViz" ) )
  }  
  #===========================================================
  # setIstanceClass
  #===========================================================
  setInstanceClass<-function( className, classType = "default") {
    istanceClass[[classType]]<-className
  }
  #===========================================================
  # setIstanceClass
  #===========================================================
  getInstanceClass<-function( className, classType = "default") {
    return(istanceClass[[classType]])
  }  
  #===========================================================
  # getLogObj
  #===========================================================  
  getLogObj<-function() {
    return(obj.log)
  }
  #===========================================================
  # setLogObj
  #===========================================================  
  setLogObj<-function( objLog ) {
    obj.log<<-objLog
  }  
  
  # ***************************************************************************************************
  # MODEL SPECIFIC PUBLIC METHODS
  # ***************************************************************************************************   
  #===========================================================
  # plot.delta.graph
  # Funzione per il plotting delle distanze rispetto ad una data metrica
  #===========================================================
  plot.delta.graph<-function( objToCheck, threshold=0, type.of.graph="delta", threshold.4.overlapped=.3 ) {
    
    if( type.of.graph != "overlapped" & type.of.graph !="delta") stop("\n Not yet implemented: err.cod. %43547g8fd")
    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc    
    
    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)
    listaStati<-unique(combinazioni[,1])
    matrice<-array(0,dim=c(length(listaStati),length(listaStati)))
    m.int<-matrice; m.ext<-matrice;  
    colnames(matrice)<-listaStati;     rownames(matrice)<-listaStati
    colnames(m.int)<-listaStati;     rownames(m.int)<-listaStati
    colnames(m.ext)<-listaStati;     rownames(m.ext)<-listaStati    
    
    for(riga in seq(1,nrow(combinazioni))) {
      matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- abs(combinazioni[riga,"int"] - combinazioni[riga,"ext"])
      m.int[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- combinazioni[riga,"int"]
      m.ext[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- combinazioni[riga,"ext"]
#      if( matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ] <threshold) matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<-0
    }
    
    if(type.of.graph=="delta")
      grafo<-build.graph.from.table(MM = matrice, threshold = threshold) 
    if(type.of.graph=="overlapped")
      grafo<-build.graph.from.table(MM = m.int, threshold = threshold, 
                                    second.MM = m.ext, 
                                    threshold.second.MM = threshold.4.overlapped, type.of.graph = type.of.graph) 
    
    grViz(grafo);
  }   
  #=================================================================================
  # get.time.transition.Prob
  # It tries to predict the probability to reach a final state (starting from a known starting state)
  # in "at least" K days
  #   initialState : the initial state
  #   finalState : the final state
  #   num.of.transitions : the max number of allowed days for reaching the final state
  #   debugString : (TRUE/FALSE) is a debug string print for debuggin issues
  #   killAutoLoop : (TRUE/FALSE) suppress autoloop during computation?
  #=================================================================================    
  get.time.transition.Prob<-function( initialState, finalState, maxTime,debugString=NA,killAutoLoop=FALSE){
    a<-getTimeProb( timeAttuale=0, maxTime =maxTime, 
                statoAttuale=initialState, statoGoal =finalState, 
                debugString=debugString,killAutoLoop=killAutoLoop, comsumedTime=0)
    return(a)
  }   
  getTimeProb<-function( timeAttuale=0, maxTime =1, statoAttuale="BEGIN", statoGoal ="END", debugString=NA,killAutoLoop=FALSE,comsumedTime=0) {
    if(is.na(debugString)) debugString<-statoAttuale;
    
    if( statoAttuale==statoGoal ) {
      cat('\n ',debugString);
      return( 1 );
    }  
    
    if( timeAttuale > maxTime & (statoAttuale!=statoGoal) ) {
      return( 1 );
    }  
    # MMPerc<-getAttribute(attributeName = matriceDaConsiderare)
    if( killAutoLoop == FALSE ) { MMPerc<-MMatrix.mean.time; MMProb<-MMatrix.perc }
    if( killAutoLoop == TRUE ) { stop("Not yet implemented err.cod -hg85h78g4578") }
    prob<-0; 
    
    for( possibileNuovoStato in rownames(MMPerc)) {
      giorni.stimati.per.transizione<-MMPerc[statoAttuale,possibileNuovoStato]
      if(giorni.stimati.per.transizione<1) giorni.stimati.per.transizione=1
      if(MMPerc[statoAttuale,possibileNuovoStato]!=Inf & (timeAttuale+giorni.stimati.per.transizione) <= maxTime ) {
        newdebugString<-paste(c(debugString,'==(',as.character(giorni.stimati.per.transizione),',',(timeAttuale+giorni.stimati.per.transizione),',',MMProb[statoAttuale,possibileNuovoStato],')==>',possibileNuovoStato),collapse='');
        addendo<-MMProb[statoAttuale,possibileNuovoStato] * getTimeProb( timeAttuale+giorni.stimati.per.transizione, maxTime,  possibileNuovoStato ,  statoGoal , debugString = newdebugString, killAutoLoop=killAutoLoop);
        prob<-prob + addendo
      }
    }  
    return(prob);
  }    
  #=================================================================================
  # build.PWF
  # IT builds a Pseudo-Workflow XML file
  #=================================================================================  
  build.PWF<-function() {
    testo <- "<xml>"
    testo <- str_c(testo,"\n\t<workflow>")
    
    # ora i nodi
    nomeNodi <- colnames(MMatrix)[!(colnames(MMatrix) %in% "BEGIN")]
    for( i in nomeNodi ) {
      testo <- str_c(testo,"\n\t<node name=\"",i,"\" />")
    }
    # Ora i trigger
    for(from.node in rownames(MMatrix)) {
      for(to.node in colnames(MMatrix)) {
        if(MMatrix[from.node,to.node] >0) {
          testo <- str_c(testo, "\n\t <trigger name=\"from.",from.node,".to.",to.node,"\">")
          testo <- str_c(testo, "\n\t\t <condition>")
          testo <- str_c(testo, "\n\t\t\t","\"",from.node,"\" %in% $st.ACTIVE$ AND $ev.NOW$==\"",to.node,"\"")
          testo <- str_c(testo, "\n\t\t </condition>")
          testo <- str_c(testo, "\n\t\t <unset>'",from.node,"'</unset>")
          testo <- str_c(testo, "\n\t\t <set>'",to.node,"'</set>")
          testo <- str_c(testo, "\n\t </trigger>")
        }
      }
    }
    testo <- str_c(testo,"\n\t</workflow>")
    testo <- str_c(testo,"\n</xml>")
    
    return(testo)
  }
  #=================================================================================
  # get.transition.Prob
  # It tries to predict the probability to reach a final state (starting from a known starting state)
  # in "at least" K transitions
  #   initialState : the initial state
  #   finalState : the final state
  #   num.of.transitions : the max number of allowed transitions for reaching the final state
  #   debugString : (TRUE/FALSE) is a debug string print for debuggin issues
  #   killAutoLoop : (TRUE/FALSE) suppress autoloop during computation?
  #=================================================================================  
  get.transition.Prob<-function( initialState, finalState, num.of.transitions,debugString=NA,killAutoLoop=FALSE){
    a<-getProb( stepAttuale=0, maxNumStep =num.of.transitions, 
                          statoAttuale=initialState, statoGoal =finalState, 
                          debugString=debugString,killAutoLoop=killAutoLoop)
      return(a)
  } 
  getProb<-function( stepAttuale=0, maxNumStep =1, statoAttuale="BEGIN", statoGoal ="END", debugString=NA,killAutoLoop=FALSE) {
    if(is.na(debugString)) debugString<-statoAttuale;

    if( statoAttuale==statoGoal ) {
      cat('\n ',debugString);
      return( 1 );
    }  
    if( stepAttuale == maxNumStep & (statoAttuale!=statoGoal) ) {
      return( 0 );
    }  
    # MMPerc<-getAttribute(attributeName = matriceDaConsiderare)
    if( killAutoLoop == FALSE ) MMPerc<-MMatrix.perc
    if( killAutoLoop == TRUE ) MMPerc<-MMatrix.perc.noLoop
    prob<-0; 
    for( possibileNuovoStato in rownames(MMPerc)) {
      if(MMPerc[statoAttuale,possibileNuovoStato]>0) {
        newdebugString<-paste(c(debugString,'=>',possibileNuovoStato),collapse='');
        addendo<-MMPerc[statoAttuale,possibileNuovoStato] * getProb( stepAttuale+1, maxNumStep,  possibileNuovoStato ,  statoGoal , debugString = newdebugString, killAutoLoop=killAutoLoop);
        prob<-prob + addendo
      }
    }  
    return(prob);
  }  
  #===========================================================
  # play.Single
  #===========================================================  
  play.Single<-function() {
    
    ct<-1;
    res<-c();
    if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
    else considerAutoLoop<-TRUE  

    if ( !("END" %in% findReacheableNodes(nodoDiPatenza = "BEGIN") )) {
      return(c() ) ;
    }
    
    # copia la tabella delle transizioni in una un po' piu' facile 
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) MM<-MMatrix.perc
    else MM<-MMatrix.perc.noLoop
    # cat("\n iniziato un single play")
    statoAttuale<-"BEGIN"
    while( statoAttuale != "END") {
      # print(statoAttuale)
      sommaCum<-cumsum(MM[statoAttuale,])
      dado<-runif(n = 1,min = 0,max = 0.99999999999999)
      # dado<-runif(n = 1,min = 0,max = max(sommaCum)-0.00001)
      posizione<-which( (cumsum(MM[statoAttuale,])-dado)>=0  )[1]
      nuovoStato<-colnames(MM)[posizione]
      # cat("\n",nuovoStato)
      if(is.na(nuovoStato)) browser()
      if ( ("END" %in% findReacheableNodes(nodoDiPatenza = nuovoStato) )) {
        res<-c(res,statoAttuale)
        statoAttuale<-nuovoStato
      }
      

      # cat("\n ------------------------------------------\n ",res)
    }
    res<-c(res,"END")
    res<-res[ which( !(res %in%  c('BEGIN','END') ))    ] 
    return(res);
  }  
  #===========================================================
  # build.graph.from.table
  #===========================================================
  build.graph.from.table<-function(MM, threshold, second.MM = NA, threshold.second.MM=.2 , type.of.graph= "delta") {
    
    if( type.of.graph != "overlapped" & type.of.graph !="delta") stop("\n Not yet implemented: err.cod. %43547g8fd")
    
    # prendi la lista dei nomi
    listaNodi<-colnames(MM)
    # la lista dei nodi raggiungibili da BEGIN
    #listaNodiFromBegin<-listaNodi[which(MM["BEGIN",]!=threshold)]
    listaNodiFromBegin<-listaNodi[which(MM["BEGIN",]>threshold)]
    # la lista dei nodi che vanno a END
    #listaNodiToEnd<-listaNodi[which(MM[,"END"]!=threshold)]
    listaNodiToEnd<-listaNodi[which(MM[,"END"]>threshold)]
    
    rigaBEGIN<-''
    
    for( i in listaNodiFromBegin) {
      rigaBEGIN<-paste(   c(rigaBEGIN, "'BEGIN'->'",i,"' "), collapse = '') 
    }    
    rigaEND<-''
    for( i in listaNodiToEnd) {
      rigaEND<-paste(   c(rigaEND, "'",i,"'->'END' "), collapse = '') 
    }        
    
    stringaNodiComplessi<-''
    for(i in seq(1,nrow(MM))) {
      #listaNodiRiga<-listaNodi[which(MM[i,]!=threshold)]
      listaNodiRiga<-listaNodi[which(MM[i,]>=threshold)]
      if(length(listaNodiRiga)>0) {
        for( ct in seq(1,length(listaNodiRiga))) {

          peso<-round(as.numeric(MM[i, listaNodiRiga[ct]]),digits = 2)
          # if(peso==0) peso = 0.01
          penwidth<- peso*3 + 0.01
          if(penwidth<0.4) penwidth=0.4
          fontSize = 5+peso*9
          colore = as.integer(100-(30+peso*70))
          if( type.of.graph == "overlapped") {
            second.peso<-round(as.numeric(second.MM[i, listaNodiRiga[ct]]),digits = 2)
            # if(second.peso==0) second.peso = 0.01
            if( abs(peso - second.peso) >= threshold.second.MM ) {
              delta.peso<-round(as.numeric((peso - second.peso)),digits = 2)
              penwidth<- max(peso,abs(delta.peso))*3 + 0.01
              fontSize = 5+max(peso,abs(delta.peso))*9
              if(delta.peso>0) colore.delta.peso<-"Red"
              else colore.delta.peso<-"Green"
               if(peso > threshold | second.peso > threshold)
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"/",second.peso,"', style='dashed', fontcolor='",colore.delta.peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = ",colore.delta.peso,"]\n"), collapse = '')   
            } else{
               if(peso > threshold)
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')   
            }
          } else {
             if(peso > threshold)
              stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')   
          }
        }
      }
    }
    listaNodiToPrint<-''
    for(i in seq(1,length(listaNodi))) {
      if(i<length(listaNodi)) listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"';"), collapse=''    )
      else listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"'"), collapse=''    )
    }
    # now plot it
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
             'END'; 
             
             node [fillcolor = orange]
             ",listaNodiToPrint,"
             
             edge [arrowsize = 1 ]
             # several edge
             ",stringaNodiComplessi,"
    }"), collapse='')       
    return(a)
    #model.grViz<<-a;
    #    model.XML<<-strutturaXML    
  }   
  old.build.graph.from.table<-function(MM, threshold, second.MM = NA, threshold.second.MM=.3) {
    # prendi la lista dei nomi
    listaNodi<-colnames(MM)
    # la lista dei nodi raggiungibili da BEGIN
    #listaNodiFromBegin<-listaNodi[which(MM["BEGIN",]!=threshold)]
    listaNodiFromBegin<-listaNodi[which(MM["BEGIN",]>threshold)]
    # la lista dei nodi che vanno a END
    #listaNodiToEnd<-listaNodi[which(MM[,"END"]!=threshold)]
    listaNodiToEnd<-listaNodi[which(MM[,"END"]>threshold)]
    
    rigaBEGIN<-''
    
    for( i in listaNodiFromBegin) {
      rigaBEGIN<-paste(   c(rigaBEGIN, "'BEGIN'->'",i,"' "), collapse = '') 
    }    
    rigaEND<-''
    for( i in listaNodiToEnd) {
      rigaEND<-paste(   c(rigaEND, "'",i,"'->'END' "), collapse = '') 
    }        
    
    stringaNodiComplessi<-''
    for(i in seq(1,nrow(MM))) {
      #listaNodiRiga<-listaNodi[which(MM[i,]!=threshold)]
      listaNodiRiga<-listaNodi[which(MM[i,]>threshold)]
      if(length(listaNodiRiga)>0) {
        for( ct in seq(1,length(listaNodiRiga))) {
          peso<-round(as.numeric(MM[i, listaNodiRiga[ct]]),digits = 2)
          if(peso==0) peso = 0.01
          penwidth<- peso*3
          if(penwidth<0.4) penwidth=0.4
          fontSize = 5+peso*9
          colore = as.integer(100-(30+peso*70))
          stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '') 
          if( !is.na(second.MM)) {
            browser()
          }
        }
      }
      #      stringaNodiComplessi<-paste( c(stringaNodiComplessi, "\n"), collapse='') 
    }
    listaNodiToPrint<-''
    for(i in seq(1,length(listaNodi))) {
      if(i<length(listaNodi)) listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"';"), collapse=''    )
      else listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"'"), collapse=''    )
    }
    
    # now plot it
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
             'END'; 
             
             node [fillcolor = orange]
             ",listaNodiToPrint,"
             
             edge [arrowsize = 1 ]
             # several edge
             ",stringaNodiComplessi,"
    }"), collapse='')       
    return(a)
    #model.grViz<<-a;
    #    model.XML<<-strutturaXML    
  }   
  #===========================================================
  # distanceFrom.default
  # Metrica di default. In questo caso la metrica di default e' semplicemente la somma
  # dei valori assoluti delle differenze di probabilita' fra le matrici di transizione 
  # di stato. 
  #===========================================================
  distanceFrom.default<-function( objToCheck) {

    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc
    
    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)
    
    distance<-sum(abs(combinazioni$ext - combinazioni$int))
    return( list(   "distance" = distance )      )
  }  
  #===========================================================
  # distanceFrom.binaryCount
  # Una banale metrica: costruisce una maschera binaria degli stati a transizione
  # con p()>0 e li setta a uno: a seguire fa la somma degli XOR fra le colonne
  # (conta '1' ogni volta che una e' '0' mentre l'altra e' '1')
  #===========================================================
  distanceFrom.binaryCount<-function( objToCheck) {
    
    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc
    
    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)
      
    combinazioni[ combinazioni[,"int"]>0, "int"] <-1
    combinazioni[ combinazioni[,"ext"]>0, "ext"] <-1

    distance<-sum(abs(combinazioni$ext - combinazioni$int))
    return( list(   "distance" = distance )      )
  }  
  #===========================================================
  # calcolaMatriceCombinazioni
  # Funzione di comodo che calcola in una matrice le differenze di probabilita'
  # fra due FSM. Di fatto e' un pre-processing per funzioni che calcolano metriche
  #===========================================================
  calcolaMatriceCombinazioni<-function( ext.MM, int.MM) {
    unione.nomi<-unique(c(colnames(ext.MM),colnames(int.MM)))
    combinazioni<-expand.grid(unione.nomi,unione.nomi)
    combinazioni<-cbind( combinazioni,  rep(0,nrow(combinazioni)  ) )
    combinazioni<-cbind( combinazioni,  rep(0,nrow(combinazioni)  ) )
    colnames(combinazioni)<-c("from","to","int","ext")
    combinazioni$from<-as.character(combinazioni$from)
    combinazioni$to<-as.character(combinazioni$to)
    
    for(riga in seq(1,nrow(combinazioni))) {
      
      if(  combinazioni[riga, "from"] %in%  colnames(int.MM)  &
           combinazioni[riga, "to"] %in%  colnames(int.MM)
      ) {
        combinazioni[riga, "int"]<-int.MM[  combinazioni[riga, "from"] , combinazioni[riga, "to"]     ]
      }
      if(  combinazioni[riga, "from"] %in%  colnames(ext.MM)  &
           combinazioni[riga, "to"] %in%  colnames(ext.MM)
      ) {
        combinazioni[riga, "ext"]<-ext.MM[  combinazioni[riga, "from"] , combinazioni[riga, "to"]     ]
      }
    }    
    return(combinazioni)  
  }
  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = list() ) {
    MMatrix<<-''
    footPrint<<-''
    model.grViz<<-'';
    model.XML<<-'';
    is.dataLoaded<<-FALSE
    parameters<<-parametersFromInput
    MMatrix.perc<<-NA
    MMatrix.perc.noLoop<<-NA  
    MMatrix.mean.time<<-NA
    MMatrix.density.list<<-NA
    istanceClass<<-list()
    obj.log<<-logHandler();
    setInstanceClass(className = "firstOrderMarkovModel")
  }
  #===========================================================
  costructor( parametersFromInput = parameters.list);
  #===========================================================
  return( list(
    "trainModel"=trainModel,
    "getModel"=getModel,
    "loadDataset"=loadDataset,
    "replay"=replay,
    "play"=play,
    "plot"=plot,
    "distanceFrom"=distanceFrom,
    "getLogObj"=getLogObj,
    "setLogObj"=setLogObj,
    "getInstanceClass"=getInstanceClass,
    "plot.delta.graph"=plot.delta.graph,
    "get.transition.Prob"=get.transition.Prob,
    "get.time.transition.Prob"=get.time.transition.Prob,
    "build.PWF"=build.PWF,
    "findReacheableNodes"=findReacheableNodes
  ) )  
}