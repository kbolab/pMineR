#' A class to train First Order Markov Models
#' 
#' @description  This is an implementation of the First Order Markov Model (FOMM) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'                \itemize{
#'                \item \code{firstOrderMarkovModel( ) } is the costructor
#'                \item \code{loadDataset( ) } loads data taken from a dataLoader::getData() method, into a FOMM object
#'                \item \code{trainModel( ) } train a model using the previously loaded dataset
#'                \item \code{replay( ) } re-play a given event log on the internal FOMM model
#'                \item \code{play( ) } play the internal FOMM model a desired number of times, in order to simulate new event-logs. This methods can also, if desired, simulate event-logs which does not complies with the internal FOMM model.
#'                \item \code{plot( ) } plot the internal model
#'                \item \code{distanceFrom( ) } calculate the scalar distance to another passed FOMM model, passed as argument. The default metric returns a scalar value
#'                \item \code{getModel( ) } return the trained internal FOMM model
#'                \item \code{getLogObj( ) } return an XML containing the execution-log of a \code{firstOrderMarkovModel::play()}  computation
#'                \item \code{getInstanceClass( ) } return the instance class Name and description (version, etc.)
#'                \item \code{plot.delta.graph( ) } plot a graph, in the desired modality, representing the difference between the internal FOMM and a passed one.
#'                \item \code{get.transition.Prob( ) } calculate the probability to go in a given number of transitions, from a state to another
#'                \item \code{get.time.transition.Prob( ) } calculate the probability to go in a given time, from a state to another
#'                \item \code{build.PWF( ) } build automatically a PWF XML definition script.
#'                \item \code{findReacheableNodes( ) } and return the array containing the reacheable states, starting from the passed one.
#'                }
#'              In order to better undestand the use of such methods, please visit: www.pminer.info
#'              
#'              The consturctor admit the following parameters:
#' parameters.list a list containing possible parameters to tune the model. 
#' @param parameters.list a list containing the parameters. The possible ones are: 'considerAutoLoop' and 'threshold'. 'considerAutoLoop' is a boolean which indicates if the autoloops have to be admitted, while 'threshold' is the minimum value that a probability should have to do not be set to zero, in the transition matrix.
#' @export
#' @examples \dontrun{
#'
#' # create a Loader
#' obj.L<-dataLoader();   
#'
#' # Load a .csv 
#' obj.L$load.csv(nomeFile = "../otherFiles/mammella.csv",
#' IDName = "CODICE_SANITARIO_ADT",
#' EVENTName = "DESC_REPARTO_RICOVERO",
#' dateColumnName = "DATA_RICOVERO")
#'
#' # get the loaded data
#' dati <- obj.L$getData()
#'
#' # build a Second Order Marvov Model with a threshold of 0.2
#' FOMM <- firstOrderMarkovModel( 
#' parameters.list = list("threshold"=0.001))
#'
#' # load the data
#' FOMM$loadDataset(dataList = dati)
#'
#' # train a model
#' FOMM$trainModel()
#'
#' # generate 10 new processes (nb: if the 
#' # threshold is too low, it can fail...)
#' aaa <- FOMM$play(numberOfPlays = 10)
#'
#' # get the transition matrix
#' TranMatrix <- FOMM$getModel(kindOfOutput = "MMatrix.perc")
#'
#' # plot the model
#' FOMM$plot()
#'
#' # generate other 20 fake-processes
#' ooo <- FOMM$play(numberOfPlays = 20)
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
  play<-function(numberOfPlays = 1, min.num.of.valid.words = NA,
                 toReturn="csv") {
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
    if(toReturn=="csv") { daRestituire <- res  }
    if(toReturn=="dataLoader"){
      # Istanzia un oggetto dataLoader che eridita il parametro "verbose"
      daRestituire<-dataLoader(verbose.mode = FALSE)
      daRestituire$load.data.frame(mydata = res,
                                   IDName = "patID",EVENTName = "event",
                                   dateColumnName = "date")      
    }
    return(daRestituire)
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
  # ***************************************************************************************************
  # MODEL SPECIFIC PUBLIC METHODS
  # ***************************************************************************************************   
  #===========================================================
  # plot.delta.graph
  # Funzione per il plotting delle distanze rispetto ad una data metrica
  #===========================================================
  plot.delta.graph<-function( objToCheck, threshold=0, type.of.graph="delta", threshold.4.overlapped=.3 ,giveBackGrViz = FALSE) {
    
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
    if(giveBackGrViz == TRUE) return(grafo);
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
    "loadDataset"=loadDataset,
    "trainModel"=trainModel,
    "getModel"=getModel,
    "replay"=replay,
    "play"=play,
    "plot"=plot,
    "distanceFrom"=distanceFrom,
    
    "getLogObj"=getLogObj,
    "getInstanceClass"=getInstanceClass,
    "plot.delta.graph"=plot.delta.graph,
    "get.transition.Prob"=get.transition.Prob,
    "get.time.transition.Prob"=get.time.transition.Prob,
    "build.PWF"=build.PWF,
    "findReacheableNodes"=findReacheableNodes
  ) )  
}