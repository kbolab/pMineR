#' A class to train Second Order Markov Models#' 
#' @description  This is an implementation of the Second Order Markov Model (SOMM) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'                \itemize{
#'                \item \code{secondOrderMarkovModel( ) } is the costructor
#'                \item \code{loadDataset( ) } loads data taken from a dataLoader::getData() method, into a SOMM object
#'                \item \code{trainModel( ) } train a model using the previously loaded dataset
#'                \item \code{replay( ) } re-play a given event log on the internal SOMM model
#'                \item \code{play( ) } play the internal FOMM model a desired number of times, in order to simulate new event-logs. This methods can also, if desired, simulate event-logs which does not complies with the internal SOMM model.
#'                \item \code{getModel( ) } return the trained internal SOMM model
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
#' SOMM <- secondOrderMarkovModel( 
#' parameters.list = list("threshold"=0.002))
#'
#' # load the data
#' SOMM$loadDataset(dataList = dati)
#'
#' # train a model
#' SOMM$trainModel()
#'
#' # generate 10 new processes (nb: if the 
#' # threshold is too low, it can fail...)
#' aaa <- SOMM$play(numberOfPlays = 10)
#'
#' # get the transition matrix
#' TranMatrix <- SOMM$getModel(kindOfOutput = "MM.2.Matrix.perc")
#'
#'
#' }
secondOrderMarkovModel<-function( parameters.list = list() ) {
  MMatrix<-''
  MMatrix.perc<-''
  MMatrix.perc.noLoop<-''
  MMatrix.density.list<-''
  MMatrix.mean.time<-''
  MM.2.Matrix<-list()
  MM.2.Matrix.perc<-list()
  is.dataLoaded<-FALSE  
  parameters<-list()
  obj.log<-NA
  istanceClass<-list()
  pat.process<-list()
  csv.IDName<-''
  csv.EVENTName<-''
  csv.dateColumnName<-''
  cache.findReacheableNodes<-list()
  
  # ***************************************************************************************************
  # WRAPPING METHODS
  # ***************************************************************************************************
  #===========================================================
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) { 
    transMatrix<-dataList$MMatrix
    MMatrix<<-transMatrix

    # calcola la matrice delle percentuali e quella delle percentuali senza i loop
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
   
    pat.process<<-dataList$pat.process
    csv.IDName<<-dataList$csv.IDName
    csv.EVENTName<<-dataList$csv.EVENTName
    csv.dateColumnName<<-dataList$csv.dateColumnName
    
    # dichiara che i dati sono stati caricati
    is.dataLoaded<<-TRUE
  }  
  #===========================================================
  # trainModel
  #===========================================================
  trainModel<-function() {
    # setta la soglia a zero, cosi' per sport...
    if(!is.null(parameters$threshold)) threshold<-parameters$threshold
    else threshold<-0
    if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
    else considerAutoLoop<-TRUE  
    
    # copia la tabella delle transizioni in una un po' piu' facile 
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) MM<-MMatrix.perc
    else MM<-MMatrix.perc.noLoop
    
    MM.2.Matrix <<- build.second.order.matrix()
    MM.2.Matrix.perc <<- MM.2.Matrix
    for( i in seq( 1 , nrow(MM.2.Matrix.perc$M.2.Matrix)) ) {if(sum(MM.2.Matrix.perc$M.2.Matrix[i,])>0)  {MM.2.Matrix.perc$M.2.Matrix[i,]<-MM.2.Matrix.perc$M.2.Matrix[i,]/sum(MM.2.Matrix.perc$M.2.Matrix[i,]);} } 

    # verifica l'autoloop
    if(considerAutoLoop==FALSE) {
      diag(MM.2.Matrix.perc$M.2.Matrix)<<-0;
      diag(MM.2.Matrix$M.2.Matrix)<<-0;
    }
    # sistema la threshold
    aa<- MM.2.Matrix.perc$M.2.Matrix
    aa[ which(aa<=threshold,arr.ind = T) ]<-0
    for( i in seq( 1 , nrow(aa)) ) {if(sum(aa[i,])>0)  {aa[i,]<-aa[i,]/sum(aa[i,]);} } 
    MM.2.Matrix.perc$M.2.Matrix<<-aa
  }
  #===========================================================
  # getModel
  #===========================================================
  getModel<-function(kindOfOutput) {
    if(kindOfOutput=="MM.2.Matrix") return( MM.2.Matrix )
    if(kindOfOutput=="MM.2.Matrix.perc") return( MM.2.Matrix.perc )
    
    obj.log$sendLog(msg= "The requested model is not available yet" , type="NMI" )
  }  
  #===========================================================
  # build.second.order.matrix
  # E' il costruttore della classe
  #===========================================================
  build.second.order.matrix<-function(  ) {
    # Calcola le possibili coppie per il FROM
    arr.stati.normali <- colnames(MMatrix)[ which(!(colnames(MMatrix) %in% c("BEGIN","END"))) ]
    mtr.states.1 <- expand.grid(c("BEGIN"),c("BEGIN",arr.stati.normali)   )
    mtr.states.2 <- expand.grid(arr.stati.normali,arr.stati.normali   )
    mtrTot <- rbind(mtr.states.1,mtr.states.2)
    M.2.Matrix<-matrix(0,nrow = nrow(mtrTot),ncol = length(colnames(MMatrix)))
    colnames(M.2.Matrix)<-colnames(MMatrix)
    M.2.Matrix.row.index <- mtrTot
    # Ora scorri il pat.xxxx a contare le occorrenze
    # browser()
    for(patID in names(pat.process)) {
      sequenza <- unlist(  pat.process[[patID]][[csv.EVENTName]]  ) 
      sequenza<-c("BEGIN","BEGIN",sequenza) #  im
      for(i in seq(1,length(sequenza)-1)) {
        
        if( i == length(sequenza)-1) { 
          indice <- which(M.2.Matrix.row.index[,1]==sequenza[i] & M.2.Matrix.row.index[,2]==sequenza[i+1])
          M.2.Matrix[indice,"END" ] <- M.2.Matrix[indice,"END" ]+1      
          break
        }
        indice <- which(M.2.Matrix.row.index[,1]==sequenza[i] & M.2.Matrix.row.index[,2]==sequenza[i+1])
        M.2.Matrix[indice,sequenza[i+2] ] <- M.2.Matrix[indice,sequenza[i+2] ]+1
      }
    }
    return(list(
      "M.2.Matrix" = M.2.Matrix,
      "M.2.Matrix.row.index" = M.2.Matrix.row.index
    ))
    
  }  
  # #=================================================================================
  # # play.easy
  # #   number.of.cases : numero di casi da generare
  # #   min.num.of.valid.words : numero minimo di parole valide
  # #   max.word.length : numero massimo di eventi per parola
  # #=================================================================================  
  # play.easy<-function(number.of.cases, min.num.of.valid.words=NA, max.word.length=100, 
  #                     howToBuildBad="resample", debug.mode = FALSE) {
  # }  
  #===========================================================
  # play
  #===========================================================
  play<-function(numberOfPlays = 1 , toReturn="csv" ) {
    obj.utils <- utils()
    res<-list()
    for(i in seq(1,numberOfPlays)) {
      res[[as.character(i)]]<-play.Single()
    }
    if(length(res)>0 & length(unlist(res))>0  ) {
        res <- obj.utils$format.data.for.csv(listaProcessi = res, lista.validi = rep(TRUE,numberOfPlays))
        res<-as.data.frame(res)
    } else return(list())
    
    # restituisci secondo il formato richiesto
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
  #===========================================================
  # play.Single
  #===========================================================  
  play.Single<-function() {
    ct<-1;
    res<-c();

    # statoPrecedente<-c("BEGIN","BEGIN");  # Stato al tempo precedente
    statoAttuale<-c("BEGIN","BEGIN");  # Stato al tempo attuale
    statoPrecedente <- c("BEGIN","BEGIN")
    # cat(".")
    aaa <- MM.2.Matrix.perc$M.2.Matrix
    bbb <- MM.2.Matrix.perc$M.2.Matrix.row.index
     iter <- 1
     # cat("\n ===============================")
    while( statoAttuale[2] != "END") {
      
      if ( iter > 1 & !("END" %in% findReacheableNodes(nodoDiPatenza = statoAttuale) )) {
        return(c() ) ;
      }
      
      # indice <- which(bbb[,1]==statoPrecedente & bbb[,2]==statoAttuale)
      indice <- which( bbb[,1]==statoAttuale[1] & bbb[,2]==statoAttuale[2])
      
      sommaCum<-cumsum(aaa[indice,])
      
      if(sum(sommaCum)==0) break;

            # browser()
      # if(sum(sommaCum)==0) break;
      dado<-runif(n = 1,min = 0,max = 0.99999999999999)
      # dado<-runif(n = 1,min = 0,max = max(sommaCum)-0.00001)
      posizione<-which( (cumsum(aaa[indice,])-dado)>=0  )[1]
      nuovoStato<-c(statoAttuale[2],colnames(aaa)[posizione])
      if( is.na(nuovoStato[2]) ) browser()
      # browser()
      if ( ("END" %in% findReacheableNodes(nodoDiPatenza = nuovoStato) )) {
        res<-c(res,statoAttuale[2])
        statoPrecedente <- statoAttuale
        statoAttuale <- nuovoStato
      }
      iter <- iter + 1

    }
    res<-c(res,"END")
    res<-res[ which( !(res %in%  c('BEGIN','END') ))    ] 
    return(res);
  }   
  get.from.cache<-function( what , index) {
    if(what=="findReacheableNodes") {
      if( index[1] %in% names(cache.findReacheableNodes)) {
        if( index[2] %in% names(cache.findReacheableNodes[[index[1]]])) {
          return(list("inCache"=TRUE, "value"=cache.findReacheableNodes[[index[1]]][[index[2]]]))
        }
      }
      return( list("inCache"=FALSE,"value"=NA))
    }
    stop("ERROR: #jdf89d8f oggetto non previsto in cache")
  }
  store.to.cache<-function(what , index   , value ) {
    if(what=="findReacheableNodes") {
      if( !(index[1] %in% names(cache.findReacheableNodes)) ) {
        cache.findReacheableNodes[[index[1]]]<<-list()
      }
      cache.findReacheableNodes[[index[1]]][[index[2]]]<<-value
      return();
    }
    stop("ERROR: #jdf89d8f oggetto non previsto in cache")    
  }
  #===========================================================
  # findReacheableNodes
  # Funzione 
  #===========================================================
  findReacheableNodes<-function( nodoDiPatenza = c('BEGIN','BEGIN')  ) {
    
    valore.in.cache <- get.from.cache(what = "findReacheableNodes", index = nodoDiPatenza ) 
    if(valore.in.cache$inCache == TRUE ) return(valore.in.cache$value);
    
    # cat("\n", nodoDiPatenza)
    # browser()
    tabellaNodiRaggiunti <- findReacheableNodes.recursiveLoop(
      nodoAttuale = nodoDiPatenza,
      nodi.raggiunti = rbind(c(),nodoDiPatenza)
    )
    arrayStati <- unique(c(tabellaNodiRaggiunti[,1],tabellaNodiRaggiunti[,2]))
    store.to.cache( what = "findReacheableNodes", index = nodoDiPatenza  , value = arrayStati)
    return(arrayStati)
  }  
  findReacheableNodes.recursiveLoop<-function( nodoAttuale , nodi.raggiunti  ) {
    
    # caso noto
    if(nodoAttuale[2]=="END") return(nodi.raggiunti)
    # if(nodoAttuale[2]==NA) return(nodi.raggiunti)
    
    
    lista.nodi.raggiungibili <- colnames(MM.2.Matrix.perc$M.2.Matrix)
    nodi.raggiunti <- unique(nodi.raggiunti)
    
    aaa <- MM.2.Matrix.perc$M.2.Matrix
    bbb <- MM.2.Matrix.perc$M.2.Matrix.row.index    

    for( nodoDestinazione in lista.nodi.raggiungibili) {
      
      coppia.possibile <- c(nodoAttuale[2],nodoDestinazione)
     
      gia.presente <- which(nodi.raggiunti[,1]==coppia.possibile[1] & nodi.raggiunti[,2]==coppia.possibile[2])
      indice.nodo.attuale <- which( bbb[,1]==nodoAttuale[1] & bbb[,2]==nodoAttuale[2])
      
      if( length(gia.presente)==0 & aaa[indice.nodo.attuale,nodoDestinazione]>0) {
        
        nuovo.stato.iniziale <- c(nodoAttuale[2],nodoDestinazione)
        aa <- findReacheableNodes.recursiveLoop( nodoAttuale = nuovo.stato.iniziale , 
                                                 nodi.raggiunti = rbind(nodi.raggiunti,c(nodoAttuale[2],nodoDestinazione)))
        
        nodi.raggiunti <- unique(rbind(nodi.raggiunti,aa))
      }
    }
    return(nodi.raggiunti)
  }     
  #===========================================================
  # replay
  #===========================================================
  replay<-function( dataList , debugMode = FALSE, col.toCheckPerformances=NA ) {
    res<-list()
    res$words<-list()
    res$success<-c()
    declared.correctness<- c()
    
    aaa <- MM.2.Matrix.perc$M.2.Matrix
    bbb <- MM.2.Matrix.perc$M.2.Matrix.row.index
    
    if(debugMode == TRUE) browser()
    for(patId in names(dataList$pat.process)) {
      parola <- unlist(dataList$pat.process[[patId]][[dataList$csv.EVENTName]])
      parola <- c("BEGIN","BEGIN",parola)
      success <- TRUE;
      path.attuale<-c()
      # if(patId==82) browser()
      for( caratt.i in seq(2,(length(parola)-1)) ) {
        
        statoPrecedente <- parola[ caratt.i - 1  ]
        statoAttuale <- parola[ caratt.i   ]
        statoFuturo <- parola[ caratt.i +1  ]
        
        indice <- which(bbb[,1]==statoPrecedente & bbb[,2]==statoAttuale)
        
        # se non l'hai trovato
        if(length(indice)==0) {
          success = FALSE; break; 
        }
        if(!(statoFuturo %in% colnames(MM.2.Matrix.perc$M.2.Matrix))) { success = FALSE; break; }
        
        jump.prob <- aaa[ indice, statoFuturo ]
        
        if(jump.prob>0) path.attuale <- c(path.attuale,parola[ caratt.i  ])
        if(jump.prob==0) { success = FALSE; break; }
        # caratt.s %in% colnames(MMatrix.perc)
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
  # replay
  #===========================================================
  plot<-function(  ) {  
    cat("\n-------------------------------------------")
    cat("\n Sorry, unfortunately we do not provide any graph for a SOMM model")
    cat("\n-------------------------------------------")
  }
  #===========================================================
  # setIstanceClass
  #===========================================================
  setInstanceClass<-function( className, classType = "default") {
    istanceClass[[classType]]<<-className
  }  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = list() ) {
    MMatrix<<-''
    MMatrix.perc<<-''
    MMatrix.perc.noLoop<<-''
    MM.2.Matrix<<-list()
    MM.2.Matrix.perc<<-list()
    MMatrix.density.list<<-list()
    MMatrix.mean.time<-''    
    is.dataLoaded<<-FALSE
    parameters<<-parametersFromInput
    obj.log<<-logHandler();
    setInstanceClass(className = "secondOrderMarkovModel")
    istanceClass<<-list()
    pat.process<<-list()
    csv.IDName<<-''
    csv.EVENTName<<-''
    csv.dateColumnName<<-''    
    cache.findReacheableNodes<<-list()
  }
  #===========================================================
  costructor( parametersFromInput = parameters.list);
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "trainModel"=trainModel,
    "getModel"=getModel,
    "play"=play,
    "replay"=replay,
    "plot"=plot
  ) )  
}


