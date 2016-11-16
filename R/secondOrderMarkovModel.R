#' secondOrderMarkovModel class
#' 
#' @description  This is an implent the Second Order Markov Model (FOMM) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'              There are two ways to use this class: directly using the methods previously 
#'              listed or via wrapping functions (called PM.<method name>). In the examples section you will find an example of both.
#' @param parameters.list a list containing possible parameters to tune the model. The admitted element of the input list are:
#' @useDynLib pMineR    
#' @export
secondOrderMarkovModel<-function( parameters.list = list() ) {
  MMatrix<-''
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
    
    for(patID in names(pat.process)) {
      sequenza <- unlist(  pat.process[[patID]][[csv.EVENTName]]  ) 
      for(i in seq(1,length(sequenza)-1)) {
        if(i == 1) {
          indice <- which(M.2.Matrix.row.index[,1]=="BEGIN" & M.2.Matrix.row.index[,2]=="BEGIN")
          M.2.Matrix[indice,sequenza[i+1] ]<- M.2.Matrix[indice,sequenza[i+1] ]+1
        }
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
  #===========================================================
  # play
  #===========================================================
  play<-function(numberOfPlays = 1 ) {
    obj.utils <- utils()
    res<-list()
    for(i in seq(1,numberOfPlays)) {
      res[[as.character(i)]]<-play.Single()
    }
    res <- obj.utils$format.data.for.csv(listaProcessi = res, lista.validi = rep(TRUE,numberOfPlays))
    res<-as.data.frame(res)
    return(res)
  }  
  #===========================================================
  # play.Single
  #===========================================================  
  play.Single<-function() {
    ct<-1;
    res<-c();
    browser()
    
    statoAttuale<-"BEGIN"
    while( statoAttuale != "END") {
      sommaCum<-cumsum(MM[statoAttuale,])
      dado<-runif(n = 1,min = 0,max = 0.99999999999999)
      posizione<-which( (cumsum(MM[statoAttuale,])-dado)>=0  )[1]
      nuovoStato<-colnames(MM)[posizione]
      res<-c(res,statoAttuale)
      statoAttuale<-nuovoStato
    }
    res<-c(res,"END")
    res<-res[ which( !(res %in%  c('BEGIN','END') ))    ] 
    return(res);
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
    MM.2.Matrix<<-list()
    MM.2.Matrix.perc<<-list()
    is.dataLoaded<<-FALSE
    parameters<<-parametersFromInput
    obj.log<<-logHandler();
    setInstanceClass(className = "secondOrderMarkovModel")
    istanceClass<<-list()
    pat.process<<-list()
    csv.IDName<<-''
    csv.EVENTName<<-''
    csv.dateColumnName<<-''    
  }
  #===========================================================
  costructor( parametersFromInput = parameters.list);
  #===========================================================
  return( list(
    "trainModel"=trainModel,
    "loadDataset"=loadDataset,
    "play"=play,
    "getModel"=getModel
  ) )  
}


