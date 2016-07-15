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
#' # Load a .csv using "DES" and "ID" as column names to indicate events 
#' # and Patient's ID
#' obj.L$loader(nomeFile = "./otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES")
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
    footPrintTable<-dataList$footPrint
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
    
    # dichiara che i dati sono stati caricati
    is.dataLoaded<<-TRUE
  }  
  #===========================================================
  # play
  #===========================================================
  play<-function(numberOfPlays = 1 ) {
    res<-list()
    for(i in seq(1,numberOfPlays)) {
      res[[i]]<-play.Single()
    }
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
    
    grafo<-build.graph.from.table( MM = MM, threshold  = threshold)
    
    model.grViz<<-grafo;
  }
  #===========================================================
  # replay
  #===========================================================
  replay<-function( wordSequence.raw ) {
    cat("\n Not yet implemented for MM models")
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
  plot.delta.graph<-function( objToCheck, threshold=0 ) {
    
    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc    
    
    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)
    listaStati<-unique(combinazioni[,1])
    matrice<-array(0,dim=c(length(listaStati),length(listaStati)))
    colnames(matrice)<-listaStati
    rownames(matrice)<-listaStati    
    
    for(riga in seq(1,nrow(combinazioni))) {
      matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- abs(combinazioni[riga,"int"] - combinazioni[riga,"ext"])
#      if( matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ] <threshold) matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<-0
    }
    
#     righe.valide<-c()
#     for(riga in listaStati) {
#       if(  sum(matrice[riga,]) > 0   ) righe.valide<-c(righe.valide,riga)
#     }
#     if(!("END" %in% righe.valide)) righe.valide<-c(righe.valide,"END")
#     if(!("BEGIN" %in% righe.valide)) righe.valide<-c(righe.valide,"BEGIN")
# 
#     matrice<-matrice[righe.valide,righe.valide]


    grafo<-build.graph.from.table(MM = matrice, threshold = threshold) 
    grViz(grafo);
  }   
  #===========================================================
  # play.Single
  #===========================================================  
  play.Single<-function() {
    ct<-1;
    res<-c();
    if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
    else considerAutoLoop<-TRUE  
    
    # copia la tabella delle transizioni in una un po' piu' facile 
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) MM<-MMatrix.perc
    else MM<-MMatrix.perc.noLoop
    
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
    return(res);
  }  
  #===========================================================
  # build.graph.from.table
  #===========================================================
  build.graph.from.table<-function(MM, threshold) {
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
    "plot.delta.graph"=plot.delta.graph
  ) )  
}