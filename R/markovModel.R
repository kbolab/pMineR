#' firstOrderMarkovModel
#' 
#' @description  implement a simple markov Model
#' @useDynLib pMineR    
#' @export
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
  
  #=================================================================================
  # loadDataset
  #=================================================================================   
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
  #=================================================================================
  # generate
  #=================================================================================  
  play<-function(numberOfPlays = 1 ) {
    res<-list()
    for(i in seq(1,numberOfPlays)) {
      res[[i]]<-play.Single()
    }
    return(res)
  }
  play.Single<-function() {
    ct<-1;
    res<-c();
    if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
    else considerAutoLoop<-TRUE  
    

    # copia la tabella delle transizioni in una un po' più facile 
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
  #=================================================================================
  # trainModel
  #=================================================================================   
  trainModel<-function() {
    # setta la soglia a zero, così per sport...
    if(!is.null(parameters$threshold)) threshold<-parameters$threshold
    else threshold<-0
    if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
    else considerAutoLoop<-TRUE  

    # copia la tabella delle transizioni in una un po' più facile 
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) MM<-MMatrix.perc
    else MM<-MMatrix.perc.noLoop
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

    model.grViz<<-a;
#    model.XML<<-strutturaXML    
  }
  #=================================================================================
  # replay
  #=================================================================================   
  replay<-function( wordSequence.raw ) {
  }
  #=================================================================================
  # convert2XML - future
  #=================================================================================   
  convert2XML<-function(  ) {
  }   
  #=================================================================================
  # getModel
  #=================================================================================   
  getModel<-function(kindOfOutput) {
    if(kindOfOutput=="XML") return( model.XML )
    if(kindOfOutput=="grViz") return( model.grViz )
    if(kindOfOutput=="MMatrix") return( MMatrix )
    if(kindOfOutput=="MMatrix.perc") return( MMatrix.perc )
    stop("The requested model is not available yet")
  }
  #=================================================================================
  # plot
  #=================================================================================   
  plot<-function(){
    grViz( getModel(kindOfOutput = "grViz" ) )
  }  
  #=================================================================================
  # distanceFrom
  #=================================================================================   
  distanceFrom<-function( objToCheck, metric="default") {

    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc
    
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
    distance<-sum(abs(combinazioni$ext - combinazioni$int))
    return( list(   "distance" = distance )      )
  }
  #=================================================================================
  # setIstanceClass
  #=================================================================================     
  setIstanceClass<-function( className, classType = "default") {
    istanceClass[[classType]]<-className
  }
  #=================================================================================
  # setIstanceClass
  #=================================================================================     
  getInstanceClass<-function( className, classType = "default") {
    return(istanceClass[[classType]])
  }  
  # -----------------------------------------------------------------
  # costructor
  # -----------------------------------------------------------------
  costructor<-function( parametersFromInput = NA ) {
    MMatrix<<-''
    footPrint<<-''
    model.grViz<<-'';
    model.XML<<-'';
    is.dataLoaded<<-FALSE
    parameters<<-parametersFromInput
    MMatrix.perc<<-NA
    MMatrix.perc.noLoop<<-NA   
    istanceClass<<-list()
  }
  # -----------------------------------------------------------------
  costructor( parametersFromInput = parameters.list);
  # -----------------------------------------------------------------
  return( list(
    "trainModel"=trainModel,
    "getModel"=getModel,
    "loadDataset"=loadDataset,
    "trainModel"=trainModel,
    "replay"=replay,
    "play"=play,
    "plot"=plot,
    "distanceFrom"=distanceFrom,
    "setIstanceClass"=setIstanceClass,
    "getInstanceClass"=getInstanceClass
  ) )  
}
