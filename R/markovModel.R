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
  
  #=================================================================================
  # loadDataset
  #=================================================================================   
  loadDataset<-function( transMatrix , footPrintTable ) {
    MMatrix<<-transMatrix
    footPrint<<-footPrintTable
    
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
  # trainModel
  #=================================================================================   
  trainModel<-function() {
    # setta la soglia a zero, così per sport...
    if(!is.null(parameters$threshold)) threshold<-parameters$threshold
    else threshold<-0

    # copia la tabella delle transizioni in una un po' più facile 
    # da maneggiare (almeno come nome)
    MM<-MMatrix.perc
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
    stop("The requested model is not available yet")
  }
  #=================================================================================
  # plot
  #=================================================================================   
  plot<-function(){
    grViz( getModel(kindOfOutput = "grViz" ) )
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
    "plot"=plot
  ) )  
}
