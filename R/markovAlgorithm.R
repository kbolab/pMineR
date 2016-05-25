#' markovModel
#' 
#' @description  implement a simple markov Model
#' @useDynLib pMineR    
#' @export
markovModel<-function( ) {
  
  MMatrix<-''
  footPrint<-''
  model.grViz<-'';
  model.XML<-'';
  is.dataLoaded<-FALSE  
  #=================================================================================
  # loadDataset
  #=================================================================================   
  loadDataset<-function( transMatrix , footPrintTable ) {
    MMatrix<<-transMatrix
    footPrint<<-footPrintTable
    is.dataLoaded<<-TRUE
  }  
  #=================================================================================
  # trainModel
  #=================================================================================   
  trainModel<-function() {
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
  costructor<-function( ) {
    MMatrix<<-''
    footPrint<<-''
    model.grViz<<-'';
    model.XML<<-'';
    is.dataLoaded<<-FALSE
  }
  # -----------------------------------------------------------------
  costructor();
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
