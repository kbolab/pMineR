#' markovModel
#' 
#' @description  implement a simple markov Model
#' @useDynLib pMineR    
#' @export
markovModel<-function( parameters.list = NA ) {
  
  MMatrix<-''
  footPrint<-''
  model.grViz<-'';
  model.XML<-'';
  is.dataLoaded<-FALSE  
  parameters<-NA
  
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
    t<-1
    browser();
    t<-2
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
    parameters<<-parameters
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
