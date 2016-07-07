#' A builder for a Process Mining model (wrapping function)
#' 
#' @description  this is a wrapping function to instantiate a Process Mining model. Models can also be instantiated directly using the appropriate classes (in esample \code{alphaAlgorithm} or \code{firstOrderMarkovModel} ), if preferred.
#' @param kindOfObject can be 'alphaAlgorithm' or 'firstOrderMarkovModel', according with the kind of object you want back
#' @param parameters.list some models allow to define some details (i.e.: thresholds for probabilities, etc.). In the current implementation 'alphaAlgorithm' does not allow to specify any parameters, while 'firstOrderMarkovModel' has two optional parameters:
#'        \itemize{
#'          \item \code{parameters.list$threshold} if specified, all the arcs with a probability less than the threshold will be suppressed
#'          \item \code{parameters.list$considerAutoLoop} if specified, all the arcs representing a loop of a node with itself will be suppressed
#'        } 
#' @return the wished object
#' @export
PM.builder<-function( kindOfObject , parameters.list = NA ) {
  res<-NA
  if( kindOfObject == "alphaAlgorithm")  {
    res<-alphaAlgorithm()
    return(res)
  }
  if( kindOfObject == "firstOrderMarkovModel")  {
    res<-firstOrderMarkovModel( parameters.list = parameters.list )
    return(res)
  }
  return(NA)
}
#' A trainer for an Process Mining model (wrapping function)
#' 
#' @description  this is a wrapping function able to forse the train for a Process Mining model. The model has to be previously loaded with a loader, as shown in the examples below
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @export
PM.trainModel<-function( PM.obj ) {
  PM.obj$trainModel()
}
#' Returns a model (XML or else)
#' 
#' @description  this is a wrapping function able to returns a Process Mining model, for example in XML format
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param kindOfOutput A string indicating the kind of desired output. Can be "\code{XML}" or "\code{grViz}" depending of the kind of output the user is interested in
#' @return the desided output, according with indicated by \code{kindOfOutput} input parameter
#' @export
PM.getModel<-function( PM.obj , kindOfOutput="XML" ) {
  return(PM.obj$getModel(kindOfOutput = kindOfOutput))
}
#' Load a dataset into a Process Mining model
#' 
#' @description  this is a wrapping function able to load a dataset into a Process Mining model
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param dataList a \code{dataList} structure as returned by the \code{getData} method of a \code{dataLoader} object
#' @export
PM.loadDataset<-function( PM.obj , dataList ) {
  PM.obj$dataLoader( dataList = dataList )
}
#' Replay a Process Mining model
#' 
#' @description  this is a wrapping function able to replay (re-run may times) a Process Mining model on a specified set of event Log. Event Log has to be passed in a list (as shown) and have to be compatible with the model previously loaded and trained.
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param wordSequence.raw is the list of word to be re-played
#' @return a \code{list} containing the results of the re-play of the given words
#' @export
PM.replay<-function(PM.obj , wordSequence.raw ) {
  return(PM.obj$replay(wordSequence.raw = wordSequence.raw))
}
#' Play a Process Mining model
#' 
#' @description  this is a wrapping function able to play (run may times) a Process Mining model.
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param numberOfPlays the desired number of sequences
#' @return a \code{list} containing the generated words
#' @export
PM.play<-function( PM.obj, numberOfPlays ) {
  return(PM.obj$play( numberOfPlays = numberOfPlays ))
}
#' Plot a Process Mining model graph
#' 
#' @description  this is a wrapping function able to plot the graph of a Process Mining model.
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @export
PM.plot<-function( PM.obj ) {
  PM.obj$plot()
}
#' Calculate the distance between two Process Mining models
#' 
#' @description  this is a wrapping function able to plot the graph of a Process Mining model. It calculates the 'distance' between an first AA model (passed with the parameter \code{aaObj}) and a second AA model (passed with the parameter \code{objToCheck}). This method allows to compute the distance according to many metrics. If not specified it uses the default metric.
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param objToCheck Another Process Mining model, OF THE SAME CLASS, previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param metric a string indicatind the desired metric to calculate the distance. The default metric is '\code{default}' but other metrics can be available, in the future.
#' @return a \code{list} containing the results of the distance and some ancillary information (some metrics could return a non-scalar value)
#' @export
PM.distanceFrom<-function( PM.obj , objToCheck , metric="default") {
  return(PM.obj$distanceFrom( objToCheck = objToCheck , metric = metric))
}
