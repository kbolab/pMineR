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
#' @examples \dontrun{
#' # Instantiate a'firstOrderMarkovModel' model
#' obj<- PM.builder(kindOfObject = "firstOrderMarkovModel")
#' }
PM.builder<-function( kindOfObject , parameters.list = list() ) {
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
#' @examples \dontrun{
#' # Instantiate a'firstOrderMarkovModel' model
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "../csv/dati_retto.csv",
#'                      IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' 
#' # Load the previously loaded csv into the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#' 
#' # train the model! 
#' PM.trainModel(PM.obj = obj.PM)
#' }
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
#' @examples \dontrun{
#' # Instantiate a'firstOrderMarkovModel' model
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "../csv/dati_retto.csv",
#'                IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "alphaAlgorithm")
#' 
#' # Load the previously loaded csv into the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#' 
#' # train the model! 
#' PM.getModel(PM.obj = obj.PM, kindOfOutput = "XML")
#' } 
PM.getModel<-function( PM.obj , kindOfOutput="XML" ) {
  return(PM.obj$getModel(kindOfOutput = kindOfOutput))
}
#' Load a dataset into a Process Mining model
#' 
#' @description  this is a wrapping function able to load a dataset into a Process Mining model
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param dataList a \code{dataList} structure as returned by the \code{getData} method of a \code{dataLoader} object
#' @export
#' @examples \dontrun{
#' # Instantiate a'firstOrderMarkovModel' model
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "../csv/dati_retto.csv",
#'                   IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' 
#' # Load the previously loaded csv into the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#' }
PM.loadDataset<-function( PM.obj , dataList ) {
  PM.obj$loadDataset( dataList = dataList )
}
#' Play a Process Mining model
#' 
#' @description  this is a wrapping function able to play (run may times) a Process Mining model.
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param numberOfPlays the desired number of sequences
#' @return a \code{list} containing the generated words
#' @export
#' @examples \dontrun{
#' # Instantiate a'firstOrderMarkovModel' model
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "../csv/dati_retto.csv",
#'               IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' 
#' # Load the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#'
#' # train it
#' PM.trainModel(PM.obj = obj.PM)#' 
#' 
#' # play the model 5 times
#' PM.play(PM.obj = obj.PM,numberOfPlays = 5)
#' } 
PM.play<-function( PM.obj, numberOfPlays ) {
  return(PM.obj$play( numberOfPlays = numberOfPlays ))
}
#' Plot a Process Mining model graph
#' 
#' @description  this is a wrapping function able to plot the graph of a Process Mining model.
#' @param PM.obj An Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @export
#' @import DiagrammeR
#' @examples \dontrun{
#' # Instantiate a'firstOrderMarkovModel' model
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "../csv/dati_retto.csv",
#'         IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' 
#' # Load the PM model
#' PM.loadDataset(PM.obj = obj.PM,dataList = LD.getData(loader.obj = obj.LD))
#'
#' # train it
#' PM.trainModel(PM.obj = obj.PM)#' 
#' 
#' # plot the model
#' PM.plot(PM.obj = obj.PM)
#' }  
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
#' @examples \dontrun{
#' # instantiate two dataLoaders
#' obj.LD.01<-LD.builder()
#' obj.LD.02<-LD.builder()
#' 
#' # load the two csv (for rectal and breast cancer)
#' LD.load.csv(loader.obj = obj.LD.01 ,nomeFile = "../csv/dati_retto.csv",
#'       IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' LD.load.csv(loader.obj = obj.LD.02 ,nomeFile = "../csv/dati_mammella.csv",
#'       IDName = "CODICE_SANITARIO_ADT",EVENTName = "CAT1")
#' 
#' # create two first order markov model
#' obj.PM.rectum <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' obj.PM.breast <-PM.builder(kindOfObject = "firstOrderMarkovModel")
#' 
#' # and load the .csv 
#' PM.loadDataset(PM.obj = obj.PM.rectum,dataList = LD.getData(loader.obj = obj.LD.01))
#' PM.loadDataset(PM.obj = obj.PM.breast,dataList = LD.getData(loader.obj = obj.LD.02))
#' 
#' # train the two models
#' PM.trainModel(PM.obj = obj.PM.rectum)
#' PM.trainModel(PM.obj = obj.PM.breast)
#' 
#' # calculate the distance accortind to the default metric
#' PM.distanceFrom(PM.obj = obj.PM.breast,objToCheck = obj.PM.rectum)
#' }  
PM.distanceFrom<-function( PM.obj , objToCheck , metric="default") {
  return(PM.obj$distanceFrom( objToCheck = objToCheck , metric = metric))
}
#' Create a loader object
#' 
#' @description  this is a wrapping function to instantiate a Loader for .csv log-files.
#' @return a \code{dataLoader} empty object
#' @export
#' @examples \dontrun{
#' # Instantiate a'dataLoader' object
#' obj<- LD.builder()
#' }
LD.builder<-function( ) {
  res<-dataLoader()
  return(res)
}
#' Load a .csv log-file into a dataLoader object
#' 
#' @description  This wrapping methods (it wraps the \code{load.csv} ). It returns nothing, but the object passed as \code{loader.obj} will be loaded with the given .csv file
#' @param loader.obj a \code{dataLoader} object to load the data in
#' @param nomeFile the path of the .csv file
#' @param IDName the name of the column which contains the PATIENT ID
#' @param EVENTName the name of the column which contains the EVENTS
#' @param quote a quoting character ( default = '"' )
#' @param sep the separator ( default = ',' )
#' @export
#' @examples \dontrun{ 
#' # Instantiate a'dataLoader' object
#' obj<- LD.builder()
#' 
#' # load a .csv
#' LD.load.csv(loader.obj = obj,nomeFile = "../csv/rectalCancer.csv",
#'    IDName = "ID_PATIENT",EVENTName = "EVNT")
#' } 
LD.load.csv<-function( loader.obj, nomeFile, IDName, EVENTName,  quote="\"",sep = ",") {
  loader.obj$load.csv(nomeFile = nomeFile,IDName=IDName, EVENTName = EVENTName, quote = quote, sep = sep)
}
#' Give back the loaded data into a dataLoader object
#' 
#' @description  It returns the data previously loaded into a dataLoader object. Because during the loadind, the dataLoader methods also performs some computations, this function can return the footprint table, the words in the files, the transition matrix, and other stuff.
#' @param loader.obj a \code{dataLoader} object to load the data in
#' @export 
#' @return it returns a list contaning: \itemize{
#' \item{ \code{arrayAssociativo} the list of the possible states}
#' \item{ \code{footprint} the footprint table (with the \code{BEGIN} and \code{END} dummy states )}
#' \item{ \code{MMatrix} the transition matrix, filled with the transition absolute frequencies}
#' \item{ \code{MMatrix.perc} the transition matrix, filled with the transition relative frequencies (the rows are normalized to 1)'}
#' \item{ \code{MMatrix.perc.noLoop} the transition matrix, without auto-loop on the states, filled with the transition relative frequencies (the rows are normalized to 1)'}
#' \item{ \code{pat.processes} a list contaning the processes, ordered for each patient}
#' \item{ \code{wordSequence.raw} a list contaning the processes, rawly listed for each patient}
#' }
#' @examples \dontrun{ 
#' # Instantiate a'dataLoader' object
#' obj<- LD.builder()
#' 
#' # load a .csv
#' LD.load.csv(loader.obj = obj,nomeFile = "../csv/rectalCancer.csv",
#'    IDName = "ID_PATIENT",EVENTName = "EVNT")
#' 
#' # get back the loaded data
#' res<- LD.getData(loader.obj = obj)
#' 
#' # print the footprint Table
#' print(res$footPrint)
#' } 
LD.getData<-function( loader.obj ) {
  return(loader.obj$getData())
}
