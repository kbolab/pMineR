#' Create an object from the classes \code{alphaAlgorithm} or \code{firstOrderMarkovModel}
#' 
#' @description  This wrapping function instantiates a Process Mining model (\code{alphaAlgorithm} or \code{firstOrderMarkovModel} ) and provides an unique interface for both of them.
#' @param kindOfObject can be 'alphaAlgorithm' or 'firstOrderMarkovModel', according with the kind of object you want back
#' @param parameters.list some models allow to define some details (i.e.: thresholds for probabilities, etc.). In the current implementation 'alphaAlgorithm' does not allow to specify any parameters, while 'firstOrderMarkovModel' has two optional parameters:
#'        \itemize{
#'        \item \code{threshold } a number between 0 and 1 (default is 0). In the graph, arcs with a probability under the threshold will be removed
#'        \item \code{considerAutoLoop } a boolean parameter (default is \code{TRUE}). If \code{FALSE} the arcs outcoming and incoming in the same node will be removed
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
#' A trainer for a Process Mining model (wrapping function)
#' 
#' @description  This function wraps the \code{<object>::train() } method and trains the Process Mining model. The model has to be previously loaded with a loader, as shown in the examples below
#' 
#' @param PM.obj a Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder() } wrapper function
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
#' @description  This function wraps the \code{<object>::getModel() } method and returns a Process Mining model, for example in XML or grViz format
#' 
#' @param PM.obj a Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder()} wrapper function
#' @param kindOfOutput a string indicating the kind of desired output. Can be "\code{XML}" or "\code{grViz}"
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
#' @description  This function wraps the \code{<object>::loadDataset( ... ) } method and loads a dataset into a Process Mining model
#' @param PM.obj an Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param dataList a \code{dataList} structure, returned by a \code{dataLoader::getData()} method
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
#' @description  This function wraps the \code{<object>::play( ... ) } method and runs many times a Process Mining model and returns the generates processes.
#' 
#' @param PM.obj a Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
#' @param numberOfPlays the number of sequences you want to obtain
#' @return a list containing the generated words
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
#' @description  This function wraps the \code{<object>::plot( ... )} method and plots the graph of a Process Mining model.
#' @param PM.obj a Process Mining model previously instantiated via the appropriate class (in example \code{alphaAlgorithm()}  or \code{firstOrderMarkovModel()}) or \code{AA.builder();} wrapper function
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
#' @description  this function wraps the \code{<object>::distanceFrom() } method and calculates the 'distance' between an first Process Mining model object (passed with the parameter \code{aaObj}) and a second Process Mining model object (passed with the parameter \code{objToCheck}). This method allows to compute the distance according to many metrics. If not specified it uses the default metric.
#' @param PM.obj the first Process Mining object
#' @param objToCheck the second Process Mining object
#' @param metric a string indicating the desired metric. At the moment only the default metric (is '\code{default}') is available
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
#' Create a \code{dataLoader} object
#' 
#' @description  This function allows to instantiate a \code{dataLoader} object.
#' @return a \code{dataLoader} empty object
#' @export
#' @examples \dontrun{
#' 
#' # Instantiate a 'dataLoader' object
#' obj<- LD.builder()
#' }
LD.builder<-function( ) {
  res<-dataLoader()
  return(res)
}
#' Load a .csv based log file into a \code{dataLoader} object
#' 
#' @description  This function wraps the \code{dataLoader::load.csv( ... ) } method and returns nothing but the object passed as \code{loader.obj} is loaded with the given .csv file content
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
#' Give back the data loaded into a dataLoader object
#' 
#' @description  This function wraps \code{dataLoader::getData() } method and returns the data previously loaded into a \code{dataLoader} object. 
#' @param loader.obj a \code{dataLoader} object
#' @export 
#' @return it returns a list contaning: \itemize{
#' \item{ \code{arrayAssociativo} the list of the possible EVENTS}
#' \item{ \code{footprint} the footprint table (with the BEGIN and END dummy states )}
#' \item{ \code{MMatrix} the transition matrix, filled with the absolute frequencies}
#' \item{ \code{MMatrix.perc} the transition matrix, filled with the relative frequencies (the rows are normalized to 1)'}
#' \item{ \code{MMatrix.perc.noLoop} the transition matrix, without auto-loop on the states, filled with the relative frequencies (the rows are normalized to 1)'}
#' \item{ \code{pat.processes} a list contaning the PROCESSES with all the other found in the csv}
#' \item{ \code{wordSequence.raw} a list contaning the just the PROCESSES}
#' }
#' @examples \dontrun{ 
#' 
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
