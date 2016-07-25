#' logInspector class
#' 
#' @description   This class provides some descriptive statistics about the log:
#'                \itemize{
#'                \item \code{costructor( ...) } is the costructor of the class
#'                \item \code{loadDataset( ...) } loads data into a logInspector object. It takes as input the output of the method getData from an instance of the classe dataLoader. It stores the logs and built,internally to the logInspector object, all the structures needed for the next computations.
#'                \item \code{getEventStats() } returns a list containing event-related stats
#'                \item \code{getProcessStats() } returns a list containing process-related stats
#'                \item \code{plotEventStats( ... )} plots event-related stats (takes a custom number of most frequent events it has to plot)
#'                \item \code{plotProcessStats( ... )} plots process-related stats (takes a custom number of most frequent processes it has to plot)
#'
#'                }
#' @param Parameters for plot methods is: 
#'   \itemize{
#'    \item \code{num } the number of most frequent events/processes to plot
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
#' obj.L$load.csv(nomeFile = "./otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES")
#' 
#' # now create an object logInspector
#' obj.logI<-logInspector();    
#' 
#' # load the data into logInspector object
#' obj.logI$loadDataset( obj.L$getData() );  
#' 
#' # get event-related descriptive statistics
#' logI$getEventStats();  
#' 
#' # get process-related descriptive statistics 
#' logI$getProcessStats(); 
#' 
#' # plot event-related descriptive statistics
#' logI$plotEventStats();  
#' 
#' # plot process-related descriptive statistics 
#' logI$getProcessStats(); 
#' }


logInspector <- function() {
  eventType <-'' 
  processInstances <-''
  numberOfDifferentEvents <- ''
  numberOfTotalEvents <- ''
  processInstances.toSymbol <- ''
  
  #===========================================================
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) { 
    eventType <<- dataList$arrayAssociativo
    processInstances <<- dataList$wordSequence.raw
  }  
  
  #===========================================================
  # getEventStats
  #===========================================================  
  getEventStats<-function() {
  
  # some useful variable definitions (unlist everything: don't care about processe instances)
    
   numberOfDifferentEvents <- length(eventType)
   allEvents <- unlist(processInstances)
   numberOfTotalEvents <- length(allEvents)
   countEventOccurrence <- numeric()
   
  # check each event name against all log events and count occurrences  
   
     for(i in 1:length(eventType)){
     countEventOccurrence[i] <- length(grep(eventType[i], allEvents))
     }
   names(countEventOccurrence) <- eventType
   
   # build output structures and fill them
   distribution.abs <- sort(countEventOccurrence, decreasing=TRUE)
   distribution.perc <- distribution.abs/numberOfTotalEvents
   eventStats <- list("Number of different event types" = numberOfDifferentEvents,
                      "Total number of events" = numberOfTotalEvents,
                      "Absolute event occurrence" = distribution.abs,
                      "Percentual event occurrence" = distribution.perc
                      )
   return(eventStats)
  }
  
  #===========================================================
  # getProcessStats
  #===========================================================  
  getProcessStats<-function() {
    
  # 1)associate each event name to a letter, and 2) define variables
    
    eventType.toSymbol <- paste(letters[1:length(eventType)])
    processInstances.toSymbol <- vector("list", length(processInstances))
    processInstances.toSymbolCollapsed <- vector("list", length(processInstances))
    processInstances.toSymbolCollapsedVector <- character()
    
    
    # substitute names with letters, then collapse into a singol string for each process instance
      for (k in 1:length(processInstances)){
        processInstances.toSymbol[[k]] <- vector(mode="character", length(processInstances[[k]]))
        for (i in 1:length(processInstances[[k]])){
          for (j in 1:length(eventType)){
           if (processInstances[[k]][i]==eventType[j]) {processInstances.toSymbol[[k]][i] <- eventType.toSymbol[j]} 
          }
        }
        processInstances.toSymbolCollapsed[[k]] <- paste(processInstances.toSymbol[[k]], sep="", collapse="")
      }
    
    processInstances.toSymbolCollapsedVector <- sapply(processInstances.toSymbolCollapsed,unlist)
    processDistributionTable.abs <- sort(table(processInstances.toSymbolCollapsedVector), decreasing=TRUE)
    
  # build output structures and fill them
    
    processDistribution.abs <- data.frame(matrix(ncol = 4, nrow = length(processDistributionTable.abs)))
    symbolToEventConversion <- data.frame(matrix(ncol = 2, nrow = length(eventType)))
    names(processDistribution.abs) <- c("Process id","Process signature", "Absolute frequency", "Relative frequency") 
    names(symbolToEventConversion) <- c("Event name","Event symbol")
    
    processDistribution.abs[,1] <- names(processInstances)
    processDistribution.abs[,2] <- names(processDistributionTable.abs)
    processDistribution.abs[,3] <- as.vector(processDistributionTable.abs)
    processDistribution.abs[,4] <- as.vector(processDistributionTable.abs)/length(processInstances)
    
    symbolToEventConversion[,1] <- eventType
    symbolToEventConversion[,2] <- eventType.toSymbol
    
    processStats <- list("Absolute frequency dataframe" = processDistribution.abs[,c(1,2,3)],
                         "Relative frequency dataframe" = processDistribution.abs[,c(1,2,4)],
                         "Event to symbol conversion" = symbolToEventConversion
    )
    return(processStats)
  }
  
  
  #===========================================================
  # plotEventStats
  #===========================================================  
  plotEventStats<-function(howManyMostFrequentEvents) {
    eventStats<-getEventStats()
    eventsToPlot <- eventStats$`Absolute event occurrence`[1:howManyMostFrequentEvents]
    barplot(eventsToPlot, horiz=TRUE , main = sprintf("First %g event absolute occurrence", howManyMostFrequentEvents), xlim =c(0,max(eventsToPlot)) , ylab = "Name of event type", xlab = "Number of occurrences" )
    axis(2, at=1:length(eventsToPlot), labels=names(eventsToPlot))
    return()
  }
  
  
  #=================================logI$plotProcessStats(howManyMostFrequentProcesses = )==========================
  # plotProcessStats
  #===========================================================  
  plotProcessStats<-function(howManyMostFrequentProcesses) {
    processStats<-getProcessStats()
    processesToPlot <- processStats$`Absolute frequency dataframe`[1:howManyMostFrequentProcesses,3]
    processesToPlotSignatures <-  processStats$`Absolute frequency dataframe`$`Process signature`[1:howManyMostFrequentProcesses]
    barplot(processesToPlot, horiz=TRUE , main = sprintf("First %g processes absolute occurrence", howManyMostFrequentProcesses), xlim =c(0,max(processesToPlot)) , ylab = "Process index", xlab = "Number of occurrences" )
    axis(2, at=1:length(processesToPlot), labels=processesToPlotSignatures)
  }

  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function() { 
    eventType <<-'' 
    processInstances <<-''
    numberOfDifferentEvents <<- ''
    numberOfTotalEvents <<- ''
    processInstances.toSymbol <<- ''
  } 
  #===========================================================
  costructor();
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "getEventStats"=getEventStats,
    "getProcessStats"=getProcessStats,
    "plotEventStats"=plotEventStats,
    "plotProcessStats"=plotProcessStats 
  ) )
  
}