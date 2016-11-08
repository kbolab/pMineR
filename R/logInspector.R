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
#'                \item \code{timeDistribution.stats.plot( ... )} <not yet commented>
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
  loaded.data<-c();
  #=================================================================================
  # clearAttributes
  #=================================================================================    
  clearAttributes<-function() {
    costructor();
  }
  #===========================================================
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) { 
    # Clear some possible previously inserted attribute
    clearAttributes()
    # set the new attributes
    eventType <<- dataList$arrayAssociativo
    processInstances <<- dataList$wordSequence.raw
    loaded.data<<-dataList
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
   arr.max.relative.observation.time<-numeric()
   
   # calculate how frequent an event occour in patient's log (1 patient counts 1)
   
   bigList<-loaded.data$pat.process
   nome.colonna.eventi<-loaded.data$csv.EVENTName

   eventi<-c()
   for( IdPat in names(bigList)){
     eventi<-c(eventi,as.character(unique(bigList[[IdPat]][,nome.colonna.eventi])))
     arr.max.relative.observation.time<-c(arr.max.relative.observation.time,bigList[[IdPat]]$pMineR.deltaDate[nrow(bigList[[IdPat]])])
   }
   names(arr.max.relative.observation.time)<-as.character(names(bigList))
   coverage.tmp<-table(eventi)
   coverage<-as.vector(coverage.tmp)
   names(coverage)<-names(coverage.tmp)
   

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
                      "Percentual event occurrence" = distribution.perc,
                      "Absolute Coverage" = coverage,
                      "arr.max.relative.observation.time"= arr.max.relative.observation.time
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
  
  
  #===========================================================
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
  # timeDistribution.stats.plot
  #===========================================================  
  timeDistribution.stats.plot<-function(
                                        lst.select.attr.Name=NA, lst.select.attr.value=NA, 
                                        lst.pnt.attr.name=NA, lst.pnt.attr.value=NA, 
                                        color='red',  plotGraph.01=TRUE,
                                        plotGraph.02=TRUE, deltaDate.column.name='pMineR.deltaDate',
                                        xlim=c()) { 
    
    res.dataLoader<-loaded.data
    # Ordina per data diangosi
    max.Delta<-c()
    arr.occorrenze<-c(); occorrenza.cum<-c(); occorrenza.diff<-c();
    aaa<-res.dataLoader$pat.process
    # browser()
    if(!(deltaDate.column.name %in% colnames(aaa[[1]])   )) stop(" please check the delta.date column name! ErrCode: #rh4389hcv ");
    
    for(i in seq(1,length(aaa) )) {
      if(length(aaa[[i]][[deltaDate.column.name]])>0)
        max.Delta<- c(max.Delta, max(aaa[[i]][[deltaDate.column.name]]))
      else 
        max.Delta<- c(max.Delta, 0)
    } 
    #     Se qualche paziente non ha eventi e'' possibile che si fissi a -Inf, in questo
    #     caso setta a zero la sua massima timeline
    # max.Delta[which(max.Delta==-Inf)]<-0

    # plotta gli assi e definisci i gap per le timeline
    y.gap<-1;  x.gap<-20
    # Se non e' stato passato xlim, calcolalo, senno' usa quello passato
    # (prevedi comunque un gap finale di 20)
    if(length(xlim)==0) { xlim2pass <- c(0,max(max.Delta)+x.gap)}
    else { xlim2pass <- c( xlim[1] , xlim[2]+x.gap) }
    
    if(plotGraph.01==T) {
      # plot(0,0,xlim=c(0,max(max.Delta)+x.gap),ylim=c(0,length(aaa)*y.gap   ), ylab='Patients', xlab='Time',main='Patient\'s Timeline'  )
      plot(0,0,xlim=xlim2pass,ylim=c(0,length(aaa)*y.gap   ), ylab='Patients', xlab='Time',main='Patient\'s Timeline'  )
    }  
    
    # array che conterranno i punti da plottare in "differita"
    arr.punti.da.plottare.x <- c(); arr.punti.da.plottare.y<-c()
    
    # Cicla per ogni paziente
    for(i in seq(1,length(aaa) )) {
      # browser()
      # Array con i delta giorni di tutti gli eventi
      arr.tak<-as.numeric(aaa[[i]]$delta.dataDiagnosi)
      # if(lengh(arr.tak)==0) arr.tak<-c(0)
      # la riga orizzontale
      if(plotGraph.01==T & length(arr.tak)>0) points(  x=c(0, max(arr.tak) ), y=c(i * y.gap,i *y.gap),  type='l' , col='grey' ) 
      # le righette verticali
      if(plotGraph.01==T & length(arr.tak)>0) points(  x=arr.tak, y=rep(c(i * y.gap),length(arr.tak) ) ,pch=3 , col='grey'  ) 
      # passiamo ai colori
      for( indice in seq(1,length(lst.pnt.attr.name))) {
        sottoMatrice<-aaa[[i]][ which(aaa[[i]][[ lst.pnt.attr.name[indice] ]] %in% lst.pnt.attr.value[[ lst.pnt.attr.name[indice] ]]  ) , ]
         if(dim(sottoMatrice)[1]!=0 ) {
          arr.tak.sottoMatrice<-sottoMatrice$delta.dataDiagnosi
          # if(plotGraph.01==T) points(  x=arr.tak.sottoMatrice, y=rep(c(i * y.gap),length(arr.tak.sottoMatrice) ) ,pch=20, col=color  ) 
          if(plotGraph.01==T) {
            # points(  x=arr.tak.sottoMatrice, y=rep(c(i * y.gap),length(arr.tak.sottoMatrice) ) ,pch=20, col=color  ) 
            arr.punti.da.plottare.x <- c( arr.punti.da.plottare.x , arr.tak.sottoMatrice)
            arr.punti.da.plottare.y <- c( arr.punti.da.plottare.y , y=rep(c(i * y.gap),length(arr.tak.sottoMatrice) )   )
          }
          arr.occorrenze<-c(arr.occorrenze,arr.tak.sottoMatrice)
         } 
      }
    }
    # mo' plotta i punti (in differita)'
    if(plotGraph.01==T) points(  x=arr.punti.da.plottare.x, y=arr.punti.da.plottare.y ,pch=20, col=color  ) 
    # Calcola l'occorrenza in cumulativo
    arr.occorrenze<-unlist(as.numeric(arr.occorrenze))
    if(length(arr.occorrenze)>0) {
      for(i in seq(1,max(arr.occorrenze)) ) {
        occorrenza.cum<-c(occorrenza.cum,length(arr.occorrenze[ which(arr.occorrenze<=i) ])	)
        if(i>1) occorrenza.diff<-c(occorrenza.diff,occorrenza.cum[i]-occorrenza.cum[i-1])
        else occorrenza.diff<-c(occorrenza.diff,occorrenza.cum)
      } 
    }
    
    if(plotGraph.02==TRUE){
      if( length(occorrenza.diff) < xlim2pass[2] ) { 
        occorrenza.diff<-c(occorrenza.diff,rep(0,xlim2pass[2]-length(occorrenza.diff) ))
        occorrenza.cum<-c(occorrenza.cum,rep(occorrenza.cum[length(occorrenza.cum)],xlim2pass[2]-length(occorrenza.cum) )); 
      }
      plot(x = seq(1,length(occorrenza.diff)),y = occorrenza.diff ,type='l',col='red',lty=4, xlab='Time', ylab='Absolute Frequencies',main='Frequencies vs Time',xlim = xlim2pass)
      par(new=TRUE)
      plot(x = seq(1,length(occorrenza.cum)),y = occorrenza.cum ,type='l',yaxt="n", col ='blue',lwd=2, xlab='', ylab='', xlim = xlim2pass)
      axis(4)
      mtext("Cumulative Frequencies",side=4)
    }

    return(list("num.of.patient"=length(aaa),"arr.occorrenze"=arr.occorrenze,"occorrenza.cum"=occorrenza.cum,"occorrenza.diff"=occorrenza.diff))
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
    loaded.data<<-''
  } 
  #===========================================================
  costructor();
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "getEventStats"=getEventStats,
    "getProcessStats"=getProcessStats,
    "plotEventStats"=plotEventStats,
    "plotProcessStats"=plotProcessStats ,
    "timeDistribution.stats.plot"=timeDistribution.stats.plot
  ) )
  
}