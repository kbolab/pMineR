#' A class to perform Hierarchical Tree clustering on sequential data for Process Mining issues
#' 
#' @description   This class performs sequence clustering on an event-log with the Hierarchical Tree (HT) algorithm. The public methods are:
#'                \itemize{
#'                \item \code{cluster_hierarchicalTree() } is the constructor of the class
#'                \item \code{loadDataset( ...) } loads data taken from a \code{dataLoader::getData()} method, into a \code{cluster_hierarchicalTree()} object
#'                \item \code{calculateClusters() } performs the actual clustering computation on the previously loaded dataset
#'                \item \code{getClusters() } returns the clusters computed by the \code{cluster_hierarchicalTree::calculateClusters()} method
#'                \item \code{getClusterStats( ... )} returns informations about the clustering result (i.e. support, between-cluster distance, within-cluster mean distance and standard deviation)
#'                \item \code{getClusterLog( ... )} returns informations about the clustering computation itself (i.e. iterations needed to converge, centroids value after each iteration)
#'                }
#'                In order to better undestand the use of such methods, please visit: www.pminer.info
#'                
#' Parameters for \code{cluster_hierarchicalTree::calculateClusters()} method are:
#'   \itemize{
#'    \item \code{num } the number of clusters it has to generate
#'    \item \code{typeOfModel } the name of the Process Mining model it has to use to generate the space (up to now, only the default \code{"firstOrdermarkovModel"} is provided)
#'   }
#' @export
#' @import cluster
#' @examples \dontrun{
#' 
#' # create a Loader
#' obj.L<-dataLoader();  
#' 
#' # Load a .csv using "DES" and "ID" as column names to indicate events 
#' # and Patient's ID
#' obj.L$load.csv(nomeFile = "./otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES",dateColumnName = "DATA")
#' 
#' # now create an object cluster_expectationMaximization
#' obj.clEM<- cluster_expectationMaximization();    
#' 
#' # load the data into logInspector object
#' obj.clEM$loadDataset( obj.L$getData() );  
#' 
#' # perform clustering computation
#' obj.clEM$calculateClusters( num = 2);  
#' 
#' # get calculated clusters 
#' a <- obj.clEM$getClusters(); 
#' 
#' # get informations about performance of clusters
#' b <- obj.clEM$getClusterStats();  
#' 
#' # get log of each iteration of the algorithm 
#' d <- obj.clEM$getClusterLog(); 
#' }


cluster_hierarchicalTree <- function() {
  eventType <-'' 
  processInstances <-''
  obj.logI <-''
  processToCluster <-''
  clusters <-''
  logNotes <- ''
  #===========================================================
  # loadDataset
  #===========================================================  
  loadDataset<-function( dataList ) { 
    eventType <<- dataList$arrayAssociativo
    processInstances <<- dataList$wordSequence.raw
    obj.logI<<-logInspector()
    obj.logI$loadDataset( dataList )
  }  
  
  #===========================================================
  # calculateClusters
  #===========================================================  
  calculateClusters<-function(num,typeOfModel = "firstOrderMarkovModel") {
    x <- sapply(processInstances,unlist)
    #create data.frame from list 
    max.length <- max(sapply(x, length))
    x <- lapply(x, function(v) { c(v, rep("NA", max.length-length(v)))})
    xx <- do.call(rbind, x)
    xx <- as.data.frame(xx)
    #compute distance matrix
    d <- daisy(xx)
    processToCluster <- cutree(hclust(d), k = num)
    if(typeOfModel == "firstOrderMarkovModel"){
    clusters <<- list("clusters"=computeClusterTransitionMatrix(processToCluster) ,"PtoClust"=processToCluster)
    }
  }
    
  
  ##########################################
  computeClusterTransitionMatrix <- function(processToCluster){
    tmp <- list()
    subLog <- list()
    M <- list()
    for (i in 1:length(levels(as.factor(processToCluster)))){
      if(as.character(i) %in% levels(as.factor(processToCluster))){
        tmp <- which(processToCluster==i)
        subLog[[i]] <- processInstances[tmp]
        for (y in 1:length(subLog[[i]])){
          subLog[[i]][[y]] <- c("BEGIN",subLog[[i]][[y]],"END")
        }
        num.el <- sapply(subLog[[i]], length)
        res <- cbind(unlist(subLog[[i]]), rep(1:length(subLog[[i]]), num.el))
        obj <- dataProcessor()
        a <- obj$createSequenceMatrix(res[,1])
        cc <- a$transitionCountMatrix
        cc <- cc/sum(cc)
        for (n in 1:length(eventType)){
          if (!eventType[n] %in% colnames(cc)) {
            al <- vector(length = length(colnames(cc)))
            cc <- cbind(cc,al)
            colnames(cc)[dim(cc)[2]] <- paste(eventType[n])
            al <- vector(length = length(colnames(cc)))
            cc <- rbind(cc,al)
            rownames(cc)[dim(cc)[1]] <- paste(eventType[n])
          }
        }
        cc["END","BEGIN"] <- 0
        M[[i]] <- cc
      }
    }
    
    return(M)
  }
  ##########################################
  
  #===========================================================
  # getClusters
  #===========================================================  
  getClusters<-function() {
    
    return(clusters)
    
  }
  
  
  #===========================================================
  # getClusterStats
  #===========================================================  
  getClusterStats<-function() {
    
    
  }
  
  #===========================================================
  # getClusterLog
  #===========================================================  
  getClusterLog<-function() {
    
    return(logNotes)
  }
  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function() { 
    eventType <<-'' 
    processInstances <<-''
    obj.logI <-''
    processToCluster <<-''
    clusters <<-''
    logNotes <<-''
  } 
  #===========================================================
  costructor();
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "calculateClusters"=calculateClusters,
    "getClusters"=getClusters,
    "getClusterStats"=getClusterStats,
    "getClusterLog"=getClusterLog 
  ) )
  
}

