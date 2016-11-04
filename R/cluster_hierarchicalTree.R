#' cluster_expectationMaximization class
#' 
#' @description   This class performs sequence clusterin on a Log by an iterative Expectation-Maximization procedure (up to now, only for First Order Markov Models) :
#'                \itemize{
#'                \item \code{costructor( ...) } is the costructor of the class
#'                \item \code{loadDataset( ...) } loads data into a cluster_expectationMaximization object. It takes as input the output of the method getData from an instance of the classe dataLoader. It stores the logs and built,internally to the logInspector object, all the structures needed for the next computations.
#'                \item \code{calculateClusters() } This is the “run” command to begin the clustering  calculus. Num is the number of clusters it has to generate andtypeOfModel is the name of the Process Mining model it has to use to generate the space (i.e. "firstOrdermarkovModel", "alphaAlgorithm", ...)
#'                \item \code{getClusters() } returns a list containing the calculated Clusters.
#'                \item \code{getClusterStats( ... )} It return a list containing the information about performances of clusters (i.e. clusters support, distances among centroids, mean distance from processes and centroids, standard deviations)
#'                \item \code{getClusterLog( ... )} Because of the calculateCluster method is an iterative method, it could be of interest for a user to get the logs of eachiteration in order to have an idea of the time needed to converge. For this reason for each iteration an internal attribute calledlogNotes should be updated adding one row and the result should be made available with this method.
#'
#'                }
#' @param Parameters for calculateCluster methoda are: 
#'   \itemize{
#'    \item \code{num } the number of clusters it has to generate
#'    \item \code{typeOfModel } the name of the Process Mining model it has to use to generate the space (i.e. "firstOrdermarkovModel", "alphaAlgorithm", ...)
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
#' # now create an object cluster_expectationMaximization
#' obj.clEM<- cluster_expectationMaximization();    
#' 
#' # load the data into logInspector object
#' obj.clEM$loadDataset( obj.L$getData() );  
#' 
#' # perform clustering computation
#' obj.clEM$calculateClusters();  
#' 
#' # get calculated clusters 
#' obj.clEM$getClusters(); 
#' 
#' # get informations about performance of clusters
#' obj.clEM$getClusterStats();  
#' 
#' # get log of each iteration of the algorithm 
#' obj.clEM$getClusterLog(); 
#' }


cluster_hierarchicalTree <- function() {
  eventType <-'' 
  processInstances <-''
  obj.logI <-''
  processToCluster <-''
  clusters <-''
  logNotes <<- ''
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
    xx <- sapply(x,unique)
    trans <- as(xx, "transactions")
    dd <- dissimilarity(trans, method = "phi", which = "processID")
    dd[is.na(dd)] <- 1 # get rid of missing values
    processToCluster <- cutree(hclust(dd), k = num)
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
        cc <- createSequenceMatrix(res[,1], toRowProbs = TRUE)
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

