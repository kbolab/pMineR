#' A class to perform Partition Around Medoids clustering on sequential data for Process Mining issues
#'
#' @description   This class performs sequence clustering on an event-log with the Partition Around Medoids (PAM) algorithm. The public methods are:
#'                \itemize{
#'                \item \code{cluster_partitionAroundMedoids() } is the constructor of the class
#'                \item \code{loadDataset( ...) } loads data taken from a \code{dataLoader::getData()} method, into a \code{cluster_partitionAroundMedoids()} object
#'                \item \code{calculateClusters() } performs the actual clustering computation on the previously loaded dataset
#'                \item \code{getClusters() } returns the clusters computed by the \code{cluster_partitionAroundMedoids::calculateClusters()} method
#'                \item \code{getClusterStats( ... )} returns informations about the clustering result (i.e. support, between-cluster distance, within-cluster mean distance and standard deviation)
#'                \item \code{getClusterLog( ... )} returns informations about the clustering computation itself (i.e. iterations needed to converge, centroids value after each iteration)
#'                }
#'                In order to better undestand the use of such methods, please visit: www.pminer.info
#'
#' Parameters for \code{cluster_partitionAroundMedoids::calculateClusters()} method are:
#'   \itemize{
#'    \item \code{num } the number of clusters it has to generate
#'    \item \code{typeOfModel } the name of the Process Mining model it has to use to generate the space (up to now, only the default \code{"firstOrdermarkovModel"} is provided)
#'   }
#' @export
#' @examples \dontrun{
#' 
#' # create a Loader 
#' obj.L<-dataLoader();   
#'
#' # Load a .csv using "DES" and "ID" as column names to indicate events
#' # and Patient's ID
#' obj.L$load.csv(nomeFile = "../otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES",dateColumnName = "DATA")
#'
#' # now create an object cluster_partitionAroundMedoids
#' obj.clPAM<- cluster_partitionAroundMedoids();
#'
#' # load the data into logInspector object
#' obj.clPAM$loadDataset( obj.L$getData() );
#'
#' # perform clustering computation
#' obj.clPAM$calculateClusters(num = 2);
#'
#' # get calculated clusters
#' a <- obj.clPAM$getClusters();
#'
#' # get informations about performance of clusters
#' b <- obj.clPAM$getClusterStats();
#'
#' # get log of each iteration of the algorithm
#' d <- obj.clPAM$getClusterLog();
#' }


cluster_partitionAroundMedoids <- function() {
  processInstances <-''
  obj.logI <-''
  clusters <-''
  clustering<-list()
  timetoConverge <-''
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList ) {
    
    processInstances <<- dataList$wordSequence.raw
    obj.logI<<-logInspector()
    obj.logI$loadDataset( dataList )
  }
  
  #===========================================================
  # calculateClusters
  #===========================================================
  calculateClusters<-function(num, typeOfModel = "firstOrderMarkovModel") {
    # browser()
    clusters_tmp <- list()
    transitionCountMatrix <-list()
    x <- sapply(processInstances,unlist)
    #create data.frame from list
    max.length <- max(sapply(x, length))
    x <- lapply(x, function(v) { c(v, rep("NA", max.length-length(v)))})
    xx <- do.call(rbind, x)
    xx <- as.data.frame(xx)
    #compute distance matrix
    d <- daisy(xx)
    start.time <- Sys.time()
    clustering <<- pam(d, k = num)
    end.time <- Sys.time()
    
    for(i in 1:length(clustering$medoids)){
      clusters_tmp[[i]] <- processInstances[[clustering$medoids[i]]]
      clusters_tmp[[i]] <- append("BEGIN",clusters_tmp[[i]])
      clusters_tmp[[i]] <- append(clusters_tmp[[i]],"END")
      if(typeOfModel=="firstOrderMarkovModel"){
        obj <- dataProcessor()
        a <- obj$createSequenceMatrix(clusters_tmp[[i]])
        TCM <- a$transitionCountMatrix
        transitionCountMatrix[[i]] <- TCM/sum(TCM)
      }
      clusters <<- list("clusters"=transitionCountMatrix ,"PtoClust"=clustering$clustering,"clustering"=clustering, "dissimilarity"=d, "data"=x)
    }
    timeToConverge=NULL #aggiunto per nota "no visible binding del check"
    t <- end.time - start.time
    timeToConverge <<- as.numeric(t)
    
  }
  
  
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
    stats <- list("clusinfo"=clustering$clusinfo, "silinfo"=clustering$silinfo, "clustering"=clustering$clustering)
    return(stats)
  }
  
  #===========================================================
  # getClusterLog
  #===========================================================
  getClusterLog<-function() {
    
    return(timetoConverge)
  }
  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function() {
    processInstances <<-''
    obj.logI <<-''
    clusters <<-''
    clustering<<-list()
    timeToConverge<-NULL
    timeToConverge<<-''
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
