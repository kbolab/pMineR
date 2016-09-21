#' cluster_expectationMaximization class
#' 
#' @description   This class performs sequence clustering on a Log using the Partition Around Medoids (PAM) algorithm :
#'                \itemize{
#'                \item \code{costructor( ...) } is the costructor of the class
#'                \item \code{loadDataset( ...) } loads data into a cluster_partitionAroundMedoids object. It takes as input the output of the method getData from an instance of the classe dataLoader. It stores the logs and built,internally to the logInspector object, all the structures needed for the next computations.
#'                \item \code{calculateClusters() } This is the “run” command to begin the clustering  calculus. Num is the number of clusters it has to generate and typeOfModel is the name of the Process Mining model it has to use to generate the space (i.e. "firstOrdermarkovModel", "alphaAlgorithm", ...)
#'                \item \code{getClusters() } returns a list containing the calculated Clusters.
#'                \item \code{getClusterStats( ... )} It return a list containing the information about performances of clusters
#'                \item \code{getClusterLog( ... )} Because of the calculateCluster method is an iterative method, it could be of interest for a user to get the logs of eachiteration in order to have an idea of the time needed to converge.
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
#' # now create an object cluster_partitionAroundMedoids
#' obj.clPAM<- cluster_partitionAroundMedoids();    
#' 
#' # load the data into logInspector object
#' obj.clPAM$loadDataset( obj.L$getData() );  
#' 
#' # perform clustering computation
#' obj.clPAM$calculateClusters();  
#' 
#' # get calculated clusters 
#' obj.clPAM$getClusters(); 
#' 
#' # get informations about performance of clusters
#' obj.clPAM$getClusterStats();  
#' 
#' # get log of each iteration of the algorithm 
#' obj.clPAM$getClusterLog(); 
#' }


cluster_partitionAroundMedoids <- function() {
  processInstances <-''
  obj.logI <-''
  clusters <-''
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
    clusters_tmp <- list()
    transitionCountMatrix <-list()
    x <- sapply(processInstances,unlist)
    #xx <- as.data.frame(t(x))
    trans <- as(x, "transactions")
    d <- dissimilarity(trans, method = "Jaccard")
    start.time <- Sys.time()
    clustering <<- pam(d, k = num)
    end.time <- Sys.time()
    #allLabels <- predict(trans[clustering$id.med], trans, method = "Jaccard")
    #clusters <- split(trans, allLabels)
    for(i in 1:length(clustering$medoids)){
      clusters_tmp[[i]] <- processInstances[[as.numeric(clustering$medoids[i])]]
       #clusters_tmp[[i]] <- append("BEGIN",clusters_tmp[[i]])
       #clusters_tmp[[i]] <- append(clusters_tmp[[i]],"END")
      
      if(typeOfModel=="firstOrderMarkovModel"){
        transitionCountMatrix[[i]] <- createSequenceMatrix(clusters_tmp[[i]], toRowProbs = FALSE)
      }
      clusters <<- list("clusters"=transitionCountMatrix ,"PtoClust"=clustering$clustering,"clustering"=clustering, "dissimilarity"=d, "data"=x)
    }
    timeToConverge <<- end.time - start.time

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
    
    browser()
    return(timetoConverge)
  }
  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function() { 
    
    processInstances <<-''
    obj.logI <-''
    clusters <<-''
    timeToConverge <<-''
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