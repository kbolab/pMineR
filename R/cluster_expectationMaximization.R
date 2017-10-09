#' A class to perform Expectation-Maximization clustering on sequential data for Process Mining issues
#' 
#' @description   This class performs sequence clustering on an event-log with the Expectation-Maximization (EM) algorithm. The public methods are:
#'                \itemize{
#'                \item \code{cluster_expectationMaximization( ) } is the constructor of the class
#'                \item \code{loadDataset( ) } loads data taken from a \code{dataLoader::getData()} method, into a \code{cluster_expectationMaximization()} object
#'                \item \code{calculateClusters( ) } performs the actual clustering computation on the previously loaded dataset
#'                \item \code{getClusters( ) } returns the clusters computed by the \code{cluster_expectationMaximization::calculateClusters()} method
#'                \item \code{getClusterStats( )} returns informations about the clustering result (i.e. support, between-cluster distance, within-cluster mean distance and standard deviation)
#'                \item \code{getClusterLog( )} returns informations about the clustering computation itself (i.e. iterations needed to converge, centroids value after each iteration)
#'                }
#'                In order to better undestand the use of such methods, please visit: www.pminer.info
#'                
#' Parameters for \code{cluster_expectationMaximization::calculateClusters()} method are:
#'   \itemize{
#'    \item \code{num } the number of clusters it has to generate
#'    \item \code{typeOfModel } the name of the Process Mining model it has to use to generate the space (up to now, only the default \code{"firstOrdermarkovModel"} is provided)
#'   }
#' @export
#' @examples \dontrun{
#' 
#' # create a Loader
#' obj.L<-dataLoader();   # create a Loader
#' 
#' # Load a .csv using "DES" and "ID" as column names to indicate events
#' # and Patient's ID
#' obj.L$load.csv(nomeFile = "./otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES", dateColumnName = "DATA")
#' 
#' # now create an object cluster_expectationMaximization
#' obj.clEM<- cluster_expectationMaximization();
#' 
#' # load the data into logInspector object
#' obj.clEM$loadDataset( obj.L$getData() );
#' 
#' # perform clustering computation
#' obj.clEM$calculateClusters(num = 5, typeOfModel = "firstOrderMarkovModel");
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


cluster_expectationMaximization <- function() {
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
  calculateClusters<-function(num, init.centroids.random=T , init.centroids  , typeOfModel="firstOrderMarkovModel") {
    
    if(typeOfModel == "firstOrderMarkovModel"){
      
      logNotes <<- list()
      
      #initialize k = num random matrices
      if (init.centroids.random==T){
        clusterM <- list()
        nRows <- length(eventType)
        nCols <- length(eventType)
        for (k in seq(1:num)){
          clusterM[[k]] <- matrix(runif(nRows*nCols), nrow=nRows,ncol=nCols)
          rownames(clusterM[[k]]) <- eventType
          colnames(clusterM[[k]]) <- eventType
        }
      }
      
      else{
        clusterM <- init.centroids
      }
      
      # make first iteration
      iter <- 1
      aa <- list()
      aa[[iter]] <- getExpectation(num,clusterM)
      clusterM <- list()
      clusterM <- setMaximization(aa[[iter]])
      clusterM <- clusterM[ ! sapply(clusterM, is.null) ]
      #     RG -im
      #       Riga aggiunta per ovviare al caso in cui END abbia degli NaN sulla riga
      #       (non ho capito perche' ma a volte capita... e' normale? Andrebbe capito e sistemata questa 
      #       'pezza' al meglio)
      clusterM[  which(is.na(clusterM), arr.ind = TRUE) ]<-0
      #     RG -fm
      iter <- iter + 1
      M <- list()
      comp <- numeric()
      M[[iter]]<- clusterM
      logNotes[[iter]] <<- list("iteration"=iter, "expectation"=aa, "maximization"=clusterM, "control parameter"= NULL)
      
      # next iterations until convergence
      
      repeat{
        iter <- iter + 1
        # aa is the vector which associate a process (vector index) to a cluster (vector value)
        aa[[iter]] <- getExpectation(num,clusterM)
        clusterM <- list()
        # clusterM is a list of transition matrices (one for each centroid)
        clusterM <- setMaximization(aa[[iter]])
        num <- length(clusterM)
        M[[iter]]<- clusterM
        tmp <- list()
        comp <- numeric()
        # some kind of comparison (sum of each cell difference absolute value)
        for(k in 1:length(clusterM))
        {
          tmp[[k]] <- M[[iter]][[k]] - M[[iter-1]][[k]]
          comp[k] <- sum(abs(tmp[[k]])) 
        }
        logNotes[[iter]] <<- list("iteration"=iter, "expectation"=aa[[iter]], "maximization"=M[[iter]], "control parameter"=comp)
        # some convergence threshold
        if(sum(comp) < .000000000001){
          break
        }
      }
    }
    
    clusters <<- list("iterations"=iter, "clusters"=clusterM,"PtoClust"=aa[[iter]],"PtoC"=aa)
  }
  
  ##########################################################################################################
  # EXPECTATION FUNCTION - subfunction of calculateClusters: assign actual processes to "nearest" cluster 
  ##########################################################################################################
  
  
  getExpectation <- function(num,clusterM){  
    prob <- list()
    for (k in seq(1:length(clusterM))){
      transitionCountMatrix <- matrix()
      BprocessInstancesE <- list()
      numberOfProcesses <- length(processInstances)
      transitionCount <- list()
      tmp3 <- vector()
      for (p in seq(1:numberOfProcesses)){
        evt <- vector()
        tmp <- vector()
        tmp2 <- vector()
        if(processInstances[[p]][1]!="BEGIN" && processInstances[[p]][length(processInstances[[p]])]!="END"){
          BprocessInstancesE <- c("BEGIN",processInstances[[p]],"END")
        }
        else{BprocessInstancesE <- processInstances[[p]]}
        obj <- dataProcessor()
        a <- obj$createSequenceMatrix(BprocessInstancesE)
        transitionCountMatrix <- a$transitionCountMatrix
        rowNames <- row.names(transitionCountMatrix)
        for(i in 1:length(rowNames)){
          for(j in 1:length(rowNames)){
            if (transitionCountMatrix[rowNames[i],rowNames[j]] != 0){
              # total probability is product of transition probabilities and number of times the transition happended (p^n)
              tmp[j] <- (clusterM[[k]][rowNames[i],rowNames[j]])^(transitionCountMatrix[rowNames[i],rowNames[j]])
            }
          }
          tmp2[i] <- prod(tmp, na.rm = TRUE)
        }
        tmp3[p] <- prod(tmp2, na.rm = TRUE)
      }
      #prob is a list of k=number_of_cluster elements, each containing a vector of probabilities (one for each unique sequence in the Log)
      prob[[k]] <- list(tmp3)
    }
    #get maximum probability for each sequence and assign to that cluster
    prob_dataFrame <- as.data.frame(sapply(prob,unlist))
    names(prob_dataFrame) <- c(1:length(clusterM))
    processToCluster <- colnames(prob_dataFrame)[apply(prob_dataFrame,1,which.max)]
    processToCluster <- as.factor(processToCluster)
    
    return(processToCluster)
  }
  
  ##########################################################################################################
  # MAXIMIZATION FUNCTION - returns transition matrices for clusters 
  ##########################################################################################################
  
  setMaximization <- function(processToCluster){
    tmp <- list()
    subLog <- list()
    M <- list()
    for (i in 1:length(levels(processToCluster))){
      if(as.character(i) %in% levels(processToCluster)){
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
    
    #support: number of events per cluster / total number of event in Log
    supportC <- numeric()
    lastIterPtoC <- unlist(clusters$PtoC[length(clusters$PtoC)])
    allLogProcess <- length(lastIterPtoC)
    for (i in 1:length(table(lastIterPtoC))){
      supportC[i] <- table(lastIterPtoC)[i]/allLogProcess
    }
    
    
    # BETWEEN-CLUSTERs distance
    FOMM <- list(list())
    for (i in 1:length(table(lastIterPtoC))){
      FOMM[[i]] <-firstOrderMarkovModel( parameters.list=list("considerAutoLoop"=TRUE,"threshold"=0.1)  )
      FOMM[[i]]$loadDataset(dataList = list("MMatrix"=clusters$clusters[[i]]))
    }
    
    Dist <- matrix(nrow = length(table(lastIterPtoC)),ncol = length(table(lastIterPtoC)))
    for (i in 1:length(table(lastIterPtoC))){
      for (j in 1:length(table(lastIterPtoC))){
        Dist[i,j] <- as.numeric(FOMM[[i]]$distanceFrom(objToCheck = FOMM[[j]]))
      }
    }
    
    
    #WITHIN-CLUSTERs mean distance an sd (distance from cluster-assigned processes to cluster centroid)
    processFOMM <- list(list())
    BprocessInstancesE <- list()
    transitionMatrix <- list()
    for (k in 1:length(processInstances)){
      BprocessInstancesE[[k]] <- c("BEGIN",processInstances[[k]],"END")
      obj <- dataProcessor()
      a <- obj$createSequenceMatrix(BprocessInstancesE[[k]])
      TCM <- a$transitionCountMatrix
      transitionMatrix[[k]] <- TCM/sum(TCM)
      processFOMM[[k]] <-firstOrderMarkovModel( parameters.list=list("considerAutoLoop"=TRUE,"threshold"=0.1)  )
      processFOMM[[k]]$loadDataset(dataList = list("MMatrix"=transitionMatrix[[k]]))
    }
    
    DistWithin <- list()
    for (i in 1:length(table(lastIterPtoC))){
      tmp <- numeric()
      for (kk in 1:length(processInstances)){
        if (kk %in% which(lastIterPtoC==i)){
          tmp[kk]  <- as.numeric(processFOMM[[kk]]$distanceFrom(objToCheck = FOMM[[i]]))
        }
      }
      meanDistWithin = mean(tmp,na.rm = TRUE)
      minDistWithin = min(tmp,na.rm = TRUE)
      maxDistWithin = max(tmp,na.rm = TRUE)
      sdDistWithin = sd(tmp,na.rm = TRUE)
      DistWithin[[i]] = list("mean distance"=meanDistWithin, "min distance"=minDistWithin,"max distance"=maxDistWithin,"standard deviation"=sdDistWithin)
    }
    
    stats <- list("support"=supportC, "between-cluster distance"=Dist, "within-cluster distance"=DistWithin)
    return(stats)
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