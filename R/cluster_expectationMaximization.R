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


cluster_expectationMaximization <- function() {
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
  calculateClusters<-function(num, typeOfModel) {
    
    if(typeOfModel == "firstOrderMarkovModel"){
      
      
      #initialize k = num random matrices
      clusterM <- list()
      logNotes <<- list()
      nRows <- length(eventType)
      nCols <- length(eventType)
        for (k in seq(1:num)){
          clusterM[[k]] <- matrix(runif(nRows*nCols), nrow=nRows,ncol=nCols)
          rownames(clusterM[[k]]) <- eventType
          colnames(clusterM[[k]]) <- eventType
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
#       (non ho capito perchè ma a volte capita... è normale? Andrebbe capito e sistemata questa 
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
          if(sum(comp) < .00001){
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
            transitionCountMatrix <- createSequenceMatrix(BprocessInstancesE, toRowProbs = FALSE)
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
          cc <- createSequenceMatrix(res[,1], toRowProbs = TRUE)
            for (n in 1:length(eventType)){
              if (!eventType[n] %in% colnames(cc)) {
                al <- vector(length = length(colnames(cc)))
                cc <- cbind(cc,al)
                colnames(cc)[dim(cc)[2]] <- paste(eventType[n])
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
        transitionMatrix[[k]] <- createSequenceMatrix(BprocessInstancesE[[k]], toRowProbs = TRUE)
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
      DistWithin[[i]] = list("mean distance"=meanDistWithin, "min distance"=minDistWithin,"max distance"=maxDistWithin,"standard deviation"=sdDistWithin, "allDistances"=tmp)
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