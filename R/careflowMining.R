#' A class for careflow mining
#'
#' @description  This is an implementation of the First Order Markov Model (FOMM) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'              In order to better undestand the use of such methods, please visit: www.pminer.info
#'
#'              The consturctor admit the following parameters: is a boolean which indicates if the autoloops have to be admitted, while 'threshold' is the minimum value that a probability should have to do not be set to zero, in the transition matrix.
#' @export


careflowMining<-function( parameters.list = list() ) {
  parameters<-list()
  evLog <- ''
  res <- ''
  allHistEnrich <- ''
  time.unit.global <- ''


  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList , end.date , format.end.date) {

    for (pat in names(dataList$pat.process)){
      newdate <- as.Date(as.character(dataList$pat.process[[pat]][,end.date]), format.end.date)
      dataList$pat.process[[pat]][,end.date] <- format(newdate, "%d/%m/%Y %H:%M:%S")

      names(dataList$pat.process[[pat]])[which(names(dataList$pat.process[[pat]])==dataList$csv.EVENTName)] <- "EVENT"
      names(dataList$pat.process[[pat]])[which(names(dataList$pat.process[[pat]])==dataList$csv.dateColumnName)] <- "DATE_INI"
      
      if(dataList$csv.dateColumnName != end.date){
        names(dataList$pat.process[[pat]])[which(names(dataList$pat.process[[pat]])==end.date)] <- "DATE_END"
      }
      else {
        dataList$pat.process[[pat]][,"DATE_END"] <- dataList$pat.process[[pat]][,"DATE_INI"]
      }

    }

    evLog <<- dataList$pat.process

  }

  #===========================================================
  # trainModel
  #===========================================================
  trainModel<-function(time.unit = "days") {

    ######## Pre-processing
    # build mat_data (flattened event log without dates)

    time.unit.global <<- time.unit

    mat_data_names <- c("ID",seq(1:parameters$max.length))
    mat_data <- as.data.frame(matrix(data = NA, nrow = length(evLog), ncol = 1 + parameters$max.length))
    names(mat_data) <- mat_data_names
    mat_data$ID <- names(evLog)

    for(pat in names(evLog)){
      mat_data[which(mat_data$ID==pat),2:ncol(mat_data)] <- evLog[[pat]][,"EVENT"][1:parameters$max.length]
    }

    # build mat_date_start
    mat_date_start <- as.data.frame(matrix(data = NA, nrow = length(evLog), ncol = 1 + parameters$max.length))
    names(mat_date_start) <- mat_data_names
    mat_date_start$ID <- names(evLog)
    for(pat in names(evLog)){
      mat_date_start[which(mat_date_start$ID==pat),2:ncol(mat_data)] <- paste(as.Date(evLog[[pat]][,"DATE_INI"][1:parameters$max.length], format="%d/%m/%Y %H:%M:%S"))
    }

    # build mat_date_end
    mat_date_end <- as.data.frame(matrix(data = NA, nrow = length(evLog), ncol = 1 + parameters$max.length))
    names(mat_date_end) <- mat_data_names
    mat_date_end$ID <- names(evLog)
    for(pat in names(evLog)){
      mat_date_end[which(mat_date_end$ID==pat),2:ncol(mat_data)] <- paste(as.Date(evLog[[pat]][,"DATE_END"][1:parameters$max.length], format="%d/%m/%Y %H:%M:%S"))
    }

    mat <- list("mat_data"=mat_data,"mat_date_start"=mat_date_start,"mat_date_end"=mat_date_end)

    ######## End Pre-processing

    ######## Begin Mining

    selected<-selection(mat$mat_data,parameters$support.th)
    historyFE<-createhistFIRSTTIME(selected)

    #if (ncol(historyFE)>=max.length){print("NO HISTORIES, TRY LOWER SUPPORT")}

    # histories<-list()
    # EVENTdurations<-list()
    # ARCHESdurations<-list()
    # IDcounts<-list()
    # IDss<-list()
    results<-list()

    for(j in 1:nrow(historyFE)) {
      history<-data.frame()
      EVENTduration<-data.frame()
      ARCHESduration<-data.frame()
      IDcount<-data.frame()
      IDs<-list()
      results[[j]]<-list()

      #initialize history
      history<-subset(historyFE,historyFE$EVENT_1==historyFE[j,1])
      matHIST<-data.frame()
      matHIST<-subset(mat$mat_data, mat$mat_data[,2] == paste(history$EVENT_1))

      #Patients count
      tabPts <- as.data.frame(table(matHIST[,2]))
      IDcount<-data.frame(EVENT_1=tabPts$Freq)

      #Initialize the list with all the patietns "%d/%m/%Y %H:%M:%S"
      IDs<-list(matHIST[,1])

      #initialize date start and end
      matHIST_dateStart<-subset(mat$mat_date_start, mat$mat_data[,2] == paste(history$EVENT_1))
      matHIST_dateEnd<-subset(mat$mat_date_end, mat$mat_data[,2] == paste(history$EVENT_1))

      #initialize events - take the first event of the history, compute event duration
        EVENTduration<-data.frame(EVENT_1= mean(   as.integer(difftime(matHIST_dateEnd[,2],matHIST_dateStart[,2], units = time.unit ) )))

      #initialize arches - take history that end after the first event, compute arches duration
        ARCHESduration<-data.frame(EVENT_1= mean(  as.integer(difftime(matHIST_dateStart[is.na(matHIST[,3]),2] ,matHIST_dateEnd[is.na(matHIST[,3]),2] ,  units = time.unit)  )))


      for (times in 1:(parameters$max.length-1)) { #here cycle through EVENT
        # if(times>1){
        #   #browser()
        #   #current_event<-history[,length(history)]
        #   #if(length(unique(current_event))==1 && unique(current_event)=="OUT"){break}
        #   if(length(unique(event))==1 && unique(event)=="OUT") break
        # }

        #if (times==3) browser()

        #Add OUT event
        history<-cbind(history,as.character("OUT"))
        names(history)[ncol(history)]<-paste0("EVENT_",ncol(history))
        history <- data.frame(lapply(history, as.character), stringsAsFactors=FALSE)

        #Add zero into the out event
        IDcount<-cbind(IDcount,0)
        names(IDcount)[ncol(IDcount)]<-paste0("EVENT_",ncol(IDcount))

        #Add last value corresponding to out event (-1)
        EVENTduration<-cbind(EVENTduration,-1)
        names(EVENTduration)[ncol(EVENTduration)]<-paste0("EVENT_",ncol(EVENTduration))

        #Add last value corresponding to out event (-1)
        ARCHESduration<-cbind(ARCHESduration,-1)
        names(ARCHESduration)[ncol(ARCHESduration)]<-paste0("EVENT_",ncol(ARCHESduration))

        #At each time take only patients that verifies frequent histories
        PtsToTake <- c()

        #========EVENT SELECTION
        event<-history[,length(history)-1] #consider the last events of previous history

        for(y in 1:length(event)){
          if (event[y]!="OUT"){

            #CHECK to have at least one column to process in the computenewMat function
            if  (ncol(matHIST) <= 2) {break}
            else {
              #update event and dates matrix
              matNEW<-computeNewMat(event[y],matHIST)
              matNEW_dateStart<-computeNewMatDates(matHIST_dateStart,matNEW$ID)
              matNEW_dateEnd<-computeNewMatDates(matHIST_dateEnd,matNEW$ID)

              #=========select events to ADD
              selected<-selection(matNEW,parameters$support.th)
              #
              if(length(selected)==0) {}
              else{

                #Count Patients
                PtsCompute <- computeIDcount(event[y], selected, history,IDcount,IDs,matNEW)
                IDcountNew<-PtsCompute[[1]]
                IDcount<-rbind(IDcount,IDcountNew)

                #Pts Ids
                IDs<-PtsCompute[[2]]

                #Pts to take
                PtsToTake <- c(PtsToTake,PtsCompute[[3]])

                #Compute Event duration
                EVENTdurationNew<-computeEventDuration(event[y],selected,history,EVENTduration,matNEW,matNEW_dateStart,matNEW_dateEnd)
                EVENTduration<-rbind(EVENTduration,EVENTdurationNew)

                #Compute Arches duration
                ARCHESdurationNew<-computeArchesDuration(event[y],selected,history,ARCHESduration,matHIST,matHIST_dateStart,matHIST_dateEnd)
                ARCHESduration<-rbind(ARCHESduration,ARCHESdurationNew)

                #Create new history
                historynew<-createhist(event[y],selected,history)
                history<-rbind(history,historynew)

                #REMOVE DUPLICATES
                INDEX <- !duplicated(history)
                history<-history[INDEX, ]
                IDcount<-IDcount[INDEX,  ]
                EVENTduration<-EVENTduration[INDEX,  ]
                ARCHESduration<-ARCHESduration[INDEX,  ]
              }
            }
          }
        }

        if(is.data.frame(matHIST)==FALSE){break} #check if there are still column to process

        else {

          matHIST<-matHIST[matHIST$ID %in% unique(PtsToTake),-2]
          matHIST_dateEnd<-matHIST_dateEnd[matHIST_dateEnd$ID %in% unique(PtsToTake),-2]
          matHIST_dateStart<-matHIST_dateStart[matHIST_dateStart$ID %in% unique(PtsToTake),-2]
        }
      }

      # histories[[j]]<-history
      # IDcounts[[j]]<-IDcount
      # IDss[[j]]<-IDs
      # EVENTdurations[[j]]<-EVENTduration
      # ARCHESdurations[[j]]<-ARCHESduration
      #browser()
      results[[j]] <- list("history"=history,"EVENTduration"=EVENTduration,"ARCHESduration"=ARCHESduration,
                       "IDcounts"=IDcount,"IDss"=IDs)
    }

    res <<- results
    ######## End Mining

    #historyData <<- getHistoryData(results)

  }

  #===========================================================
  # getModel
  #===========================================================

  getModel <- function(){

    return(res)
  }

  #===========================================================
  # clinicalEnrichment
  #===========================================================
  enrichModel <-function( clinicalDataFrame ,clinicalDataFile="",sep=";",header=T,dec = ".", value = "", ID="", date="",date.format="") {

    historyData <- getHistoryData()
    if (clinicalDataFile!=""){
      clinicalData <- read.table(file = clinicalDataFile, sep = sep, dec = dec, header=header,stringsAsFactors = FALSE)
    }
    else {
      clinicalData <- clinicalDataFrame
    }
    
    colnames(clinicalData)[which(colnames(clinicalData)==ID)] <- "ID"
    
    if (date.format!= "%d/%m/%Y %H:%M:%S"){
      newdate <- as.Date(as.character(clinicalData[,date]), date.format)
      clinicalData[,date] <- format(newdate, "%d/%m/%Y %H:%M:%S")
    }
    
    clinicalData$data <- clinicalData[[date]]
    
    histEnrich <- list()

    for(hist in 1:length(res)){
      actualIds <- historyData$actualIds[[hist]]
      arcEnrich <- as.data.frame(matrix(data = NA,nrow = nrow(res[[hist]]$history),ncol= ncol(res[[hist]]$history)  ))
      arcEnrich.shadow <- as.data.frame(matrix(data = NA,nrow = nrow(res[[hist]]$history),ncol= ncol(res[[hist]]$history)  ))
      arcEnrich.npat <- as.data.frame(matrix(data = NA,nrow = nrow(res[[hist]]$history),ncol= ncol(res[[hist]]$history)  ))
      arcEnrich.nonavalues <- as.data.frame(matrix(data = NA,nrow = nrow(res[[hist]]$history),ncol= ncol(res[[hist]]$history)  ))
      arcEnrich.navalues <- as.data.frame(matrix(data = NA,nrow = nrow(res[[hist]]$history),ncol= ncol(res[[hist]]$history)  ))
      names(arcEnrich) <- names(res[[hist]]$history)
      history <- res[[hist]]$history

      for (j in 1:nrow(history)){
        #browser()
        tmp <- actualIds[[j]]
        clinDataTmp <- clinicalData[which(clinicalData$ID %in% tmp),]
        ll <- length(history[j,][which(history[j,]!="OUT")])

        if(history[j,dim(history)[2]] == "OUT"){
          arcEnrich[j,(ll+1):dim(arcEnrich)[2]] <- NA
          arcEnrich.shadow[j,(ll+1):dim(arcEnrich.shadow)[2]] <- "OUT"
        }

        meanValue <- numeric()

        if(ll > 1) {
          #browser()
          for(ev in 1:(ll-1)){
            datesDF <- data.frame(ID=names(evLog[tmp]),date_ini1=NA,date_ini2=NA,stringsAsFactors = FALSE)
            for(pat in names(evLog[tmp])){
            #dates <- evLog[tmp][[pat]]$DATE_INI[ev:(ev+1)]
            datesDF[which(datesDF$ID==pat),2]<-evLog[tmp][[pat]][,"DATE_INI"][ev]
            datesDF[which(datesDF$ID==pat),3]<-evLog[tmp][[pat]][,"DATE_INI"][ev+1]
            }

            allThisArchData <- merge(clinDataTmp, datesDF, by=c("ID"))
            clinSubsetByDates <- allThisArchData[which(allThisArchData$data > as.Date(as.character(allThisArchData$date_ini1),format = "%d/%m/%Y") & allThisArchData$data < as.Date(as.character(allThisArchData$date_ini2),format = "%d/%m/%Y")),]
            if(length(clinSubsetByDates[,value])!=0){
              arcEnrich[j,ev+1] <- mean(clinSubsetByDates[,value],na.rm=T)
              arcEnrich.shadow[j,ev+1] <- "ok"
              arcEnrich.npat[j,ev+1] <- length(unique(clinSubsetByDates$ID))
              arcEnrich.nonavalues[j,ev+1] <- length(clinSubsetByDates[,value])
              arcEnrich.navalues[j,ev+1] <- length(which(is.na(clinSubsetByDates[,value])))
            }
            else {
              arcEnrich[j,ev+1] <- NA
              arcEnrich.shadow[j,ev+1] <- "no values"
              arcEnrich.npat[j,ev+1] <- 0
              arcEnrich.nonavalues[j,ev+1] <- 0
              arcEnrich.navalues[j,ev+1] <- 0
            }
          }
        }
        else {
          next
        }
      }
      histEnrich[[hist]] <- list("arcEnrich"=arcEnrich,"arcEnrich.shadow"=arcEnrich.shadow,
                                 "arcEnrich.npat"=arcEnrich.npat, "arcEnrich.nonavalues"=arcEnrich.nonavalues,
                                 "arcEnrich.navalues"=arcEnrich.navalues)
    }

    allHistEnrich <<- histEnrich

  }
  #===========================================================
  # plotModel
  #===========================================================
   plot<-function( kindOfOutput ="time") {

    graphics <- list()
    bb <- getHistoryData()

    if(kindOfOutput=="time"){

     for(hist in 1:length(res)){

      HistMined<-res[[hist]]$history
      IDcountsLIST<-res[[hist]]$IDcounts
      EVENT.durations<-res[[hist]]$EVENTduration
      ARCHES.durations<-res[[hist]]$ARCHESduration
      IDsTOTTime <- bb$IDsTOTTime[[hist]]
      nodeTemp<-c()
      arches<-c()

      for (k in 1:nrow(HistMined)){
        # Create a simple NODES
        for (j in 1:ncol(HistMined)){
          if (HistMined[k,j] != "OUT"){
            nodeTemp<-c(nodeTemp, paste(j, HistMined[k,j],
                                        "\n","pts.",IDcountsLIST[k,j],
                                        "\n",paste("event duration:",time.unit.global),trunc(EVENT.durations[k,j]) ))
            arches<-c(arches,  trunc(ARCHES.durations[k,j]))
            }

          else {
            if (HistMined[k,j-1] != "OUT" ){
              nodeTemp<-c(nodeTemp, paste(k, HistMined[k,j],
                                          "\n","pts.",
                                          IDsTOTTime[k,]$TotPts,
                                          "\n",paste("Total history duration:",time.unit.global),
                                          trunc(IDsTOTTime[k,]$MeanMaxtimesDays)
              ))
              arches<-c(arches,  trunc(ARCHES.durations[k,j]))
            }}}}


      # Create a simple NDF

      nodes <-create_node_df(n= length(unique(nodeTemp)), type = "chr", label = unique(nodeTemp))

      arches <-create_node_df(n= length((arches)), type = "chr", label = (arches))
      arches<-arches[-1,]

      aaa<-data.frame( from = nodeTemp[1:length(nodeTemp)-1],to = nodeTemp[2:length(nodeTemp)])
      aaa$trans<-arches$label
      aaa<-subset(aaa,aaa$trans!="NaN")
      aaa<-unique(aaa)

      # Create a simple EDF
      edges <- create_edge_df(from=match(aaa$from,unique(nodeTemp)),
                              to = match(aaa$to,unique(nodeTemp)),
                              rel = "related")

      edges$label <-paste(time.unit.global,aaa$trans,sep=":")
      edges$label <- ifelse(edges$label ==paste(time.unit.global,"-1",sep=":"),"",edges$label)

      graph_tmp <- create_graph( nodes_df = nodes,
                                 edges_df = edges)

      graph_tmp$global_attrs[1,"theme"]<- "neato"
      graph_tmp$global_attrs[1,"value"] <- "dot"
      #browser()
      #render_graph(graph_tmp)
      graphics[[hist]] <- graph_tmp

     }
    }

      else if(kindOfOutput=="clinical"){

        for(hist in 1:length(res)){

          HistMined<-res[[hist]]$history
          IDcountsLIST<-res[[hist]]$IDcounts
          EVENT.durations<-res[[hist]]$EVENTduration
          ARCHES.enrichment <- allHistEnrich[[hist]]$arcEnrich
          ARCHES.enrichment.shadow <- allHistEnrich[[hist]]$arcEnrich.shadow
          ARCHES.enrichment.npat <- allHistEnrich[[hist]]$arcEnrich.npat
          ARCHES.enrichment.nonavalues <- allHistEnrich[[hist]]$arcEnrich.nonavalues
          ARCHES.enrichment.navalues <- allHistEnrich[[hist]]$arcEnrich.navalues
          IDsTOTTime <- bb$IDsTOTTime[[hist]]
          nodeTemp<-c()
          enrich <- c()
          enrich.shadow <- c()
          enrich.npat <- c()
          enrich.nonavalues <- c()
          enrich.navalues <- c()
          ARCHES.enrichment[,1] <- NaN
          single.ARCHES.enrichment <- as.data.frame((matrix(data = NA,nrow = nrow(ARCHES.enrichment),
                                                            ncol =ncol(ARCHES.enrichment) )))
          single.ARCHES.enrichment.npat <- as.data.frame((matrix(data = NA,nrow = nrow(ARCHES.enrichment.npat),
                                                            ncol =ncol(ARCHES.enrichment.npat) )))
          single.ARCHES.enrichment.nonavalues <- as.data.frame((matrix(data = NA,nrow = nrow(ARCHES.enrichment.nonavalues),
                                                            ncol =ncol(ARCHES.enrichment.nonavalues) )))
          single.ARCHES.enrichment.navalues <- as.data.frame((matrix(data = NA,nrow = nrow(ARCHES.enrichment.navalues),
                                                            ncol =ncol(ARCHES.enrichment.navalues) )))

          # Average on different enrichment groups on single arch (those patients who make the transition and might split on the next arch)

          for (ii in 2:ncol(HistMined)){
            if(length(which(duplicated(HistMined[,1:ii]) | duplicated(HistMined[,1:ii], fromLast = TRUE)))!=0){
              unici <- unique(HistMined[,1:ii][which(duplicated(HistMined[,1:ii]) | duplicated(HistMined[,1:ii], fromLast = TRUE)),])
              for(zz in 1:nrow(unici)){
                arches.toaverage <- numeric()
                index.toget <- numeric()
                pat.tosum <- numeric()
                nonavalues.tosum <- numeric()
                navalues.tosum <- numeric()

                for (kk in 1:nrow(HistMined)){
                    if(all(HistMined[kk,1:ii]==unici[zz,])){
                    arches.toaverage[kk] <- ARCHES.enrichment[kk,ii]
                    pat.tosum[kk] <- ARCHES.enrichment.npat[kk,ii]
                    nonavalues.tosum[kk] <- ARCHES.enrichment.nonavalues[kk,ii]
                    navalues.tosum[kk] <- ARCHES.enrichment.navalues[kk,ii]
                    index.toget[kk] <- kk
                    }
                  else{
                    next
                  }
                }
              index.toget <- index.toget[which(!is.na(index.toget))]
              single.ARCHES.enrichment[index.toget,ii] <- mean(arches.toaverage,na.rm = T)
              single.ARCHES.enrichment.npat[index.toget,ii] <- sum(pat.tosum,na.rm = T)
              single.ARCHES.enrichment.nonavalues[index.toget,ii] <- sum(nonavalues.tosum,na.rm = T)
              single.ARCHES.enrichment.navalues[index.toget,ii] <- sum(navalues.tosum,na.rm = T)
              }
            }
            else {
              single.ARCHES.enrichment[,ii] <- ARCHES.enrichment[,ii]
              single.ARCHES.enrichment.npat[,ii] <- ARCHES.enrichment.npat[,ii]
              single.ARCHES.enrichment.nonavalues[,ii] <- ARCHES.enrichment.nonavalues[,ii]
              single.ARCHES.enrichment.navalues[,ii] <- ARCHES.enrichment.navalues[,ii]
              }
          }


          #update arches enrichment shadow based on previous step (no values single arch is ok when parallel to a valued one)
          for (n in 1:ncol(ARCHES.enrichment.shadow)){
            noval.index <- which(ARCHES.enrichment.shadow[,n] == "no values" & !is.na(single.ARCHES.enrichment[,n]))
            ARCHES.enrichment.shadow[noval.index,n]<- "ok"
          }

          #Build nodes and edges

          for (k in 1:nrow(HistMined)){
            # Create a simple NODES
            for (j in 1:ncol(HistMined)){
              if (HistMined[k,j] != "OUT"){
                nodeTemp<-c(nodeTemp, paste(j, HistMined[k,j],
                                            "\n","pts.",IDcountsLIST[k,j],
                                            "\n","event duration",trunc(EVENT.durations[k,j]) ))
                enrich <- c(enrich, trunc(single.ARCHES.enrichment[k,j]))
                enrich.shadow <- c(enrich.shadow, ARCHES.enrichment.shadow[k,j])
                enrich.npat <- c(enrich.npat, single.ARCHES.enrichment.npat[k,j])
                enrich.nonavalues <- c(enrich.nonavalues, single.ARCHES.enrichment.nonavalues[k,j])
                enrich.navalues <- c(enrich.navalues, single.ARCHES.enrichment.navalues[k,j])
              }

              else {
                if (HistMined[k,j-1] != "OUT" ){
                  nodeTemp<-c(nodeTemp, paste(k, HistMined[k,j],
                                              "\n","pts.",
                                              IDsTOTTime[k,]$TotPts,
                                              "\n","Total history duration",
                                              trunc(IDsTOTTime[k,]$MeanMaxtimesDays)
                  ))
                  enrich <- c(enrich, trunc(single.ARCHES.enrichment[k,j]))
                  enrich.shadow <- c(enrich.shadow, ARCHES.enrichment.shadow[k,j])
                  enrich.npat <- c(enrich.npat, single.ARCHES.enrichment.npat[k,j])
                  enrich.nonavalues <- c(enrich.nonavalues, single.ARCHES.enrichment.nonavalues[k,j])
                  enrich.navalues <- c(enrich.navalues, single.ARCHES.enrichment.navalues[k,j])
                }}}}


        # Create a NDF
        nodes <-create_node_df(n= length(unique(nodeTemp)), type = "chr", label = unique(nodeTemp))

        #add al the things we need for output
        enrich <-create_node_df(n= length(enrich), type = "chr", label = enrich)
        enrich.shadow <-create_node_df(n= length(enrich.shadow), type = "chr", label = enrich.shadow)
        enrich.npat <-create_node_df(n= length(enrich.npat), type = "chr", label = enrich.npat)
        enrich.nonavalues <-create_node_df(n= length(enrich.nonavalues), type = "chr", label = enrich.nonavalues)
        enrich.navalues <-create_node_df(n= length(enrich.navalues), type = "chr", label = enrich.navalues)
        enrich<-enrich[-1,]
        enrich.shadow<-enrich.shadow[-1,]
        enrich.npat<-enrich.npat[-1,]
        enrich.nonavalues<-enrich.nonavalues[-1,]
        enrich.navalues<-enrich.navalues[-1,]

        aaa<-data.frame( from = nodeTemp[1:length(nodeTemp)-1],to = nodeTemp[2:length(nodeTemp)])
        aaa$trans<-enrich$label
        aaa$trans.shadow <-enrich.shadow$label
        aaa$trans.npat <-enrich.npat$label
        aaa$trans.nonavalues <-enrich.nonavalues$label
        aaa$trans.navalues <-enrich.navalues$label
        aaa<-aaa[which(!is.na(aaa$trans.shadow)),]
        #aaa <- subset(aaa, !duplicated(aaa[,1:2]))
        aaa <- unique(aaa)

        # Create a simple EDF
        edges <- create_edge_df(from=match(aaa$from,unique(nodeTemp)),
                                to = match(aaa$to,unique(nodeTemp)),
                                rel = "related")

        edges$label <-paste("mean val.",aaa$trans,"\n",
                            "pts.",aaa$trans.npat,"\n",
                            "obs.",aaa$trans.nonavalues,"\n",
                            "NAs",aaa$trans.navalues)
        edges$label <- ifelse(aaa$trans.shadow =="OUT","",edges$label)
        edges$label <- ifelse(aaa$trans.shadow =="no values","no values",edges$label)

        graph_tmp <- create_graph( nodes_df = nodes,
                                   edges_df = edges)

        graph_tmp$global_attrs[1,"value"] <- "dot"
        #browser()
        #render_graph(graph_tmp)
        graphics[[hist]] <- graph_tmp

        }
      }
    return(graphics)
 }

  #===========================================================
  #===================BEGIN PRIVATE FUNCTIONS=================
  #===========================================================


  selection <- function(mat,support.th){
    #check how many time an event is the first event of histories
    tab <- as.data.frame(table(mat[,2]))
    #Compute the support (supp) for each first event
    tab$supp=tab$Freq/nrow(mat)
    #select the first events where the threshold is verified
    selected<-tab[which(tab$supp >= support.th ),1]
    return(selected)
  }

  createhistFIRSTTIME <- function(selected){
    historyFE<- data.frame()
    #create histories
    for (k in 1:length(selected)){
      historyFE[k,1]<-paste(selected[k])
    }
    names(historyFE)[1]<-paste("EVENT_1")
    return(historyFE)
  }

  computeNewMat<- function(event,mat){
    #select only histories starting with first frequent event
    mat_new<-mat[which(mat[,2] == event),]

    #delete first event column
    mat_new<-mat_new[,-2]
    out<-subset(mat_new, is.na(mat_new[,2])==T)
    mat_new<-subset(mat_new, is.na(mat_new[,2])==F)

    return(mat_new)
  }

  computeNewMatDates<- function(mat,IDs){
    #update date matrix
    mat_NEW<-mat[which(mat$ID %in% IDs),]
    mat_NEW<-mat_NEW[,-2]
    return(mat_NEW)
  }

  createhist <- function(event, selected, history){
    totake<-history[,ncol(history)-1]==event
    toadd<-c(history[totake,1:ncol(history)-1])
    #create histories
    for (k in 1:length(selected)){
      addtohist<-c(toadd,paste(selected[k]))
      addtohist <- data.frame(lapply(addtohist, as.character), stringsAsFactors=FALSE)
      names(addtohist)<-names(history)
      history<-rbind(history,addtohist)
    }
    return(history)
  }


  computeIDcount <- function(event, selected, history,IDcount,IDs,matNEW){
    totakeID<-history[,ncol(history)-1]==event
    toaddID<-c(IDcount[totakeID,1:ncol(IDcount)-1])
    IdsToTake <- c()

    for (k in 1:length(selected)) {
      IDcounttoADD<- length(matNEW[which(matNEW[,2] == selected[k]),1])

      addtoId<-c(toaddID,IDcounttoADD)
      addtoId <- data.frame(lapply(addtoId, as.numeric), stringsAsFactors=FALSE)
      names(addtoId)<-names(IDcount)

      IDcount<-rbind(IDcount,addtoId)

      #ID of patients
      IDs[[length(IDs)+1]] <- matNEW[which(matNEW[,2] == selected[k]),1]

      #Update Pts to take
      IdsToTake <- c(IdsToTake,matNEW[which(matNEW[,2] == selected[k]),1])
    }

    my_list <- list(IDcount, IDs,IdsToTake)
    return(my_list)
  }

  computeEventDuration <- function(event,selected,history,EVENTduration,matNEW,matNEW_dateStart,matNEW_dateEnd){
    totakeE<-history[,ncol(history)-1]==event
    toaddE<-c(EVENTduration[totakeE,1:ncol(EVENTduration)-1])
    for (k in 1:length(selected)){
      EVNTini<-subset(matNEW_dateStart, matNEW[,2]==selected[k])
      EVNTend<-subset(matNEW_dateEnd, matNEW[,2]==selected[k])


      # EVENTdurationSELECTED<-mean((EVNTend[,2] - EVNTini[,2]), na.rm = TRUE)
      EVENTdurationSELECTED<-mean(    difftime(EVNTend[,2] ,EVNTini[,2], units  = time.unit.global ) , na.rm = TRUE)


      toaddevnt<-c(toaddE,EVENTdurationSELECTED)
      toaddevnt <- data.frame(lapply(toaddevnt, as.numeric), stringsAsFactors=FALSE)
      names(toaddevnt)<-names(EVENTduration)

      EVENTduration<-rbind(EVENTduration,toaddevnt)
    }

    return(EVENTduration)
  }

  computeArchesDuration <- function(event,selected,history,ARCHESduration,matHIST,matHIST_dateStart,matHIST_dateEnd){
    totakeA<-history[,ncol(history)-1]==event
    toaddA<-c(ARCHESduration[totakeA,1:ncol(ARCHESduration)-1])

    for (k in 1:length(selected)){

      Archesini<-subset(matHIST_dateStart, matHIST[,2]==event & matHIST[,3]==selected[k])
      Archesend<-subset(matHIST_dateEnd, matHIST[,2]==event & matHIST[,3]==selected[k])
      # ArchesSELECTED<-mean((Archesini[,3] - Archesend[,2]),na.rm = TRUE)
      ArchesSELECTED<-mean(     as.integer(difftime(Archesini[,3] , Archesend[,2], units = time.unit.global) )   ,na.rm = TRUE)

      toaddevnt<-c(toaddA,ArchesSELECTED)
      toaddevnt <- data.frame(lapply(toaddevnt, as.numeric), stringsAsFactors=FALSE)
      names(toaddevnt)<-names(ARCHESduration)

      ARCHESduration<-rbind(ARCHESduration,toaddevnt)
    }

    return(ARCHESduration)
  }


  getHistoryData <- function() {

    HistMined <- list()
    IDsTOTTime <- list()
    actualIds <- list()
    for (hist in 1:length(res)){

      HistMined[[hist]]<-res[[hist]]$history
      IDs<-res[[hist]]$IDss
      actualIds[[hist]] <- list()

      #Number of Events in the History
      LengthHist<-c()


      for (j in 1:nrow(HistMined[[hist]])){
        LengthHistTMP<-length(HistMined[[hist]][j,HistMined[[hist]][j,]!="OUT"])
        LengthHist<-c(LengthHist,LengthHistTMP)
      }
      HistMined[[hist]]$LengthHistEvnt<-LengthHist

      alltheID<-unique(unlist(IDs))
      HistMined[[hist]]$TotPts<-NA
      HistMined[[hist]]$MeanMaxtimes<-NA


      for (j in 1:nrow(HistMined[[hist]])){
        #Pts in the history but no in the those mined at the following iteration (hisotry lenght plus 1)

        IntheHist <-unique(unlist(IDs[[j]]))
        HistToTakeLength<-  as.integer(HistMined[[hist]]$LengthHistEvnt[j]+1)
        InOtherHist<- unique(unlist( IDs[HistMined[[hist]]$LengthHistEvnt == HistToTakeLength]))

        IDDDSSS<-alltheID[alltheID %in% IntheHist]
        IDDDSSS<-IDDDSSS[!IDDDSSS %in% InOtherHist]

          HistMined[[hist]]$TotPts[j]<-length(IDDDSSS)
        if(length(IDDDSSS)!=0){
          LogID<-evLog[IDDDSSS]
          HistMined[[hist]]$MeanMaxtimes[j] <-mean(sapply(1:length(LogID), function(i) max(as.numeric(LogID[[i]]$pMineR.deltaDate))))
          actualIds[[hist]][[j]] <- IDDDSSS
          }
        else{
          HistMined[[hist]]$TotPts[j]<- 0
          HistMined[[hist]]$MeanMaxtimes[j] <- 0
          actualIds[[hist]][[j]] <- NA
        }


      }

      #trnasform in Days
      HistMined[[hist]]$MeanMaxtimesDays<- (HistMined[[hist]]$MeanMaxtimes/60)/24
      IDsTOTTime[[hist]]<-HistMined[[hist]][,(ncol(HistMined[[hist]])-3):ncol(HistMined[[hist]])]

      }

    output <- list("IDsTOTTime"=IDsTOTTime,"actualIds"=actualIds)
    return(output)
  }
  #===========================================================
  #===================END PRIVATE FUNCTIONS===================
  #===========================================================


  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = list() ) {
    parameters <<-parametersFromInput
    evLog <<- ''
    res <<- ''
    allHistEnrich <<- ''
    time.unit.global <<- ''

  }
  #===========================================================
  costructor( parametersFromInput = parameters.list);
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "trainModel"=trainModel,
    "getModel"=getModel,
    # "play"=play,
    "enrichModel"=enrichModel,
    "plot"=plot

  ) )
}
