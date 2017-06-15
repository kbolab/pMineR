#' Load the event-logs
#' 
#' @description  A loader for csv based log files. It also calculates the footprint table, transition matrix probabilities, and presents data in different shapes. The public methods are:
#'              \itemize{
#'              \item \code{dataLoader() } the costructor
#'              \item \code{load.csv( ... ) } loads the csv file into the \code{dataLoader} object
#'              \item \code{load.data.frame() } loads a data.frame into the \code{dataLoader} object
#'              \item \code{getData() } return the processed, previously-loaded, data
#'              \item \code{removeEvents() } remove the events in the array passed as argument (dual of \code{dataLoader::keepOnlyEvents()} )
#'              \item \code{keepOnlyEvents() } keep only the events in the array passed as argument (dual of \code{dataLoader::removeEvents()} )
#'              \item \code{addDictionary() } add a dictionary in order, afterward, to translate or group some event name
#'              \item \code{getTranslation() } perform a translation applying the given dictionary to the loaded csv or data.frame
#'              \item \code{plot.Timeline() } plot the timeline of the events regarding a single patient 
#'              } 
#'              In order to better undestand the use of such methods, please visit: www.pminer.info
#'              
#'              The consturctor admit the following parameters:
#' verbose.mode are some notification wished, during the computation? The defaul value is \code{true}
#' @param verbose.mode boolean. If TRUE some messages will appear in console, during the computation; otherwise the computation will be silent.
#' @import stringr stats progress           
#' @export
#' @examples \dontrun{
#'
#' # create a Loader
#' obj.L<-dataLoader();   
#'
#' # Load a .csv 
#' obj.L$load.csv(nomeFile = "../otherFiles/mammella.csv",
#' IDName = "CODICE_SANITARIO_ADT",
#' EVENTName = "DESC_REPARTO_RICOVERO",
#' dateColumnName = "DATA_RICOVERO")
#'
#' # return the results
#' obj.L$getData()
#' 
#' }
dataLoader<-function( verbose.mode = TRUE, max.char.length.label = 50 ) {
  arrayAssociativo<-''
  footPrint<-''
  MMatrix<-''
  pat.process<-''   
  wordSequence.raw<-''
  MM.mean.time<-''
  MM.mean.outflow.time<-''
  MM.density.list<-''
  MM.den.list.high.det<-''
  list.dictionary<-''
  list.dict.column.event.name<-''
  input.format.date<-''
  # print(max.char.length.label)
  param.IDName<-''
  param.EVENTName<-''
  param.dateColumnName<-''  
  param.verbose<-''
  param.max.char.length.label<-'';
  param.column.names<-''
  obj.LH<-''
  #=================================================================================
  # clearAttributes
  # this method clear all the attributes in order to make the object re-useable
  # for other issues ( dirty variables could have dramatic effetcs! )
  #=================================================================================    
  clearAttributes<-function() {
    costructor( verboseMode = param.verbose , max.char.length.label = param.max.char.length.label )
  }
  #=================================================================================
  # addDictionary
  #=================================================================================    
  addDictionary<-function( fileName, sep =',', dict.name='main' , column.event.name) {
    list.dictionary[[ dict.name ]] <<- read.csv(fileName,header = T,sep = sep)
    list.dict.column.event.name[[ dict.name ]] <<- column.event.name
  }    
  #=================================================================================
  # getTranslation
  #=================================================================================   
  getTranslation<-function(  column.name , dict.name = 'main', toReturn="csv") {
    # Se era stato indicato un dizionario (e la relativa colonna) caricalo
    # e popola una colonna aggiuntiva
    new.myData<-c()
    
    if(param.verbose == TRUE) obj.LH$sendLog(" 1) Converting the Events for all the patients :\n")
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(names(pat.process)), style = 3)
    pb.ct <- 0
    
    for(idPaz in names(pat.process)) {
      
      pb.ct <- pb.ct + 1; 
      if(param.verbose == TRUE) setTxtProgressBar(pb, pb.ct)
      
      matrice<-pat.process[[idPaz]]
      names(matrice)<-names(pat.process[[idPaz]])
      
      aaa<-as.character(pat.process[[idPaz]][[param.EVENTName]])

      bbb<-unlist(lapply(aaa, function(x) { 
        # prendi la voce corrispondente al nome dell'evento
        column.event.name<-list.dict.column.event.name[[ dict.name ]] 
        arrPosizioniTMP<-which(list.dictionary[[ dict.name ]][[ column.event.name ]]==x )
        if(length(arrPosizioniTMP)>1) stop("Error! an Event is associated to more possible new Event names!")
        # e sostituisci

        if(length(arrPosizioniTMP)==0) return( "" )
        else return(as.character( list.dictionary[[ dict.name ]][[ column.name ]][arrPosizioniTMP])  )
      }  ))   
      # if(param.verbose==TRUE) cat("\n Grouping now the events of the patient: ",idPaz)
      matrice[[param.EVENTName]] <- bbb
      matrice <- matrice[  which(matrice[[param.EVENTName]]!="") ,   ]
        
      new.myData <- rbind(new.myData,matrice)
    }
    
    if(param.verbose == TRUE) close(pb)
    
    if(toReturn=="csv") { daRestituire <- new.myData  }
    if(toReturn=="dataLoader"){
      if(param.verbose == TRUE) obj.LH$sendLog(" 2) Create a new dataLoader object  (this splits in many steps) :\n")
      # Istanzia un oggetto dataLoader che eridita il parametro "verbose"
      daRestituire<-dataLoader()
      daRestituire$load.data.frame(mydata = new.myData,
                                   IDName = param.IDName,EVENTName = param.EVENTName,
                                   dateColumnName = param.dateColumnName,format.column.date = "%d/%m/%Y %H:%M:%S")      
    }    
    return(daRestituire)
  }
  #=================================================================================
  # ricalcolaCSV
  # Ricalcola il CSV togliendo pazienti e/o eventi a piacere
  #=================================================================================   
  ricalcolaCSV<-function( 
            array.events.to.remove=c(), 
            array.events.to.keep=c(), 
            array.pazienti.to.remove=c(),
            array.pazienti.to.keep=c()  ) {
    matriciona <- c()
    # browser()
    # Costruisci la lista dei pazienti da analizzare
    ID.Pazienti.Validi<-names(pat.process)
    if( length(array.pazienti.to.keep) > 0  )   { ID.Pazienti.Validi <- array.pazienti.to.keep }
    else {ID.Pazienti.Validi <- names(pat.process)[ !( names(pat.process) %in% array.pazienti.to.remove )  ]  }
    
    # loopa
    if(param.verbose == TRUE) obj.LH$sendLog(" 0) Cleaning original dataset :\n")
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(ID.Pazienti.Validi), style = 3)
    pb.ct <- 1
    for(patID in ID.Pazienti.Validi  ) {
      
      pb.ct <- pb.ct + 1; 
      if(param.verbose == TRUE) setTxtProgressBar(pb, pb.ct)
      
      if(length(array.events.to.remove)>0) {
        submatrix <- pat.process[[patID]][ which( !(pat.process[[patID]][ ,param.EVENTName ] %in% array.events.to.remove  )), param.column.names]
      }
      if(length(array.events.to.keep)>0) {
        submatrix <- pat.process[[patID]][ which( (pat.process[[patID]][ ,param.EVENTName ] %in% array.events.to.keep  )), param.column.names]
      }
      if(length(array.events.to.keep)==0 & length(array.events.to.remove)==0) {
        submatrix <- pat.process[[patID]][ , param.column.names]
      }

      matriciona <- rbind( matriciona, submatrix ) 
    }
    if(param.verbose == TRUE) close(pb)
    
    return(matriciona)
  }
  #=================================================================================
  # applyFilter
  #================================================================================= 
  applyFilter<-function(
                                   array.events.to.keep=c(), 
                                   array.events.to.remove=c(),
                                   array.pazienti.to.keep=c(),
                                   array.pazienti.to.remove=c(),
                                   whatToReturn="itself") {
    
    if(!(whatToReturn %in% c( "itself" , "csv" ,"dataLoader" ) ) ) {
      obj.LH$sendLog( c(" 'whatToReturn can only be 'itself', 'csv' or 'dataLoader'! ")  ,"ERR"); return()
    }
    matriciona <- as.data.frame(ricalcolaCSV( array.events.to.remove = array.events.to.remove,
                                              array.events.to.keep = array.events.to.keep,
                                              array.pazienti.to.remove = array.pazienti.to.remove,
                                              array.pazienti.to.keep = array.pazienti.to.keep)) 
    
    IDName <- param.IDName
    EVENTName <- param.EVENTName
    dateColumnName <- param.dateColumnName
    if( whatToReturn == "itself"){
      load.data.frame( mydata = matriciona, IDName = IDName, EVENTName = EVENTName, 
                       dateColumnName = dateColumnName , format.column.date = "%d/%m/%Y %H:%M:%S", 
                       convertUTF = FALSE, suppress.invalid.date = FALSE)  
    }
    if( whatToReturn == "csv"){
      return(matriciona);  
    }    
    if( whatToReturn == "dataLoader"){
      newObj <- dataLoader();
      newObj$load.data.frame( mydata = matriciona, IDName = IDName, EVENTName = EVENTName, 
                       dateColumnName = dateColumnName , format.column.date = "%d/%m/%Y %H:%M:%S", 
                       convertUTF = FALSE, suppress.invalid.date = FALSE)        
      return(newObj);  
    }      
    
  }
  #=================================================================================
  # removeEvents
  # array.events: the array of Events to remove
  # min.abs.freq: the threshold to keep an event (absolute frequences): NOT YET IMPLEMENTED
  #================================================================================= 
  removeEvents<-function( array.events=NA) {
    
    matriciona <- as.data.frame(ricalcolaCSV( array.events.to.remove = array.events))
    IDName <- param.IDName
    EVENTName <- param.EVENTName
    dateColumnName <- param.dateColumnName
    
    load.data.frame( mydata = matriciona, IDName = IDName, EVENTName = EVENTName, 
                     dateColumnName = dateColumnName , format.column.date = "%d/%m/%Y %H:%M:%S", 
                     convertUTF = FALSE, suppress.invalid.date = FALSE)    
    # browser()
    # 
    # 
    # 
    # bbb<-array.events
    # 
    # # (1) arrayAssociativo
    # arrayAssociativo<<-arrayAssociativo[!(arrayAssociativo %in% bbb)]
    # if(is.matrix(footPrint)) { 
    #   footPrint<<-footPrint[ !(rownames(footPrint) %in% bbb),!(colnames(footPrint) %in% bbb) ]
    # }
    # 
    # # (2) MMatrix
    # MMatrix<<-MMatrix[ !(rownames(MMatrix) %in% bbb),!(colnames(MMatrix) %in% bbb) ]
    # 
    # # (3) MM.mean.time
    # MM.mean.time<<- MM.mean.time[ !(rownames(MM.mean.time) %in% bbb),!(colnames(MM.mean.time) %in% bbb) ]
    # 
    # # (4) wordSequence.raw
    # # (5) pat.process
    # new.list.density<-list()
    # for(i in seq(1,length(pat.process))) {
    #   pat.process[[i]]<<-pat.process[[i]][which(!(pat.process[[i]][[param.EVENTName]] %in% array.events)),]
    #   wordSequence.raw[[i]]<<-wordSequence.raw[[i]][  !( wordSequence.raw[[i]] %in% array.events)]
    # }
    # 
    # # (6) MM.density.list
    # for( name.from in names(MM.density.list)) {
    #   if( !(name.from %in% array.events)) {
    #     if(is.null(new.list.density[[name.from]])) new.list.density[[name.from]]<-list()
    #     for( name.to in names(new.list.density[[name.from]])) {
    #       if(!(name.to %in% array.events )) {
    #         new.list.density[[name.from]][[name.to]]<-MM.density.list[[name.from]][[name.to]]
    #       }  
    #     }
    #   }
    # }
    # MM.density.list<<-new.list.density
  } 
  #=================================================================================
  # keepOnlyEvents
  # array.events: the array of Events to keep
  #================================================================================= 
  keepOnlyEvents<-function( array.events=NA) {
    # calcola quelli da togliere
    eventi.da.togliere <- arrayAssociativo[!(arrayAssociativo %in% array.events)]
    eventi.da.togliere <- eventi.da.togliere[!(eventi.da.togliere %in% c("BEGIN","END"))]
    removeEvents(array.events = eventi.da.togliere)
  }   
  #=================================================================================
  # getAttribute
  #=================================================================================  
  getAttribute<-function( attributeName ) {
    if(attributeName=="pat.process") return( pat.process )
    if(attributeName=="MMatrix.perc") {
      MM<-MMatrix;
      for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
      return(MM);
    } 
    if(attributeName=="MMatrix") return( MMatrix )
    if(attributeName=="footPrint") return( footPrint )
    if(attributeName=="MMatrix.perc.noLoop") {
      MM<-MMatrix;
      diag(MM)<-0;
      for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
      return(MM);     
    }
    return();
  }  
  #=================================================================================
  # groupPatientLogActivity
  # raggruppa i dati, come sono da CSV in una maniera piu' consona ad essere analizzati
  #=================================================================================   
  groupPatientLogActivity<-function(mydata, ID.list.names) {

    # prendi la lista di pazienti e
    # per ogni paziente costruisci i gruppi 
    ID.list<-unique(mydata[[ID.list.names]])
    ID.act.group<-list();
    paziente.da.tenere<-c()
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(ID.list), style = 3)
    pb.ct <- 0
    for(i in ID.list) {
      
      pb.ct <- pb.ct + 1; 
      if( param.verbose == TRUE ) setTxtProgressBar(pb, pb.ct)
      # prendi i soli record che afferiscono al paziente in esame
      ID.act.group[[i]]<-mydata[ which(mydata[[ID.list.names]]==i  ), ]
      if(nrow(ID.act.group[[i]])>2) {paziente.da.tenere <- c(paziente.da.tenere,i) }
    }    
    if(param.verbose == TRUE) close(pb)
    return(
      list(
        "ID.act.group" = ID.act.group,
        "paziente.da.tenere" = paziente.da.tenere
      )
    )
  }

  setData<-function(   dataToSet  ) {
    # set the desired attribute (the ones passed as !is.na() )
    nomiAttributi<-names(dataToSet)
    
    if( "arrayAssociativo" %in%  nomiAttributi  ) arrayAssociativo<<-dataToSet$arrayAssociativo
    if( "footPrint" %in%  nomiAttributi  ) footPrint<<-dataToSet$footPrint
    if( "MMatrix" %in%  nomiAttributi  ) MMatrix<<-dataToSet$MMatrix
    if( "pat.process" %in%  nomiAttributi  ) pat.process<<-dataToSet$pat.process
    if( "wordSequence.raw" %in%  nomiAttributi  ) wordSequence.raw<<-dataToSet$wordSequence.raw

  }
  order.list.by.date<-function(   listToBeOrdered, dateColumnName, deltaDate.column.name='pMineR.deltaDate', 
                                  format.column.date = "%d/%m/%Y %H:%M:%S" ) {

    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(listToBeOrdered), style = 3)
    # Cicla per ogni paziente
    for( paziente in seq(1,length(listToBeOrdered)) ) {
      if( param.verbose == TRUE ) setTxtProgressBar(pb, paziente)
      # Estrai la matrice
      matrice.date<-listToBeOrdered[[paziente]]
      # Leggi la colonna data secondo la formattazione indicata in ingresso e riscrivila nel formato %d/%m/%Y (lo stesso viene fatto in plot.Timeline)
      newdate <- strptime(as.character(matrice.date[,dateColumnName]), format.column.date)
      matrice.date[,dateColumnName] <- format(newdate, "%d/%m/%Y %H:%M:%S")
      # Calcola la colonna delle differenze di date rispetto ad una data di riferimento ed azzera rispetto al minore
      # colonna.delta.date.TMPh898h98h9<-as.numeric(difftime(as.POSIXct(matrice.date[, dateColumnName], format = "%d/%m/%Y"),as.POSIXct("01/01/2001", format = "%d/%m/%Y"),units = 'days'))
      colonna.delta.date.TMPh898h98h9<-as.numeric(difftime(as.POSIXct(matrice.date[, dateColumnName], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct("01/01/2001 00:00:00", format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
      colonna.delta.date.TMPh898h98h9<-colonna.delta.date.TMPh898h98h9-min(colonna.delta.date.TMPh898h98h9)
      # Aggiungi la colonna dei delta data
      listToBeOrdered[[paziente]]<-cbind(listToBeOrdered[[paziente]],colonna.delta.date.TMPh898h98h9)
      colnames(listToBeOrdered[[paziente]])<-c(colnames(listToBeOrdered[[paziente]])[1:length(colnames(listToBeOrdered[[paziente]]))-1],deltaDate.column.name)
      # Ordina il data.frame di ogni paziente per la colonna DeltaT
      listToBeOrdered[[paziente]]<-listToBeOrdered[[paziente]][order(listToBeOrdered[[paziente]][[deltaDate.column.name]]),]
    }
    if(param.verbose == TRUE) close(pb)
    return(listToBeOrdered);
  } 
  load.data.frame<-function( mydata, IDName, EVENTName, dateColumnName=NA, 
                             format.column.date = "%d/%m/%Y %H:%M:%S", 
                             convertUTF = TRUE, suppress.invalid.date = TRUE) {
    # clear all the attributes
    obj.Utils <- utils()
    clearAttributes( );
    param.column.names<<-colnames(mydata)
    # browser()
    # aaaaaaa <- mydata
    if(length(mydata[[dateColumnName]]) == 0) { obj.LH$sendLog( c("dateColumnName '",dateColumnName,"' not present! ")  ,"ERR"); return() }
    if(length(mydata[[EVENTName]]) == 0) { obj.LH$sendLog( c("EVENTName '",EVENTName,"' not present! ")  ,"ERR"); return() }
    if(length(mydata[[IDName]]) == 0) { obj.LH$sendLog( c("IDName '",IDName,"' not present! ")  ,"ERR"); return() }    
    
    obj.dataProcessor <- dataProcessor()

    # Add an internal ID attribute to myData (to uniquely identify Logs)
    if(!("pMineR.internal.ID.Evt" %in% colnames(mydata) ))
      { mydata <- cbind("pMineR.internal.ID.Evt"=seq(1,nrow(mydata)),mydata ) }

    # Change the DATA FORMAT!
    mydata[[dateColumnName]] <- as.character(mydata[[dateColumnName]] )
    mydata[[dateColumnName]] <- strptime(as.character(mydata[[dateColumnName]]), format.column.date)
    mydata[[dateColumnName]] <- format(mydata[[dateColumnName]],"%d/%m/%Y %H:%M:%S")
    format.column.date <- "%d/%m/%Y %H:%M:%S"
    
    if(suppress.invalid.date==TRUE) {
      mydata <- mydata[ which(mydata[[dateColumnName]]!="" ),]
    }
    
    # Just to have then an idea of the passed parameters...
    param.IDName<<-IDName
    param.EVENTName<<-EVENTName
    param.dateColumnName<<-dateColumnName
    input.format.date<<- format.column.date
    
    # ok, let's begin!
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName    

    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    
    if(convertUTF == TRUE) {
      mydata <- obj.Utils$cleanUTF(mydata,EVENT.list.names)
      mydata[[EVENT.list.names]] <- gsub("\"", "", mydata[[EVENT.list.names]])
      mydata[[EVENT.list.names]] <- gsub("$", "", mydata[[EVENT.list.names]])
      mydata[[EVENT.list.names]] <- gsub("'", "", mydata[[EVENT.list.names]])
    }
    
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])
    if(!is.na(dateColumnName)) {
      mydata[[dateColumnName]]<-as.character(mydata[[dateColumnName]])
    }
    if(verbose.mode == TRUE) obj.LH$sendLog("\n 1) internal Grouping (1/3):\n")
    # group the log of the patient in a structure easier to handle
    ooo <- groupPatientLogActivity(mydata, ID.list.names) 
    ID.act.group<-ooo$ID.act.group
    paziente.da.tenere<-ooo$paziente.da.tenere
    
    # Se non ci sono almeno due eventi per ogni paziente, togli il paziente dalla lista
    # (e dal data frame originale)
    # Se non ci sono almeno due eventi per il paziente, toglilo dalla lista
    ID.act.group <- ID.act.group[  paziente.da.tenere  ]
    mydata <- mydata[ ( mydata[[IDName]] %in% paziente.da.tenere  ), ]
    
    # if(verbose.mode == TRUE) cat("\n 2) Ordering date:\n")
    if(verbose.mode == TRUE) obj.LH$sendLog(" 2) Ordering date (2/3):\n")
    # Order the list by the interested date (if exists)
    if(!is.na(dateColumnName)) {
      if(length(ID.act.group)==0) browser()
      ID.act.group<-order.list.by.date(listToBeOrdered = ID.act.group, dateColumnName = dateColumnName, format.column.date = format.column.date)
    }
    # if(verbose.mode == TRUE) cat("\n 3) Building MMatrices and other stuff")
    if(verbose.mode == TRUE) obj.LH$sendLog(" 3) Building MMatrices and other stuff (3/3):\n")
    
    # build the MM matrix and other stuff...
    res <- obj.dataProcessor$buildMMMatrices.and.other.structures(mydata = mydata, 
                                                                  EVENT.list.names = EVENT.list.names, 
                                                                  EVENTName = EVENTName,
                                                                  EVENTDateColumnName = param.dateColumnName,
                                                                  ID.act.group = ID.act.group,
                                                                  max.char.length.label = param.max.char.length.label,
                                                                  verbose.mode = param.verbose 
                                                                  )
    if(res$error == TRUE) { 
      if(res$errCode == 1) {obj.LH$sendLog( "event '' (BLANK) detected, please check the file\n"  ,"ERR"); return()}
      if(res$errCode == 2) {obj.LH$sendLog( c("an event has a label with a length greter than ",param.max.char.length.label," chars...\n")  ,"ERR"); return()}
      if(res$errCode == 3) {obj.LH$sendLog( "at least an event has an invalid char in the label (',$,\")\n"  ,"ERR"); return()}      
    }
    if(  sum( is.na(mydata[[dateColumnName]]) ) > 0  ) {  obj.LH$sendLog( c("at least one date is set to NA, please check loaded data and data format! (patients: ",paste(    mydata[which(is.na(mydata[[dateColumnName]])),IDName]  ,collapse = ','),") \n")  ,"ERR"); return()}      
#     res<-buildMMMatrices.and.other.structures(mydata = mydata, 
#                                               EVENT.list.names = EVENT.list.names, 
#                                               EVENTName = EVENTName, 
#                                               ID.act.group = ID.act.group)
    #populate the internal attributes
    
    # browser()
    
    arrayAssociativo<<-res$arrayAssociativo
    footPrint<<-res$footPrint
    MMatrix<<-res$MMatrix
    pat.process<<-res$pat.process
    wordSequence.raw<<-res$wordSequence.raw    
    MM.mean.time<<-res$MM.mean.time  
    MM.mean.outflow.time<<-res$MM.mean.outflow.time
    MM.density.list<<-res$MM.density.list   
    MM.den.list.high.det <<- res$MM.den.list.high.det
  }
  #=================================================================================
  # load.csv
  #=================================================================================  
  load.csv<-function( nomeFile, IDName, EVENTName,  quote="\"",sep = ",", dateColumnName=NA, 
                      format.column.date="%d/%m/%Y %H:%M:%S", 
                      convertUTF = TRUE, suppress.invalid.date = TRUE) {
    
    # load the file
    if(!file.exists(nomeFile)) { obj.LH$sendLog(c( "'",nomeFile,"' does not exist!\n" ),"ERR"); return() }
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    
    if(length(mydata)==0) { obj.LH$sendLog(c( "'",nomeFile,"' seems to be empty....\n" ),"ERR"); return() }
    if(dim(mydata)[2]==1) { obj.LH$sendLog(c( "'",nomeFile,"' seems to have only one column... check the separator!\n" ),"ERR"); return() }
    
    # Now "load" the data.frame
    load.data.frame( mydata = mydata, IDName = IDName, EVENTName = EVENTName, 
                     dateColumnName = dateColumnName , format.column.date = format.column.date, 
                     convertUTF = convertUTF, suppress.invalid.date = suppress.invalid.date)
  }
  #=================================================================================
  # plotTimeline
  #=================================================================================   
  plot.Timeline<-function( patID , table.format.date="%d/%m/%Y",output.format.date = "%d/%m/%Y",cex.axis = 0.6, cex.text = 0.7) {

   matrice <- cbind( pat.process[[ as.character(patID) ]][[param.dateColumnName]],
                         pat.process[[ as.character(patID) ]][[param.EVENTName]]) 
   # vedi stessa cosa in order.list.by.date
   newdate <- strptime(as.character(matrice[,1]), input.format.date)
   matrice[,1] <- format(newdate, table.format.date)
   
   # plotTimeline(eventTable = matrice, output.format.date = output.format.date, cex.axis = cex.axis,
   #              cex.text = cex.text )
   plotTimeline(eventTable = matrice, table.format.date = table.format.date, output.format.date = output.format.date, 
               cex.axis = cex.axis,
                cex.text = cex.text )   
  }
  #=================================================================================
  # plot.time.probability
  #=================================================================================   
  plot.transition.time.probability<-function( from.state , to.state , col.cum='red' , 
                                              col.dens = 'BLUE',
                                              prob.cumulative = TRUE, prob.density = TRUE,
                                              plotIT = TRUE, returnValues=FALSE) {
# browser()
    ppp <- MM.den.list.high.det[[ from.state ]][[ to.state ]]
    density.pp <- density(ppp,from = 0)
    # delta.x <- density(ppp)$x[ 2 ] - density(ppp)$x[ 1 ]
    delta.x <- density.pp$x[ 2 ] - density.pp$x[ 1 ]
    # min.cum.sum = cumsum(density(ppp)$y)
    min.cum.sum = cumsum(density.pp$y)
    min.cum.sum <- min.cum.sum * delta.x
    # normalizza a '1'
    min.cum.sum <- (1/max(min.cum.sum)) * min.cum.sum
    
    if( plotIT == TRUE ) {
      main <- paste(  c("Time-Transition Probability\n(",from.state," => ",to.state,")") ,collapse='')
      # plot(0,0,ylab='Prob.',xlab='Time (mins)',xlim = range(density(ppp)$x),ylim=c(0,1),col='white',main=main)
      plot(0,0,ylab='Prob.',xlab='Time (mins)',xlim = range(density.pp$x),ylim=c(0,1),col='white',main=main)
      # points(y = min.cum.sum, x = density(ppp)$x,type='l',col = col.cum)
      points(y = min.cum.sum, x = density.pp$x,type='l',col = col.cum)
      par(new=TRUE)
      # plot(y = density(ppp)$y, x = density(ppp)$x,type='l', axes = FALSE, bty = "n", xlab = "", ylab = "", col = col.dens)
      plot(y = density.pp$y, x = density.pp$x,type='l', axes = FALSE, bty = "n", xlab = "", ylab = "", col = col.dens)
      axis(4)
    }
    
    if(returnValues == TRUE) {
      return(list(
        # "x" = density(ppp)$x,
        # "density" = density(ppp)$y,
        "x" = density.pp$x,
        "density" = density.pp$y,
        "cumulative" = min.cum.sum
      ))
    }

  }
  old.plot.transition.time.probability<-function( from.state , to.state , col.cum='red' , 
                                              col.dens = 'green',
                                              prob.cumulative = TRUE, prob.density = TRUE,
                                              plotIT = TRUE, returnValues=FALSE) {
    # MM.den.list.high.det
    
    ppp <- MM.den.list.high.det[[ from.state ]][[ to.state ]]
    delta.x <- density(ppp)$x[ 2 ] - density(ppp)$x[ 1 ]
    min.cum.sum = cumsum(density(ppp)$y)
    min.cum.sum <- min.cum.sum * delta.x
    
    main <- paste(  c("Time-Transition Probability\n(",from.state," => ",to.state,")") ,collapse='')
    plot(0,0,ylab='Prob.',xlab='Time',xlim = range(density(ppp)$x),ylim=c(0,1),col='white',main=main)
    if(prob.cumulative==TRUE) points(y = min.cum.sum, x = density(ppp)$x,type='l',col = col.cum)
    if(prob.density==TRUE) points(y = density(ppp)$y, x = density(ppp)$x,type='l',col = col.dens)
    # plot(y = min.cum.sum, x = density(ppp)$x,main= main,xlab='time',ylab='Prob',type='l',col = col)
    
  }  
  #=================================================================================
  # loader
  #=================================================================================  
  getData<-function( ) {
    
    # MMatrix.perc
    MM<-MMatrix;
    for( i in seq( 1 , nrow(MM)) ) {  if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);}  } 
    MMatrix.perc<-MM
    
    # MMatrix.perc.noLoop
    MM<-MMatrix;
    diag(MM)<-0;
    for( i in seq( 1 , nrow(MM)) ) {  if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);}  } 
    MMatrix.perc.noLoop<-MM     

    return(list(
      "arrayAssociativo"=arrayAssociativo,
      "footPrint"=footPrint,
      "MMatrix"=MMatrix,
      "pat.process"=pat.process,
      "MMatrix.perc"=MMatrix.perc,
      "MMatrix.perc.noLoop"=MMatrix.perc.noLoop,
      "wordSequence.raw"=wordSequence.raw,
      "MM.mean.time"=MM.mean.time,
      "MM.density.list"=MM.density.list,
      "MM.den.list.high.det"=MM.den.list.high.det,
      "MM.mean.outflow.time"=MM.mean.outflow.time,
      "csv.column.names" = param.column.names,
      "csv.IDName"=param.IDName,
      "csv.EVENTName"=param.EVENTName,
      "csv.dateColumnName"=param.dateColumnName,
      "csv.date.format"=input.format.date
    ))
  }
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function( verboseMode , max.char.length.label  ) {
    arrayAssociativo<<-''
    footPrint<<-''
    MMatrix<<-''
    pat.process<<-'' 
    wordSequence.raw<<-''
    MM.mean.time<<-''  
    MM.mean.outflow.time<<-''
    MM.density.list<<-''    
    MM.den.list.high.det<<-''
    list.dictionary<<-list()
    list.dict.column.event.name<<-list()
    input.format.date<<-''
    # Not true data, but useful anyway
    param.IDName<<-''
    param.EVENTName<<-''
    param.dateColumnName<<-''
    param.verbose<<-verbose.mode
    param.column.names<<-''
    param.max.char.length.label<<-max.char.length.label
    
    obj.LH<<-logHandler()
    # print(timesTwo( 3.2 ))
    
  }
  costructor( verboseMode = verbose.mode, max.char.length.label = max.char.length.label )
  #================================================================================= 
  return(list(
    "load.csv"=load.csv,
    "load.data.frame"=load.data.frame,
    "getData"=getData,
    "applyFilter"=applyFilter,
    # "removeEvents"=removeEvents,
    # "keepOnlyEvents"=keepOnlyEvents,
    "addDictionary"=addDictionary,
    "getTranslation"=getTranslation,
    "plot.Timeline"=plot.Timeline,
    "plot.transition.time.probability"=plot.transition.time.probability
  ))
}