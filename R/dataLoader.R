#' load csv based log files
#' 
#' @description  A loader for csv based log files. It also calculates the footprint table, transition matrix probabilities, and presents data in different shapes. The public methods are:
#'              \itemize{
#'              \item \code{dataLoader() } the costructor
#'              \item \code{load.csv( ... ) } loads the a csv file into the \code{dataLoader} object
#'              \item \code{getData() } returns the loaded data
#'              \item \code{apply.filter() } apply a filter to logs (es: for denoising issues)
#'              \item \code{removeEvents() } remove some events from stored logs
#'              } 
#'              There are two ways to use this class: directly using the methods previously 
#'              listed or via wrapping functions (called LD.<method name>). In the examples section you will find an example of both.
#' @import stringr utils stats           
#' @export
#' @examples \dontrun{
#' # -----------------------------------------------
#' #  USING THE METHODS of the class
#' # -----------------------------------------------
#' obj.L<-dataLoader();   # create a Loader
#' 
#' # Load a .csv using "DES" and "ID" as column names to indeicate events 
#' # and Patient's ID
#' obj.L$load.csv(nomeFile = "./otherFiles/test_02.csv",IDName = "ID",
#' EVENTName = "DES")
#' 
#' # print the footprint table 
#' res<- obj.L$getData()
#' print(res$footprint)
#' 
#' 
#' # -----------------------------------------------
#' #  USING THE WRAPPER Functions
#' # -----------------------------------------------
#' # Instantiate a loader
#' obj.LD<-LD.builder()
#' 
#' # Load a CSV into the loader
#' LD.load.csv(loader.obj = obj.LD ,nomeFile = "./otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES")
#' 
#' # Instantiate a PM model
#' obj.PM <-PM.builder(kindOfObject = "alphaAlgorithm")
#' 
#' # get the data
#' res = LD.getData(loader.obj = obj.LD)
#'
#' # print the footprint table
#' print(res$footprint)
#' }
dataLoader<-function( verbose.mode = TRUE ) {
  arrayAssociativo<-''
  footPrint<-''
  MMatrix<-''
  pat.process<-''   
  wordSequence.raw<-''
  MM.mean.time<-''  
  MM.density.list<-''
  list.dictionary<-''
  list.dict.column.event.name<-''
  input.format.date<-''
  
  param.IDName<-''
  param.EVENTName<-''
  param.dateColumnName<-''  
  param.verbose<-''
  #=================================================================================
  # clearAttributes
  # this method clear all the attributes in order to make the object re-useable
  # for other issues ( dirty variables could have dramatic effetcs! )
  #=================================================================================    
  clearAttributes<-function() {
    costructor( verboseMode = param.verbose )
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
  getTranslation<-function(  column.name , dict.name = 'main') {
    # Se era stato indicato un dizionario (e la relativa colonna) caricalo
    # e popola una colonna aggiuntiva
    
    new.myData<-c()
    # uuu$pat.process
    for(idPaz in names(pat.process)) {
      matrice<-pat.process[[idPaz]]
      names(matrice)<-names(pat.process[[idPaz]])
      # for( riga in seq(1,nrow(pat.process[[idPaz]]))) {
        # browser()
        aaa<-as.character(pat.process[[idPaz]][[param.EVENTName]])
        # browser()
        # APPLY!
        # browser()
        bbb<-unlist(lapply(aaa, function(x) { 
          # prendi la voce corrispondente al nome dell'evento
          column.event.name<-list.dict.column.event.name[[ dict.name ]] 
          arrPosizioniTMP<-which(list.dictionary[[ dict.name ]][[ column.event.name ]]==x )
          if(length(arrPosizioniTMP)>1) stop("ERRORE::: RIGA RIPETUTA NEL DIZIONARIO!")
          # e sostituisci
          # browser()
#           cat("\n ",x," :: ",length(arrPosizioniTMP))
          if(length(arrPosizioniTMP)==0) return( "" )
          else return(as.character( list.dictionary[[ dict.name ]][[ column.name ]][arrPosizioniTMP])  )
        }  ))   
        if(param.verbose==TRUE) cat("\n",idPaz)
        # browser()
        matrice[[param.EVENTName]] <- bbb
        # cat("\n",riga, " = ",param.EVENTName)
        matrice <- matrice[  which(matrice[[param.EVENTName]]!="") ,   ]
      # }
      new.myData <- rbind(new.myData,matrice)
    }
#     
#     
#     if(length(column.name)  > 0 ) {
#       if(nrow(list.dictionary[[ dict.name ]])  > 0 ) {
#         aaa<-as.character(a[[matchingColumn.csv]])
#         bbb<-unlist(lapply(aaa, function(x) { 
#           arrPosizioniTMP<-which(dizionarioPrestazioni[[ matchingColumn.diz ]]==x )
#           if(length(arrPosizioniTMP)==0) return( misclassifiedName )
#           else return(as.character(dizionarioPrestazioni[  arrPosizioniTMP  ,column.name])  )
#         }  ))
#         a<-cbind(a,bbb)
#         colnames(a)<-c(colnames(a)[1:(length(colnames(a))-1)] ,newColumnName)
#       } 
#     }
    return(new.myData)
  }
  #=================================================================================
  # removeEvents
  # array.events: the array of Events to remove
  # min.abs.freq: the threshold to keep an event (absolute frequences): NOT YET IMPLEMENTED
  #================================================================================= 
  removeEvents<-function( array.events=NA) {
    bbb<-array.events
    arrayAssociativo<<-arrayAssociativo[!(arrayAssociativo %in% bbb)]
    if(is.matrix(footPrint)) { 
      footPrint<<-footPrint[ !(rownames(footPrint) %in% bbb),!(colnames(footPrint) %in% bbb) ]
    }
    MMatrix<<-MMatrix[ !(rownames(MMatrix) %in% bbb),!(colnames(MMatrix) %in% bbb) ]
    MM.mean.time<<- MM.mean.time[ !(rownames(MM.mean.time) %in% bbb),!(colnames(MM.mean.time) %in% bbb) ]
    
    new.list.density<-list()
    for(i in seq(1,length(pat.process))) {
      pat.process[[i]]<<-pat.process[[i]][which(!(pat.process[[i]][[param.EVENTName]] %in% array.events)),]
      wordSequence.raw[[i]]<<-wordSequence.raw[[i]][  !( wordSequence.raw[[i]] %in% array.events)]
    }
    for( name.from in names(MM.density.list)) {
      if( !(name.from %in% array.events)) {
        if(is.null(new.list.density[[name.from]])) new.list.density[[name.from]]<-list()
        for( name.to in names(new.list.density[[name.from]])) {
          if(!(name.to %in% array.events )) {
            new.list.density[[name.from]][[name.to]]<-MM.density.list[[name.from]][[name.to]]
          }  
        }
      }
    }
    MM.density.list<<-new.list.density
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
    for(i in ID.list) {
      ID.act.group[[i]]<-mydata[ which(mydata[[ID.list.names]]==i  ), ]
    }    
    return(ID.act.group)
  }
#   #=================================================================================
#   # buildSplittedLoader
#   #=================================================================================    
#   buildSplittedLoaderDataAndTables<-function( nomeFile, IDName, EVENTName,quote="\"",sep = ",", splitDataSet = c(.5,.5)) {
#     objLoaders<-list();
#     ID.list.names<-IDName
#     EVENT.list.names<-EVENTName    
#     # carica il file
#     mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
#     mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
#     mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])   
#     
#     # group the log of the patient in a structure easier to be handler
#     total.ID.act<-groupPatientLogActivity(mydata,ID.list.names) 
#     
#     # split the 'total.ID.act in order to separate populations
#     arrPositions<-c(1,cumsum(splitDataSet) *  length(total.ID.act))
#     partial.ID<-list();
#     for( i in seq(1,length(splitDataSet))) {
#       partial.ID[[i]]<-total.ID.act[ seq( arrPositions[i], arrPositions[i+1]) ] 
#     }
#   
#     res<-list();
#     
#     # now loop and populate the different loaders
#     for( i in seq(1,length(splitDataSet))) {
#       # build a data loader for each slitted dataset
# #      objLoaders[[i]]<-dataLoader()
#       browser()
#       # build the MM matrix and other stuff...
#       res[[i]]<-buildMMMatrices.and.other.structures(mydata = mydata, 
#                                                 EVENT.list.names = EVENT.list.names, 
#                                                 EVENTName = EVENTName, 
#                                                 ID.act.group = partial.ID[[i]])
#     }
#     return(res)
#   }  
  setData<-function(   dataToSet  ) {
    # set the desired attribute (the ones passed as !is.na() )
    nomiAttributi<-names(dataToSet)
    
    if( "arrayAssociativo" %in%  nomiAttributi  ) arrayAssociativo<<-dataToSet$arrayAssociativo
    if( "footPrint" %in%  nomiAttributi  ) footPrint<<-dataToSet$footPrint
    if( "MMatrix" %in%  nomiAttributi  ) MMatrix<<-dataToSet$MMatrix
    if( "pat.process" %in%  nomiAttributi  ) pat.process<<-dataToSet$pat.process
    if( "wordSequence.raw" %in%  nomiAttributi  ) wordSequence.raw<<-dataToSet$wordSequence.raw

  }
  order.list.by.date<-function(   listToBeOrdered, dateColumnName, deltaDate.column.name='pMineR.deltaDate', format.column.date = "%d/%m/%Y" ) {
    # if(length(listToBeOrdered)==0) return(listToBeOrdered);

    # Cicla per ogni paziente
    for( paziente in seq(1,length(listToBeOrdered)) ) {
      # Estrai la matrice
      matrice.date<-listToBeOrdered[[paziente]]
      
      # Leggi la colonna data secondo la formattazione indicata in ingresso e riscrivila nel formato %d/%m/%Y (lo stesso viene fatto in plot.Timeline)
      newdate <- strptime(as.character(matrice.date[,dateColumnName]), format.column.date)
      matrice.date[,dateColumnName] <- format(newdate, "%d/%m/%Y")
    
      # Calcola la colonna delle differenze di date rispetto ad una data di riferimento ed azzera rispetto al minore
      colonna.delta.date.TMPh898h98h9<-as.numeric(difftime(as.POSIXct(matrice.date[, dateColumnName], format = "%d/%m/%Y"),as.POSIXct("01/01/2001", format = "%d/%m/%Y"),units = 'days'))
      colonna.delta.date.TMPh898h98h9<-colonna.delta.date.TMPh898h98h9-min(colonna.delta.date.TMPh898h98h9)
      # Aggiungi la colonna dei delta data
      listToBeOrdered[[paziente]]<-cbind(listToBeOrdered[[paziente]],colonna.delta.date.TMPh898h98h9)
      colnames(listToBeOrdered[[paziente]])<-c(colnames(listToBeOrdered[[paziente]])[1:length(colnames(listToBeOrdered[[paziente]]))-1],deltaDate.column.name)
      # Ordina il data.frame di ogni paziente per la colonna DeltaT
      listToBeOrdered[[paziente]]<-listToBeOrdered[[paziente]][order(listToBeOrdered[[paziente]][[deltaDate.column.name]]),]
      if(param.verbose == TRUE) cat("\n Now ordering: ",paziente)
    }
#     browser()
    return(listToBeOrdered);
  } 
  load.data.frame<-function( mydata, IDName, EVENTName, dateColumnName=NA, format.column.date = "%d/%m/%Y") {
    # clear all the attributes
    clearAttributes( );
    
    obj.dataProcessor <- dataProcessor()
    
    # Add an internal ID attribute to myData (to uniquely identify Logs)
    if(!("pMineR.internal.ID.Evt" %in% colnames(mydata) ))
      { mydata <- cbind("pMineR.internal.ID.Evt"=seq(1,nrow(mydata)),mydata ) }
    
    # Just to have then an idea of the passed parameters...
    param.IDName<<-IDName
    param.EVENTName<<-EVENTName
    param.dateColumnName<<-dateColumnName
    input.format.date<<- format.column.date
    
    # ok, let's begin!
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName    

    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])
    if(!is.na(dateColumnName)) {
      mydata[[dateColumnName]]<-as.character(mydata[[dateColumnName]])
    }
    if(verbose.mode == TRUE) cat("\n internal Grouping")
    # group the log of the patient in a structure easier to handle
    ID.act.group<-groupPatientLogActivity(mydata, ID.list.names) 
    
    if(verbose.mode == TRUE) cat("\n Ordering date:")
    # Order the list by the interested date (if exists)
    if(!is.na(dateColumnName)) {
      if(length(ID.act.group)==0) browser()
      ID.act.group<-order.list.by.date(listToBeOrdered = ID.act.group, dateColumnName = dateColumnName, format.column.date = format.column.date)
    }
    if(verbose.mode == TRUE) cat("\n Building MMatrices and other stuff")
    # build the MM matrix and other stuff...
    res <- obj.dataProcessor$buildMMMatrices.and.other.structures(mydata = mydata, 
                                                                  EVENT.list.names = EVENT.list.names, 
                                                                  EVENTName = EVENTName,
                                                                  EVENTDateColumnName = param.dateColumnName,
                                                                  ID.act.group = ID.act.group)
#     res<-buildMMMatrices.and.other.structures(mydata = mydata, 
#                                               EVENT.list.names = EVENT.list.names, 
#                                               EVENTName = EVENTName, 
#                                               ID.act.group = ID.act.group)
    #populate the internal attributes
    
    
    arrayAssociativo<<-res$arrayAssociativo
    footPrint<<-res$footPrint
    MMatrix<<-res$MMatrix
    pat.process<<-res$pat.process
    wordSequence.raw<<-res$wordSequence.raw    
    MM.mean.time<<-res$MM.mean.time  
    MM.density.list<<-res$MM.density.list   
  }
  #=================================================================================
  # load.csv
  #=================================================================================  
  load.csv<-function( nomeFile, IDName, EVENTName,  quote="\"",sep = ",", dateColumnName=NA, format.column.date="%d/%m/%Y") {
    # load the file
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    
    # Now "load" the data.frame
    load.data.frame( mydata = mydata, IDName = IDName, EVENTName = EVENTName, dateColumnName = dateColumnName , format.column.date = format.column.date)
  }
  #=================================================================================
  # apply.filter
  #=================================================================================  
  apply.filter<-function( filter.list=list() ) {  

    if(length(filter.list) == 0 ) return;
    for( filtro in names(filter.list)) {
      if(filtro == "event.absolute.coverage.threshold" | filtro == "event.relative.coverage.threshold") {
        I.1<-logInspector(); 
        I.1$loadDataset( getData() );
        evt.stat <- I.1$getEventStats()
        proc.stat <- I.1$getProcessStats()
        
        if(filtro == "event.absolute.coverage.threshold") copertura<-evt.stat$`Absolute Coverage`
        else copertura<-evt.stat$`Absolute Coverage`/length(pat.process)
        names(copertura)<-names(evt.stat$`Absolute Coverage`)
        
        under.threshold<-names(copertura)[which(copertura<= filter.list[[filtro]]$threshold )]
        under.threshold<-under.threshold[!(under.threshold=="END" | under.threshold=="BEGIN")]
        removeEvents(array.events = under.threshold)
      }
    }
    return;
  }
  #=================================================================================
  # plotTimeline
  #=================================================================================   
  plot.Timeline<-function( patID , output.format.date = "%d/%m/%Y") {

   matrice <- cbind( pat.process[[ as.character(patID) ]][[param.dateColumnName]],
                         pat.process[[ as.character(patID) ]][[param.EVENTName]]) 
   
   # vedi stessa cosa in order.list.by.date
   newdate <- strptime(as.character(matrice[,1]), input.format.date)
   matrice[,1] <- format(newdate, output.format.date)
   
   plotTimeline(eventTable = matrice, output.format.date = output.format.date)
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
      "csv.IDName"=param.IDName,
      "csv.EVENTName"=param.EVENTName,
      "csv.dateColumnName"=param.dateColumnName
    ))
  }
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function( verboseMode  ) {
    arrayAssociativo<<-''
    footPrint<<-''
    MMatrix<<-''
    pat.process<<-'' 
    wordSequence.raw<<-''
    MM.mean.time<<-''  
    MM.density.list<<-''    
    list.dictionary<<-list()
    list.dict.column.event.name<<-list()
    input.format.date<<-''
    # Not true data, but useful anyway
    param.IDName<<-''
    param.EVENTName<<-''
    param.dateColumnName<<-''
    param.verbose<<-verbose.mode
  }
  costructor( verboseMode = verbose.mode )
  #================================================================================= 
  return(list(
    "load.csv"=load.csv,
    "load.data.frame"=load.data.frame,
    "getData"=getData,
    "removeEvents"=removeEvents,
    "keepOnlyEvents"=keepOnlyEvents,
    # "apply.filter"=apply.filter,
    "addDictionary"=addDictionary,
    "getTranslation"=getTranslation,
    "plot.Timeline"=plot.Timeline
  ))
}