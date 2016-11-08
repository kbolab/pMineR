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
#' @useDynLib pMineR    
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
  
  param.IDName<-''
  param.EVENTName<-''
  param.dateColumnName<-''  
  param.verbose<-''
  #=================================================================================
  # clearAttributes
  #=================================================================================    
  clearAttributes<-function() {
    costructor( verboseMode = param.verbose )
  }
  #=================================================================================
  # buildFootPrintTable
  #=================================================================================   
  buildFootPrintTable<-function( MM ) {
    actionList<-list();
    FF<-array("#",dim=dim(MM));
    colnames(FF)<-colnames(MM);  rownames(FF)<-rownames(MM)
    elementi<-expand.grid(rownames(MM),rownames(MM))
    for( riga in seq(1,nrow(elementi))) {
      if(elementi[riga,1] == elementi[riga,2]) { FF[ elementi[riga,1] , elementi[riga,2]]<-"#" }
      if(  MM[elementi[riga,1],elementi[riga,2]] == 0 & MM[elementi[riga,2],elementi[riga,1]]!=0  ) { FF[ elementi[riga,1] , elementi[riga,2]]<-"<-" }
      if(  MM[elementi[riga,1],elementi[riga,2]] != 0 & MM[elementi[riga,2],elementi[riga,1]]==0  ) { FF[ elementi[riga,1] , elementi[riga,2]]<-"->" }
      if(  MM[elementi[riga,1],elementi[riga,2]] != 0 & MM[elementi[riga,2],elementi[riga,1]]!=0  ) { FF[ elementi[riga,1] , elementi[riga,2]]<-"||" }
    }
    # CHIODO
    for(i in seq(1,nrow(FF))) FF[i,i]<-'#'
    return(FF);
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
        bbb<-unlist(lapply(aaa, function(x) { 
          # prendi la voce corrispondente al nome dell'evento
          column.event.name<-list.dict.column.event.name[[ dict.name ]] 
          arrPosizioniTMP<-which(list.dictionary[[ dict.name ]][[ column.event.name ]]==x )
          # e sostituisci
          # browser()
#           cat("\n ",x," :: ",length(arrPosizioniTMP))
          if(length(arrPosizioniTMP)==0) return( "" )
          else return(as.character( list.dictionary[[ dict.name ]][[ column.name ]][arrPosizioniTMP])  )
        }  ))   
        # cat("\n",idPaz)
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
  # buildFootPrintTable.plus
  #=================================================================================   
  buildFootPrintTable.plus<-function( MM , wordsSeq ) {
    actionList<-list();

    FF<-array("#",dim=dim(MM));
    colnames(FF)<-colnames(MM);  rownames(FF)<-rownames(MM)
    elementi<-expand.grid(rownames(MM),rownames(MM))
    for( riga in seq(1,nrow(elementi))) {
      if(elementi[riga,1] == elementi[riga,2]) {FF[ elementi[riga,1] , elementi[riga,2]]<-"#"}
      if(  MM[elementi[riga,1],elementi[riga,2]] == 0 & MM[elementi[riga,2],elementi[riga,1]]!=0  ) { FF[ elementi[riga,1] , elementi[riga,2]]<-"<-" }
      if(  MM[elementi[riga,1],elementi[riga,2]] != 0 & MM[elementi[riga,2],elementi[riga,1]]==0  ) { FF[ elementi[riga,1] , elementi[riga,2]]<-"->" }
      if(  MM[elementi[riga,1],elementi[riga,2]] != 0 & MM[elementi[riga,2],elementi[riga,1]]!=0  ) { FF[ elementi[riga,1] , elementi[riga,2]]<-"||" }
    }
    # CHIODO
    for(i in seq(1,nrow(FF))) FF[i,i]<-'#'
    
    # ora scorri la lista delle parole e cerca di infilare diamanti e triangoli
    # T = TRIANGOLI
    for(i in names(wordsSeq)) {
      for( pos in seq(1,length(wordsSeq[[i]])-2 ) ) {
        # quello in mezzo deve essere per forza diverso?!??!??!?!
        # aspetta a cancellare l'IF sottostante!
        #if(  wordsSeq[[i]][pos] == wordsSeq[[i]][pos+2] ) {
        if(  wordsSeq[[i]][pos] == wordsSeq[[i]][pos+2] & wordsSeq[[i]][pos]!=wordsSeq[[i]][pos+1]) {
          FF[  wordsSeq[[i]][pos] , wordsSeq[[i]][pos+1]  ] = "T"
        }
      }
    }
     browser()
    aa<-as.data.frame(which(FF=="T",arr.ind = T)) # cerca i T e mettili in un dataFrame
    for(i in seq(1,nrow(aa))) {
      rigaComplementare<-which(  aa$row == aa$col[i]  & aa$col== aa$row[i])
      if(length(rigaComplementare)==1) {
        FF[ aa$row[i], aa$col[i] ]<-"D"
        FF[ aa$col[i], aa$row[i] ]<-"D"
      }
    }
    browser()
    return(FF);
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
  #=================================================================================
  # buildMMMatrices.and.other.structures
  # costruisce la MM matrix ed anche altra robaccia
  #=================================================================================    
  buildMMMatrices.and.other.structures<-function(mydata, EVENT.list.names, EVENTName, ID.act.group) {
    # costruisci la matrice
    MM<-matrix(0, ncol=length(unique(mydata[[EVENT.list.names]]))+2, nrow=length(unique(mydata[[EVENT.list.names]]))+2 )
    colnames(MM)<-c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]])))
    rownames(MM)<-colnames(MM)
    # Creiamo anche la matrice con le density dei tempi di transizione
    # (ma solo se c'è un campo DATA TIME)
    MM.den.list<-list()
    # ID.act.group[[1]][EVENT.list.names]<-as.character(ID.act.group[[1]][EVENT.list.names])
    # browser()
    # ora scorri la storia dei singoli pazienti per estrarre le ricorrenze
    # per ogni paziente
    for(patID in seq(1,length(ID.act.group))) {
      cat(str_c("\n   ->",patID))
      # su ogni elemento del percorso clinico
      # t e' il "tempo" in senso di "step"
      for(t in seq(1,nrow(ID.act.group[[patID]]))) {
        # vedi se devi legare il BEGIN
        if( t == 1) {
          valore<-MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]
          #MM[ "BEGIN", ID.act.group[[patID]][ t ,ID.list.names] ]<-valore+1
          MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]<-valore+1
        }
        # vedi se devi legare l'END   
        if( t == nrow(ID.act.group[[patID]])) {
          nomeCampo<-ID.act.group[[patID]][t,EVENT.list.names]
          MM[nomeCampo,"END"]<-MM[nomeCampo,"END"]+1
        }
        # tutti gli altri
        if( t < nrow(ID.act.group[[patID]])) {
          
          nomeCampo.pre<-ID.act.group[[patID]][t,EVENT.list.names]
          nomeCampo.post<-ID.act.group[[patID]][t+1,EVENT.list.names]
          MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
          if(param.dateColumnName!='' & ! is.na(param.dateColumnName)){
            delta.date<-as.numeric(difftime(as.POSIXct(ID.act.group[[patID]][t+1,param.dateColumnName], format = "%d/%m/%Y"),as.POSIXct(ID.act.group[[patID]][t,param.dateColumnName], format = "%d/%m/%Y"),units = 'days'))
            if(length(MM.den.list[[ nomeCampo.pre]])==0) MM.den.list[[ nomeCampo.pre]]<-list()
            if(length(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]])==0) MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c()
            MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]],delta.date)
          }
        }    
      }
    }
    quanti.da.fare<-length(names(MM.den.list)) * length(names(MM.den.list))
    cat(str_c("\n mean times : ",quanti.da.fare," \n"))
    # browser()
    # Calcola la matrice delle medie dei tempi
    # Sarebbe bello avere le density... vabbè. più avanti
    if(param.dateColumnName!='' & !is.na(param.dateColumnName)){
      MM.mean.time<-MM
      MM.mean.time[ 1:nrow(MM.mean.time) , 1:ncol(MM.mean.time)   ]<-Inf
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          cat(str_c("."))
          # if(length(MM.den.list[[ state.from]][[ state.from ]])!=0)
          MM.mean.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]])
#           else 
#             MM.density[state.from,state.from ]<-Inf
        }        
      }
    }
    # browser()
    # costruisci una semplice versione, con le parole (come piace tanto a Van der Aalst)
    cat(str_c("\n simple version: ",length(seq(1,length(ID.act.group))),"\n"))
    wordSequence.TMP01<-list();
    for(i in seq(1,length(ID.act.group))) {
      cat(str_c("*"))
      IDPat<-names(  ID.act.group)[i]
      wordSequence.TMP01[[IDPat]]<-ID.act.group[[ IDPat ]][[EVENTName]]
    }    
    cat(str_c("\n End"))
    # browser()
#     return(list( "arrayAssociativo" = rownames(MM),
#                  "footPrint"=buildFootPrintTable(MM),
# #                 "footPrint.plus"=buildFootPrintTable.plus(MM = MM, wordsSeq = wordSequence.TMP01),
#                  "MMatrix"=MM,
#                  "MM.mean.time"=MM.mean.time,
#                  "MM.density.list"=MM.den.list,
#                  "pat.process"=ID.act.group,
#                  "wordSequence.raw"=wordSequence.TMP01) )
    return(list( "arrayAssociativo" = rownames(MM),
                 "footPrint"="",
                                  # "footPrint.plus"=buildFootPrintTable.plus(MM = MM, wordsSeq = wordSequence.TMP01),
                 "MMatrix"=MM,
                 "MM.mean.time"=MM.mean.time,
                 "MM.density.list"=MM.den.list,
                 "pat.process"=ID.act.group,
                 "wordSequence.raw"=wordSequence.TMP01) )    
  }
  #=================================================================================
  # buildSplittedLoader
  #=================================================================================    
  buildSplittedLoaderDataAndTables<-function( nomeFile, IDName, EVENTName,quote="\"",sep = ",", splitDataSet = c(.5,.5)) {
    objLoaders<-list();
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName    
    # carica il file
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])   
    
    # group the log of the patient in a structure easier to be handler
    total.ID.act<-groupPatientLogActivity(mydata,ID.list.names) 
    
    # split the 'total.ID.act in order to separate populations
    arrPositions<-c(1,cumsum(splitDataSet) *  length(total.ID.act))
    partial.ID<-list();
    for( i in seq(1,length(splitDataSet))) {
      partial.ID[[i]]<-total.ID.act[ seq( arrPositions[i], arrPositions[i+1]) ] 
    }
  
    res<-list();
    
    # now loop and populate the different loaders
    for( i in seq(1,length(splitDataSet))) {
      # build a data loader for each slitted dataset
#      objLoaders[[i]]<-dataLoader()
      browser()
      # build the MM matrix and other stuff...
      res[[i]]<-buildMMMatrices.and.other.structures(mydata = mydata, 
                                                EVENT.list.names = EVENT.list.names, 
                                                EVENTName = EVENTName, 
                                                ID.act.group = partial.ID[[i]])
    }
    return(res)
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
  order.list.by.date<-function(   listToBeOrdered, dateColumnName, deltaDate.column.name='pMineR.deltaDate', format.column.date="%d/%m/%Y"  ) {
    
    if(format.column.date!="%d/%m/%Y") stop("Not Yet Implemented (ErrCod: #89h89h8h")
    # Cicla per ogni paziente
    # browser()
    for( paziente in seq(1,length(listToBeOrdered)) ) {
      # Estrai la matrice
      
      matrice.date<-listToBeOrdered[[paziente]]
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
  load.data.frame<-function( mydata, IDName, EVENTName, dateColumnName=NA) {
    # clear all the attributes
    clearAttributes( );
    
    # Add an internal ID attribute to myData (to uniquely identify Logs)
    if(!("pMineR.internal.ID.Evt" %in% colnames(mydata) ))
      { mydata <- cbind("pMineR.internal.ID.Evt"=seq(1,nrow(mydata)),mydata ) }
    
    # Just to have then an idea of the passed parameters...
    param.IDName<<-IDName
    param.EVENTName<<-EVENTName
    param.dateColumnName<<-dateColumnName
    
    # ok, let's begin!
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName    

    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])
    if(!is.na(dateColumnName)) {
      mydata[[dateColumnName]]<-as.character(mydata[[dateColumnName]])
    }
    cat("\n internal Grouping")
    # group the log of the patient in a structure easier to handle
    ID.act.group<-groupPatientLogActivity(mydata, ID.list.names) 
    
    cat("\n Ordering date:")
    # Order the list by the interested date (if exists)
    if(!is.na(dateColumnName)) {
      ID.act.group<-order.list.by.date(listToBeOrdered = ID.act.group, dateColumnName = dateColumnName)
    }
    cat("\n Building MMatrices and other stuff")
    # build the MM matrix and other stuff...
    res<-buildMMMatrices.and.other.structures(mydata = mydata, 
                                              EVENT.list.names = EVENT.list.names, 
                                              EVENTName = EVENTName, 
                                              ID.act.group = ID.act.group)
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
  load.csv<-function( nomeFile, IDName, EVENTName,  quote="\"",sep = ",", dateColumnName=NA) {
    # load the file
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    # Now "load" the data.frame
    load.data.frame( mydata = mydata, IDName = IDName, EVENTName = EVENTName, dateColumnName = dateColumnName )
  }
  #=================================================================================
  # load.listOfWords
  #=================================================================================  
  load.listOfSimpleWords<-function( load.listOfSimpleWords , IDName="ID1", EVENTName="Event") {
    cat("\n ==================\n NOT MORE SUPPORTED \n =====================")
    stop("Not more supported")
    clearAttributes( );
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName
    
    grossaMatrice<-c()
    for(i in seq(1,length(load.listOfSimpleWords))) {
      for( singEv in seq(1,length(load.listOfSimpleWords[[i]]))) {
        grossaMatrice<-rbind( grossaMatrice , c(  as.character(i), load.listOfSimpleWords[[i]][singEv] )  )
      }
    }
    colnames(grossaMatrice)<-c(IDName,EVENTName)
    
    aaa<-as.data.frame(grossaMatrice)
    aaa[[EVENTName]]<-as.character(aaa[[EVENTName]])
    aaa[[IDName]]<-as.character(aaa[[IDName]])
    
    # group the log of the patient in a structure easier to be handler
    ID.act.group<-groupPatientLogActivity(as.data.frame(aaa),IDName) 
    
    # build the MM matrix and other stuff...
    res<-buildMMMatrices.and.other.structures(mydata = aaa, 
                                              EVENT.list.names = EVENT.list.names, 
                                              EVENTName = EVENTName, 
                                              ID.act.group = ID.act.group)

    #populate the internal attributes
    arrayAssociativo<<-res$arrayAssociativo
    footPrint<<-res$footPrint
    MMatrix<<-res$MMatrix
    pat.process<<-res$pat.process
    wordSequence.raw<<-res$wordSequence.raw
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
  plot.Timeline<-function( patID ) {

   matrice <- cbind( pat.process[[ as.character(patID) ]][[param.dateColumnName]],
                         pat.process[[ as.character(patID) ]][[param.EVENTName]]) 
   plotTimeline(eventTable = matrice, format.date = "%d/%m/%Y")
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
    "load.listOfSimpleWords"=load.listOfSimpleWords,
    "getData"=getData,
    "removeEvents"=removeEvents,
    "keepOnlyEvents"=keepOnlyEvents,
    # "apply.filter"=apply.filter,
    "addDictionary"=addDictionary,
    "getTranslation"=getTranslation,
    "plot.Timeline"=plot.Timeline
  ))
}