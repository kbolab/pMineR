#' A class born to cope with resources
#' @description  A class to manage resourcese
#' @param parameters.list list. a list containing the parameters. The possible ones are: 'considerAutoLoop' and 'threshold'. 'considerAutoLoop' is a boolean which indicates if the autoloops have to be admitted, while 'threshold' is the minimum value that a probability should have to do not be set to zero, in the transition matrix.
#' @param verbose.mode boolean. Verbose mode TRUE / FALSE
#' @export
resource_A01<-function( parameters.list = list(), verbose.mode =TRUE ) {
  
  dataStorage<-c()
  default.dateFormat <- c()
  obj.LH<-logHandler()
  data.dataLoader<-c()
  parameter<-c()
  is.trained<-NA
  param.verbose.mode<-TRUE
  cache.getTimeResourceAllocation<-list()
  global.personal.ID<-NA
  
  #===========================================================
  # loadDataset
  #===========================================================   
  loadDataset<-function( dataList , in.evt=c() , out.evt=c()) {
    
    if ( length(in.evt)==0 || length(out.evt)==0  )
    { obj.LH$sendLog( c("'in.evt' and 'out.evt' are not optionals!")  ,"ERR"); return() } 
    
    data.dataLoader<<-dataList;
    parameter$in.evt<<-in.evt
    parameter$out.evt<<-out.evt
  }
  #===========================================================
  # auto.Push.and.Pull
  #===========================================================     
  auto.Push.and.Pull<-function() {
    
    if(param.verbose.mode == TRUE) obj.LH$sendLog(" 1) Event auto push/pull (1/1):\n")
    pb <- txtProgressBar(min = 0, max = length(data.dataLoader$pat.process), style = 3)
    pb.ct <- 0
    
    # Per tutti i pazienti del dataLoader
    for( patID.index in seq(1,length(data.dataLoader$pat.process))) {
      
      patID <- names(data.dataLoader$pat.process)[patID.index]
      
      pb.ct <- pb.ct + 1; setTxtProgressBar(pb, pb.ct)
      
      aaa <- data.dataLoader$pat.process[[patID]]
      # estrai tutti gli eventi di INP/OUT dalla risorsa
      evt.push.pull <- aaa[ which( aaa[,data.dataLoader$csv.EVENTName] %in% parameter$out.evt | aaa[,data.dataLoader$csv.EVENTName] %in% parameter$in.evt), ]
      formato.data <- data.dataLoader$csv.date.format
      
      dentro <- FALSE;
      # Per ogni evento di IN/OUT
      if(nrow(evt.push.pull)>0) {
        for(riga in 1:nrow(evt.push.pull)) {
          # browser()
          # Se è di input (e non sei gia' dentro)  , entra
          if( evt.push.pull[ riga , data.dataLoader$csv.EVENTName ] %in%  parameter$in.evt  & dentro == FALSE ) {
            push(id = patID, dateValue = evt.push.pull[ riga , data.dataLoader$csv.dateColumnName ], dateFormat = formato.data)
            dentro<-TRUE;
          }
          # Se è di otput (e non sei gia' fuori)  , esci
          if( evt.push.pull[ riga , data.dataLoader$csv.EVENTName ] %in%  parameter$out.evt & dentro == TRUE ) {
            pull(id = patID, dateValue = evt.push.pull[ riga , data.dataLoader$csv.dateColumnName ], dateFormat = formato.data)
            dentro<-FALSE;
          }          
        }
      }
    }
    close(pb)
  }
  # =============================================
  # push
  #===========================================================  
  push<-function( id, dateValue, dateFormat=NA, attributesMatrix=c() ) {
    # E' stato specificato un dateFormat? 
    # (se no, error)
    if ( is.na(default.dateFormat) & is.na(dateFormat) )
    { obj.LH$sendLog( c("'default.dateFormat' and  'dateFormat' are not set! ")  ,"ERR"); return() } 
    
    if(is.trained==TRUE)
    { obj.LH$sendLog( c(" the model has already been trained! push and pull are not more admitted!")  ,"ERR"); return() } 
    
    if(is.na(dateFormat)) dateFormat <- default.dateFormat
    dateValue <- strptime(as.character(dateValue), dateFormat)
    dateValue <- format(dateValue,"%d/%m/%Y %H:%M:%S")
    
    numero.riga <- nrow(dataStorage$raw.events)
    if(is.null(numero.riga)) numero.riga <- 0
    # browser()
    dataStorage$raw.events <<- rbind(dataStorage$raw.events,  c(  numero.riga, id,dateValue,"i")  )
    dataStorage$events.attributes[[numero.riga]] <- attributesMatrix
    return();
  }
  #===========================================================
  # pull
  #===========================================================  
  pull<-function( id, dateValue, dateFormat=NA, attributesMatrix=c() ) {
    # E' stato specificato un dateFormat? 
    # (se no, error)
    if ( is.na(default.dateFormat) & is.na(dateFormat) )
    { obj.LH$sendLog( c("'default.dateFormat' and  'dateFormat' are not set! ")  ,"ERR"); return() }  
    
    if(is.trained==TRUE)
    { obj.LH$sendLog( c(" the model has already been trained! push and pull are not more admitted!")  ,"ERR"); return() } 
  
    if(is.na(dateFormat)) dateFormat <- default.dateFormat
    dateValue <- strptime(as.character(dateValue), dateFormat)
    dateValue <- format(dateValue,"%d/%m/%Y %H:%M:%S")
    
    numero.riga <- nrow(dataStorage$raw.events)
    if(is.null(numero.riga)) numero.riga <- 0
    dataStorage$raw.events <<- rbind(dataStorage$raw.events,  c(  numero.riga, id,dateValue,"o")  )
    return();
  }  
  #===========================================================
  # train
  #===========================================================  
  train<-function( ) {
    
    auto.Push.and.Pull()
    
    new.matrix <- dataStorage$raw.events
    
    # come data di riferimento, considera la prima!
    colnames(new.matrix) <- c("n.riga","id","data","IO")
    riferimento.data <- new.matrix[1,"data"]
    
    # array distanze temporali
    arr.delta.date <- as.numeric(difftime(as.POSIXct(new.matrix[, "data"], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(rep(riferimento.data,nrow(new.matrix)), format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
    new.matrix <- cbind(new.matrix, arr.delta.date)
    colnames(new.matrix) <- c("n.riga","id","data","IO","pMineR.delta.Date")
    # riordina la matrice per deltaDate
    new.matrix<-new.matrix[order(as.numeric(new.matrix[, "pMineR.delta.Date"])),]

    # Aggiungi lo stato della riga (input - output progressivi)
    stato.riga<-c()
    for( nriga in seq(1,nrow(new.matrix))) {
      stato.riga <- c(stato.riga,sum(new.matrix[ 1:nriga, "IO"]=="i")-sum(new.matrix[ 1:nriga, "IO"]=="o"))
    }
    new.matrix <- cbind(new.matrix, stato.riga)
    colnames(new.matrix) <- c("n.riga","id","data","IO","pMineR.delta.Date","pMineR.stato.riga")
    
    # Ricalcola i delta data, così da avere un riferimento rispetto alla prima data nel tempo!
    riferimento.data <- new.matrix[1,"data"]
    arr.delta.date <- as.numeric(difftime(as.POSIXct(new.matrix[, "data"], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(rep(riferimento.data,nrow(new.matrix)), format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
    new.matrix[,"pMineR.delta.Date"]<-arr.delta.date
    
    # registra il training come 'fatto'
    dataStorage$raw.events <<- new.matrix
    is.trained <<- TRUE;
  }    
  #===========================================================
  # getResourceAllocationAtTime
  #===========================================================   
  getResourceAllocationAtTime<-function( dateToCheck, UM, different.date.format = NA ) {
    
    if(is.na(different.date.format))  different.date.format<-"%Y-%m-%d %H:%M:%S"
    
    aa <- getTimeResourceAllocation( UM )
    
    dateToCheck<-strptime(dateToCheck, different.date.format)
    dateToCheck <- format(dateToCheck,"%Y-%m-%d %H:%M:%S")
    
    arr.delta.date <- as.numeric(difftime(as.POSIXct(aa[, "data"], format = "%Y-%m-%d %H:%M:%S"),
                                          as.POSIXct(rep(dateToCheck,nrow(aa)), format = "%Y-%m-%d %H:%M:%S"),
                                          units = 'mins'))
    posizione.minima <- min(which(arr.delta.date>0,arr.ind = T)-1)
    
    return(aa[posizione.minima,] )
  }
  #===========================================================
  # getTimeResourceAllocation
  #===========================================================    
  getTimeResourceAllocation<-function(  UM="mins" ) {
    
    cat("\n ATTENZIONE: Si tratta solo di un campionamento, al tempo di interesse! Procedura che sarebbe opportuno rivedere...")
    
    if(UM %in% names(cache.getTimeResourceAllocation)) {
      return(cache.getTimeResourceAllocation[[UM]])
    }
      
    if(UM!="mins" & UM!="days" & UM!="hours" & UM!="weeks" )
    { obj.LH$sendLog( c("UM must be 'days' or 'mins' or 'hours' or 'weeks' ")  ,"ERR"); return() }  
    
    new.matrix <- dataStorage$raw.events

    data.attuale <- new.matrix[1,"data"]
    if(UM=="mins")
      data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") - min(1)
    if(UM=="days")
      data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") - days(1)  
    if(UM=="hours")
      data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") - hours(1)       
    if(UM=="weeks")
      data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") - weeks(1) 

    # Calcola quanti eventi restano
    quanti.ne.restano<-nrow(new.matrix)
    matriciona<-c()
    id.pazienti.entrati<-0
    quanti.pazienti.entrati<-0
    pb.ct <- 0
    barra.settata <- FALSE
    
    # loop fino a che non hai finito gli eventi
    while(quanti.ne.restano > 0) {
      
      arr.da.fare <- which(as.numeric(difftime(
        as.POSIXct( new.matrix[,"data"], 
                    format = "%d/%m/%Y %H:%M:%S"),
        as.POSIXct( rep(data.attuale,nrow(new.matrix) ) ,
                    format = "%d/%m/%Y %H:%M:%S"),units = UM ) )  > 0)

      id.pazienti.da.entrare <- unique( new.matrix[ arr.da.fare ,"id"]  )
      id.pazienti.entrati <- seq(1,nrow(new.matrix)) [ !( seq(1,nrow(new.matrix)) %in% arr.da.fare ) ]

      quanti.sono.dentro.ora <- sum(new.matrix[id.pazienti.entrati,"IO"]=="i") - sum(new.matrix[id.pazienti.entrati,"IO"]=="o")
      
      id.pazienti.entrati <- unique(new.matrix[ id.pazienti.entrati ,"id"])
      
      eventi.da.fare <- length(arr.da.fare)
      eventi.fatti <- nrow(new.matrix) - eventi.da.fare
      quanti.pazienti.entrati <- length(id.pazienti.entrati)
      quanti.pazienti.da.entrare <- length(id.pazienti.da.entrare)

      quanti.ne.restano <- length(arr.da.fare)
      if(UM=="mins")
        data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") + min(1)
      if(UM=="days")
        data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") + days(1)  
      if(UM=="hours")
        data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") + hours(1)  
      if(UM=="weeks")
        data.attuale <- as.POSIXct(data.attuale,format="%d/%m/%Y %H:%M:%S") + weeks(1)     
      
      matriciona <- rbind(matriciona, c(  as.character(data.attuale), quanti.pazienti.entrati , 
                                          quanti.pazienti.da.entrare, eventi.fatti , eventi.da.fare ,
                                          quanti.sono.dentro.ora)   )
    }
    colnames(matriciona)<-c("data","id.done","id.to.do","evt.done","evt.to.do", "occupation")
    
    cache.getTimeResourceAllocation[[UM]]<<-matriciona

    return(matriciona)
  }    
  #===========================================================
  # plotTimeResourceAllocation
  #===========================================================    
  plotTimeResourceAllocation<-function(  UM, whatToPlot=c( "occupation" ), col=c("red","green","blue","orange","brown")  ) {  
    MM <- getTimeResourceAllocation( UM = UM )
    
    maxY<-0;
    for(cosa in whatToPlot) {
      maxY <- max(maxY,as.numeric(MM[ ,cosa]))
    }
      
    xlim <- c(0,nrow(MM))
    ylim <- c(0,maxY)
    
    plot(0,0,xlim=xlim, ylim=ylim, xlab = paste( c("time [",UM,"]"),collapse='' ), 
         main=paste(c("resource allocation"),collapse=''), ylab="number" )
    
    points( x=xlim, y=c(0,0),  type='l')

    for(i in seq(1,length(whatToPlot))) {
      cosa <- whatToPlot[ i ]
      points( x=seq(1,nrow(MM))  , y=as.numeric(MM[ ,cosa]),  type='l' , col = col[ i ])
    }
    
  }
  #===========================================================
  # plotOccupationProbability
  #===========================================================    
  plotOccupationProbability <- function( UM ) { 
    aa <- getTimeResourceAllocation( UM )
    hist(as.numeric(aa[,"occupation"] ),probability = T,main= paste(c("occupation probability (",UM,")") ,collapse='') ,xlab='patients')
    abline(v = mean(as.numeric(aa[,"occupation"] )),col="red")
  }
  #===========================================================
  # setDateDefaultFormat
  #===========================================================  
  setDateDefaultFormat<-function(  dateFormat ) {
    default.dateFormat<<-dateFormat
  }  
  #===========================================================
  # getAttribute
  #===========================================================  
  getAttribute<-function(  what ) {
    
    if(is.trained==FALSE)
    { obj.LH$sendLog( c(" You have to train the model before asking for some attributes... ")  ,"ERR"); return() } 
    
    if( what == "raw.events" ) return(dataStorage$raw.events);
    if( what == "events.attributes" ) return(dataStorage$events.attributes);
  }    
  getClass<-function(){
    return(list(
      "class"="resource_A01",
      "obj.ID"=global.personal.ID
    ))
  }  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = list(), verbose.mode ) {
    dataStorage<<-list()
    dataStorage$raw.events<<-c()
    dataStorage$events.attributes<-list()
    default.dateFormat<<-NA
    obj.LH<-logHandler()
    is.trained <<- FALSE;
    data.dataLoader<<-NA
    parameter<<-list()
    parameter$in.evt<<-c()
    parameter$out.evt<<-c()
    param.verbose.mode<<-verbose.mode
    cache.getTimeResourceAllocation<<-list()
    global.personal.ID<<-paste( c(as.character(runif(1,1,100000)),as.character(runif(1,1,100000)),as.character(runif(1,1,100000))), collapse = '' )
  }
  #===========================================================
  costructor( parametersFromInput = parameters.list , verbose.mode = verbose.mode);
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "getAttribute"=getAttribute,
    "train"=train,
    "getTimeResourceAllocation"=getTimeResourceAllocation,
    "getResourceAllocationAtTime"=getResourceAllocationAtTime,
    "plotTimeResourceAllocation"=plotTimeResourceAllocation,
    "plotOccupationProbability"=plotOccupationProbability,
    "getClass"=getClass
  ) )  
}


