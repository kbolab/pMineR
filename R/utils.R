#' Sfdfdsfdome useful tools
#' 
#' @description  A class which provide some tools. pMineR intarnal use only.ddd
#' @import Rcpp
#' @export
IsASCII<-function( fileName ) {
  return(c_IsASCII(fileName = fileName))
}

#' Some useful tools
#' 
#' @description  A class which provide some tools. pMineR intarnal use only.
#' @import Rcpp
#' @export
utils<-function() {
  dectobin <- function(y) {
    # find the binary sequence corresponding to the decimal number 'y'
    stopifnot(length(y) == 1, mode(y) == 'numeric')
    q1 <- (y / 2) %/% 1
    r <- y - q1 * 2
    res = c(r)
    while (q1 >= 1) {
      q2 <- (q1 / 2) %/% 1
      r <- q1 - q2 * 2
      q1 <- q2
      res = c(r, res)
    }
    return(res)
  } 
  is.included<-function( a , b ) {
    if(sum(is.element(a,b)) == length(a)) return(TRUE)
    else return(FALSE)
  }
  cleanUTF <- function( dati , colonna.evento , def.val.to.substitute = 95 ){
    # cat("clearing non UTF-8 characters...")
    # browser()
    for (riga in 1:nrow(dati)){
      arr <- as.numeric(charToRaw(x = as.character(dati[riga,colonna.evento]) ))
      arr[ which(arr > 127)]<-def.val.to.substitute 
      dati[riga,colonna.evento] <- intToUtf8(arr)
    }
    return(dati)
  }  
  
  format.data.for.csv<-function(listaProcessi, lista.validi, typeOfRandomDataGenerator="dayAfterDay",output.format.date = "%d/%m/%Y %H:%M:%S") { 
    big.csv<-c()
    ct <- 1
    
    
    for(i in names(listaProcessi)) {
      numeroElementi<-length(listaProcessi[[i]])
      
      if(typeOfRandomDataGenerator=="dayAfterDay") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=1))
      if(typeOfRandomDataGenerator=="randomDay1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) )
      if(typeOfRandomDataGenerator=="randomWeek1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) * 7)
      if(typeOfRandomDataGenerator=="randomMonth1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) * 30)
      
      # matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],rep("01/01/1999",numeroElementi),rep(as.character(lista.validi[ct]),numeroElementi) )
      # array.Date <- as.character(format(as.Date("01/01/2000",format="%d/%m/%Y") + seq(1,numeroElementi) ,format="%d/%m/%Y") )
      array.Date <- as.character(format(as.Date("01/01/2000 12:00:00",format=output.format.date) + cumsum(giorni.da.sommare) ,format=output.format.date) )
      matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],array.Date,rep(as.character(lista.validi[ct]),numeroElementi) )
      big.csv<-rbind(big.csv,matrice )
      ct <- ct + 1
    }
    # cat("\n",dim(big.csv))
    if(!is.null(dim(big.csv))) {
      # cat("\n DIM(big.csv)=",dim(big.csv))
      # if(dim(big.csv)[2]==1) browser()
      colnames(big.csv)<-c("patID","event","date","valido")
    }
    return(big.csv)
  }  
  old.format.data.for.csv<-function(listaProcessi, lista.validi, typeOfRandomDataGenerator="dayAfterDay") { 
    big.csv<-c()
    ct <- 1
    
    
    for(i in names(listaProcessi)) {
      numeroElementi<-length(listaProcessi[[i]])
      
      if(typeOfRandomDataGenerator=="dayAfterDay") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=1))
      if(typeOfRandomDataGenerator=="randomDay1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) )
      if(typeOfRandomDataGenerator=="randomWeek1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) * 7)
      if(typeOfRandomDataGenerator=="randomMonth1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) * 30)
      
      # matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],rep("01/01/1999",numeroElementi),rep(as.character(lista.validi[ct]),numeroElementi) )
      # array.Date <- as.character(format(as.Date("01/01/2000",format="%d/%m/%Y") + seq(1,numeroElementi) ,format="%d/%m/%Y") )
      array.Date <- as.character(format(as.Date("01/01/2000",format="%d/%m/%Y") + cumsum(giorni.da.sommare) ,format="%d/%m/%Y") )
      matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],array.Date,rep(as.character(lista.validi[ct]),numeroElementi) )
      big.csv<-rbind(big.csv,matrice )
      ct <- ct + 1
    }
    # cat("\n",dim(big.csv))
    if(!is.null(dim(big.csv))) {
      # cat("\n DIM(big.csv)=",dim(big.csv))
      # if(dim(big.csv)[2]==1) browser()
      colnames(big.csv)<-c("patID","event","date","valido")
    }
    return(big.csv)
  }
  return(list(
    "dectobin" = dectobin,
    "is.included" = is.included,
    "format.data.for.csv" = format.data.for.csv,
    "cleanUTF"=cleanUTF
  ))
}
textObj<-function() {
  testo<-'';
  add<-function( stringa, carriage=TRUE) {
    if(length(stringa)>1) stringa<-paste(stringa,collapse='')
    if(carriage==TRUE)
      testo <<- paste( c(testo,'\n',stringa), collapse = ''  ) 
    else
      testo <<- paste( c(testo,stringa), collapse = ''  ) 
  }
  get<-function() {
    return(testo)
  } 
  costructor<-function() {
    testo<<-'';
  }
  return(list("add"=add,"get"=get))
}
#' some data processing useful tools
#' 
#' @description  A class which provide some tools. pMineR intarnal use only.
#' @export
#' @useDynLib pMineR
dataProcessor<-function() {
  #=================================================================================
  # buildMMMatrices.and.other.structures
  # costruisce la MM matrix ed anche altra robaccia
  #=================================================================================    
  buildMMMatrices.and.other.structures<-function(mydata, EVENT.list.names, 
                                                 EVENTName, EVENTDateColumnName=NA, 
                                                 ID.act.group,
                                                 max.char.length.label = 50,
                                                 verbose.mode = TRUE) {
    
    # costruisci la matrice
    MM<-matrix(0, ncol=length(unique(mydata[[EVENT.list.names]]))+2, nrow=length(unique(mydata[[EVENT.list.names]]))+2 )
    colnames(MM)<-c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]])))
    rownames(MM)<-colnames(MM)
    # print(max.char.length.label)
    if(("" %in% trimws(colnames(MM))) == TRUE) {
      return( list("error"=TRUE, "errCode"=1)  )
    }
    
    if(max(nchar(colnames(MM)))>max.char.length.label)  {
      return( list("error"=TRUE, "errCode"=2)  )
    }
    if(length(grep("'", colnames(MM))))  {
      return( list("error"=TRUE, "errCode"=3)  )
    }    
    
    # Creiamo anche la matrice con le density dei tempi di transizione
    # (ma solo se c'e' un campo DATA TIME)
    MM.den.list<-list()
    MM.den.list.high.det<-list()
    
    # ora scorri la storia dei singoli pazienti per estrarre le ricorrenze
    # per ogni paziente
    if( verbose.mode == TRUE ) pb <- txtProgressBar(min = 0, max = length(ID.act.group), style = 3)
    for(patID in seq(1,length(ID.act.group))) {
      if( verbose.mode == TRUE ) setTxtProgressBar(pb, patID)
      # su ogni elemento del percorso clinico
      # t e' il "tempo" in senso di "step"
      for(t in seq(1,nrow(ID.act.group[[patID]]))) {
        # vedi se devi legare il BEGIN
        if( t == 1) {
          valore<-MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]
          MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]<-valore+1
        }
        # vedi se devi legare l'END   
        if( t == nrow(ID.act.group[[patID]])) {
          nomeCampo<-ID.act.group[[patID]][t,EVENT.list.names]
          MM[nomeCampo,"END"]<-MM[nomeCampo,"END"]+1
        }
        # browser()
        # tmp.uuu <- getInterestingSinglePatientData( ID.act.group[[patID]] );
        # cat("\n",tmp.uuu)
        # tutti gli altri
        if( t < nrow(ID.act.group[[patID]])) {
          nomeCampo.pre<-ID.act.group[[patID]][t,EVENT.list.names]
          nomeCampo.post<-ID.act.group[[patID]][t+1,EVENT.list.names]
          MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
          if(EVENTDateColumnName!='' & ! is.na(EVENTDateColumnName)){
            delta.date<-as.numeric(difftime(as.POSIXct(ID.act.group[[patID]][t+1,EVENTDateColumnName], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(ID.act.group[[patID]][t,EVENTDateColumnName], format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
            if(length(MM.den.list[[ nomeCampo.pre]])==0) MM.den.list[[ nomeCampo.pre]]<-list()
            if(length(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]])==0) MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c()
            MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]],delta.date)
          }
        }    
      }
      # invoca il programma in C per estrarre i tempi reciproci fra TUTTI
      iii <- unlist(lapply(ID.act.group[[patID]][,EVENT.list.names] , function(x) which(colnames(MM)==x) ))
      massimo <-max(iii)
      out.MM<-rep( 0 , (massimo)*(massimo) )
      out.delta<-c()
      nuovoOut <- c()
      
      aaa <- transitionsTime( iii , ID.act.group[[patID]][,"pMineR.deltaDate"], max(iii) );
      
      mm.in <- matrix(c(iii,ID.act.group[[patID]][,"pMineR.deltaDate"]),nrow=2,byrow = T)
      mm.out <- t(matrix(c(aaa$from,aaa$to,aaa$time),nrow=3,byrow = T))
      for( riga in seq(1,nrow(mm.out))) {
        int.from <-colnames(MM)[mm.out[riga,1]];
        int.to <-colnames(MM)[mm.out[riga,2]];
        delta.tempo <-mm.out[riga,3];
        if(length(MM.den.list.high.det[[ int.from ]])==0) MM.den.list.high.det[[ int.from]]<-list()
        if(length(MM.den.list.high.det[[ int.from]][[ int.to ]])==0) MM.den.list.high.det[[ int.from]][[ int.to ]]<-c()
        MM.den.list.high.det[[ int.from]][[ int.to ]]<-c(MM.den.list.high.det[[ int.from]][[ int.to ]],delta.tempo)
      }
    }
    if( verbose.mode == TRUE ) close(pb)
    quanti.da.fare<-length(names(MM.den.list)) * length(names(MM.den.list))
    
    # Calcola la matrice delle medie dei tempi
    # Sarebbe bello avere le density... vabbe'. piu' avanti
    if(EVENTDateColumnName!='' & !is.na(EVENTDateColumnName)){
      MM.mean.time<-MM
      MM.mean.time[ 1:nrow(MM.mean.time) , 1:ncol(MM.mean.time)   ]<-Inf
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          MM.mean.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]])
        }        
      }
    }
    
    # CALCOLO LA MATRICE DEI FLUSSI FUORI DALLO STATO
    
    if(EVENTDateColumnName!='' & !is.na(EVENTDateColumnName)){
      MM.mean.outflow.time<-MM
      MM.mean.outflow.time[ 1:nrow(MM.mean.outflow.time) , 1:ncol(MM.mean.outflow.time)   ]<-NA
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          MM.mean.outflow.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]][which(MM.den.list[[ state.from]][[ state.to ]] >=0 & state.from != state.to)])
        }
      }
    }
    
    # costruisci una semplice versione, con le parole (come piace tanto a Van der Aalst)
    wordSequence.TMP01<-list();
    for(i in seq(1,length(ID.act.group))) {
      IDPat<-names(  ID.act.group)[i]
      wordSequence.TMP01[[IDPat]]<-ID.act.group[[ IDPat ]][[EVENTName]]
    }    
    
    return(list( "arrayAssociativo" = rownames(MM),
                 "footPrint"="",
                 "MMatrix"=MM,
                 "MM.mean.time"=MM.mean.time,
                 "MM.density.list"=MM.den.list,
                 "MM.mean.outflow.time"=MM.mean.outflow.time,
                 "MM.den.list.high.det" = MM.den.list.high.det,
                 "pat.process"=ID.act.group,
                 "wordSequence.raw"=wordSequence.TMP01,
                 "error"=FALSE) )    
  }  
  
  #=================================================================================
  # buildMMMatrices.and.other.structures
  # costruisce la MM matrix ed anche altra robaccia
  #=================================================================================    
  buildMMMatrices.and.other.structures.v2<-function(mydata, EVENT.list.names, 
                                                 EVENTName, EVENTDateColumnName=NA, 
                                                 ID.act.group,
                                                 max.char.length.label = 50,
                                                 verbose.mode = TRUE) {
    # cat("\n 1")
    # costruisci la matrice
    separatoreNomi <- "|;|,|.|:|"
    MM<-matrix(0, ncol=length(unique(mydata[[EVENT.list.names]]))+2, nrow=length(unique(mydata[[EVENT.list.names]]))+2 )
    colnames(MM)<-c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]])))
    rownames(MM)<-colnames(MM)
    matrice.possibili.nomi <- expand.grid(c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]]))),c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]]))))
    ooo <- apply(matrice.possibili.nomi,MARGIN = 1,function(x){ paste(c(x[1],separatoreNomi,x[2]),collapse = '') } )
    # listaOccorrenzeTempi <- cbind(ooo,rep("",length(ooo)))
    listaOccorrenzeTempi<-rep("",length(ooo))
    names(listaOccorrenzeTempi) <- ooo
    
    # listaOccorrenzeTempi <- rep(list(c(1)),length(ooo))
    # names(listaOccorrenzeTempi) <- ooo;
    
    # print(max.char.length.label)
    if(("" %in% trimws(colnames(MM))) == TRUE) {
      return( list("error"=TRUE, "errCode"=1)  )
    }
    
    if(max(nchar(colnames(MM)))>max.char.length.label)  {
      return( list("error"=TRUE, "errCode"=2)  )
    }
    if(length(grep("'", colnames(MM))))  {
      return( list("error"=TRUE, "errCode"=3)  )
    }    
    MM.pulita <- MM
    nuova.MM <- MM
    
    # Creiamo anche la matrice con le density dei tempi di transizione
    # (ma solo se c'e' un campo DATA TIME)
    MM.den.list<-list()
    MM.den.list.high.det<-list()
    
    
    if(sum(is.na(mydata[[EVENTDateColumnName]] )) !=0 ) stop("ERROR: a date is set to a NA")
    if(EVENTDateColumnName=="") stop("ERROR: missing 'EVENTDateColumnName'")
    
    posizione.colonna.evento <- which(colnames(ID.act.group[[1]]) == EVENTName) -1
    posizione.colonna.data.inizio.evento <- which(colnames(ID.act.group[[1]]) == EVENTDateColumnName) -1
    # cat("\n 2")
    c_buildMMMatrices_and_other_structures_v2( 
      ID.act.group, 
      nuova.MM, 
      posizione.colonna.evento,
      (which(colnames(nuova.MM)=="BEGIN")-1),
      (which(colnames(nuova.MM)=="END")-1),
      posizione.colonna.data.inizio.evento,
      rownames(nuova.MM),
      separatoreNomi,
      listaOccorrenzeTempi,
      names(listaOccorrenzeTempi)) 
    # Sai tu perche', mi ha sporcato MM: devo ripulirla
    # (forse perche' globale?? Bo'..)
    MM <- MM * 0
    
    # Ora, a partire dal contenuto della 'listaOccorrenzeTempi', devi costruire la 'MM.den.list'
    Matrice.occorrenze.tempi <- cbind(names(listaOccorrenzeTempi),listaOccorrenzeTempi)
    tmp.MM.den.list <- list()
      
    oppalo <- apply(X = Matrice.occorrenze.tempi, MARGIN = 1,
                     FUN = function(x) {
                              stati<-strsplit( x = x[1],split = separatoreNomi,fixed = TRUE)[[1]]
                              valori.esplosi <- strsplit( x = x[2],split = ",",fixed = TRUE)[[1]]
                              valori.esplosi <- as.numeric(valori.esplosi[ which(!(valori.esplosi %in% c(""))) ])
                              valori.esplosi <- valori.esplosi[ which(!is.na(valori.esplosi)) ]
                              if(length(tmp.MM.den.list[[ stati[1] ]])==0) tmp.MM.den.list[[ stati[1] ]]<<-list()
                              if(length(tmp.MM.den.list[[ stati[1] ]][[ stati[2] ]])==0) tmp.MM.den.list[[ stati[1] ]][[ stati[2] ]]<<-as.numeric(valori.esplosi)
                           }
                    )
    MM.den.list <- tmp.MM.den.list
    # cat("\n 3")
    # ora scorri la storia dei singoli pazienti per estrarre le ricorrenze
    # per ogni paziente
    # if( verbose.mode == TRUE ) pb <- txtProgressBar(min = 0, max = length(ID.act.group), style = 3)
    for(patID in seq(1,length(ID.act.group))) {
      # if( verbose.mode == TRUE ) setTxtProgressBar(pb, patID)
      # su ogni elemento del percorso clinico
      # t e' il "tempo" in senso di "step"
      # for(t in seq(1,nrow(ID.act.group[[patID]]))) {
      #   # vedi se devi legare il BEGIN
      #   if( t == 1) {
      #     valore<-MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]
      #     MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]<-valore+1
      #   }
      #   # vedi se devi legare l'END   
      #   if( t == nrow(ID.act.group[[patID]])) {
      #     nomeCampo<-ID.act.group[[patID]][t,EVENT.list.names]
      #     MM[nomeCampo,"END"]<-MM[nomeCampo,"END"]+1
      #   }
      #   # browser()
      #   # tmp.uuu <- getInterestingSinglePatientData( ID.act.group[[patID]] );
      #   # cat("\n",tmp.uuu)
      #   # tutti gli altri
      #   if( t < nrow(ID.act.group[[patID]])) {
      #     nomeCampo.pre<-ID.act.group[[patID]][t,EVENT.list.names]
      #     nomeCampo.post<-ID.act.group[[patID]][t+1,EVENT.list.names]
      #     MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
      #     if(EVENTDateColumnName!='' & ! is.na(EVENTDateColumnName)){
      #       delta.date<-as.numeric(difftime(as.POSIXct(ID.act.group[[patID]][t+1,EVENTDateColumnName], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(ID.act.group[[patID]][t,EVENTDateColumnName], format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
      #       if(length(MM.den.list[[ nomeCampo.pre]])==0) MM.den.list[[ nomeCampo.pre]]<-list()
      #       if(length(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]])==0) MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c()
      #       MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]],delta.date)
      #     }
      #   }    
      # }
      # invoca il programma in C per estrarre i tempi reciproci fra TUTTI
      iii <- unlist(lapply(ID.act.group[[patID]][,EVENT.list.names] , function(x) which(colnames(MM)==x) ))
      massimo <-max(iii)
      out.MM<-rep( 0 , (massimo)*(massimo) )
      out.delta<-c()
      nuovoOut <- c()
      
      aaa <- transitionsTime( iii , ID.act.group[[patID]][,"pMineR.deltaDate"], max(iii) );
      
      mm.in <- matrix(c(iii,ID.act.group[[patID]][,"pMineR.deltaDate"]),nrow=2,byrow = T)
      mm.out <- t(matrix(c(aaa$from,aaa$to,aaa$time),nrow=3,byrow = T))
      for( riga in seq(1,nrow(mm.out))) {
        int.from <-colnames(MM)[mm.out[riga,1]];
        int.to <-colnames(MM)[mm.out[riga,2]];
        delta.tempo <-mm.out[riga,3];
        if(length(MM.den.list.high.det[[ int.from ]])==0) MM.den.list.high.det[[ int.from]]<-list()
        if(length(MM.den.list.high.det[[ int.from]][[ int.to ]])==0) MM.den.list.high.det[[ int.from]][[ int.to ]]<-c()
        MM.den.list.high.det[[ int.from]][[ int.to ]]<-c(MM.den.list.high.det[[ int.from]][[ int.to ]],delta.tempo)
      }
    }
    # if( verbose.mode == TRUE ) close(pb)
    quanti.da.fare<-length(names(MM.den.list)) * length(names(MM.den.list))
    
    # cat("\n 4")
    # browser()
    # Calcola la matrice delle medie dei tempi
    # Sarebbe bello avere le density... vabbe'. piu' avanti
    if(EVENTDateColumnName!='' & !is.na(EVENTDateColumnName)){
      MM.mean.time<-MM
      MM.mean.time[ 1:nrow(MM.mean.time) , 1:ncol(MM.mean.time)   ]<-Inf
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          MM.mean.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]])
        }        
      }
    }
    
    # CALCOLO LA MATRICE DEI FLUSSI FUORI DALLO STATO
    
    if(EVENTDateColumnName!='' & !is.na(EVENTDateColumnName)){
      MM.mean.outflow.time<-MM
      MM.mean.outflow.time[ 1:nrow(MM.mean.outflow.time) , 1:ncol(MM.mean.outflow.time)   ]<-NA
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          MM.mean.outflow.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]][which(MM.den.list[[ state.from]][[ state.to ]] >=0 & state.from != state.to)])
        }
      }
    }
    
    # costruisci una semplice versione, con le parole (come piace tanto a Van der Aalst)
    wordSequence.TMP01<-list();
    for(i in seq(1,length(ID.act.group))) {
      IDPat<-names(  ID.act.group)[i]
      wordSequence.TMP01[[IDPat]]<-ID.act.group[[ IDPat ]][[EVENTName]]
    }    
    
    # browser()
    cat("\n 5")
    return(list( "arrayAssociativo" = rownames(MM),
                 "footPrint"="",
                 "MMatrix"=MM,
                 "MM.mean.time"=MM.mean.time,
                 "MM.density.list"=MM.den.list,
                 "MM.mean.outflow.time"=MM.mean.outflow.time,
                 "MM.den.list.high.det" = MM.den.list.high.det,
                 "pat.process"=ID.act.group,
                 "wordSequence.raw"=wordSequence.TMP01,
                 "error"=FALSE) )    
  }
  
  #=================================================================================
  # createSequenceMatrix
  # crea una matrice di transizione a partire da una mera sequenza di eventi.
  # Creata per poter evitare di dover usare il pacchetto markovChain
  #=================================================================================      
  createSequenceMatrix<-function( sequence2parse ) {
    
    sequenza.simboli <- unique(as.character(sequence2parse))
    MM<-matrix(0, ncol=length(sequenza.simboli), nrow=length(sequenza.simboli) )  
    colnames(MM)<-sequenza.simboli
    rownames(MM)<-sequenza.simboli
    
    # cicla su ogni elemento della sequenza ed incrementa la relativa posizione nella 
    # matrice di transizione from=>to
    for(t in seq(1,length(sequence2parse)-1)) {
      # tutti gli altri
      nomeCampo.pre<-sequence2parse[t]
      nomeCampo.post<-sequence2parse[t+1]
      MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
    }
    return(list(
      "transitionCountMatrix" = MM
    ))
  }
  
  return(list(
    "buildMMMatrices.and.other.structures"=buildMMMatrices.and.other.structures,
    "buildMMMatrices.and.other.structures.v2"=buildMMMatrices.and.other.structures.v2,
    "createSequenceMatrix" = createSequenceMatrix
  ))
}

# -----------------------------------------------------------------------
# funzione plotPatientReplayedTimelineFunction
# -----------------------------------------------------------------------
#' Some useful tools new version
#' 
#' @description  A class which provide some tools. pMineR intarnal use only. wow
#' @export
plotPatientReplayedTimelineFunction<-function( list.computation.matrix , patientID,
                                               text.cex=.7, y.intra.gap = 40, x.offset = 100,
                                               thickness=5 , 
                                               bar.border = "Navy",bar.volume = "lightsteelblue1",
                                               text.date.cex =.6) {
  
  date.notevoli <-c()
  durate.notevoli <- c()
  matrice <- list.computation.matrix$list.computation.matrix$stati.timeline[[patientID]]
  tempo.max <- max(  as.numeric(matrice[,4])  )
  numero.stati <- length(unique(matrice[,1]))
  arr.stati <- c()
  for( tmp in 1:length(matrice[,1])) {
    if( !(matrice[tmp,1] %in%arr.stati)) { arr.stati <- c(arr.stati,matrice[tmp,1]) }
  }
  # browser()
  par(mar=c(2,0,2,0)+0)
  plot( x=c(), y=c(), 
        xlim = c(0,tempo.max + x.offset+ 15) , 
        ylim=c(0,(numero.stati+1)*y.intra.gap ), 
        bty='n',axes = FALSE, xlab='', ylab='' )
  
  lista.boxes<- list()
  lista.points<- list()
  lista.date<-list()
  
  for( index in seq(1,length(arr.stati) )) {
    ypos.line <- (numero.stati+1)*y.intra.gap - index * y.intra.gap
    stato <- arr.stati[ index ]
    text(x = 0,y = ypos.line,labels = stato, cex = text.cex, pos = 4)
    
    # lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset,tempo.max+x.offset), "y"=c( ypos.line,ypos.line ))
    
    sub.matrice <- matrice[ which(matrice[ ,1]==stato )  ,]
    numero.righe.sub.matrice <- length(sub.matrice)/4
    # Se è almeno una matrice (se ho almeno due rilevazioni)
    if(numero.righe.sub.matrice>1) {
      l.from <- NA
      l.to <- NA
      for( i in seq(1,numero.righe.sub.matrice )) {
        if(sub.matrice[i,2]=="begin") { 
          l.from <- as.numeric(sub.matrice[i,4]) 
          durate.notevoli <- c(durate.notevoli, l.from )
          date.notevoli <- c(date.notevoli, sub.matrice[i,3] )
          lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset + l.from ,x.offset + l.from), "y"=c( -5, (numero.stati+1)*y.intra.gap +5),"label.data"=sub.matrice[i,3],"label.durata"=sub.matrice[i,4])          
        }
        if(sub.matrice[i,2]=="end") {
          l.to <- as.numeric(sub.matrice[i,4] )
          lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset + l.to ,x.offset + l.to), "y"=c( -5, (numero.stati+1)*y.intra.gap +5),"label.data"=sub.matrice[i,3],"label.durata"=sub.matrice[i,4])          
          lista.boxes[[length(lista.boxes)+1]]<-list( "x"=c( l.from ,l.to, l.to, l.from, l.from ) + x.offset, "y"=c( -thickness, -thickness, thickness, thickness , -thickness)+ypos.line )
          durate.notevoli <- c(durate.notevoli, l.to )
          date.notevoli <- c(date.notevoli, sub.matrice[i,3]  )
        }
      }
    }
    # Se c'è solo una riga!
    if(numero.righe.sub.matrice==1) {
      l.pos <- as.numeric(sub.matrice[4] )
      durate.notevoli <- c(durate.notevoli, l.pos )
      date.notevoli <- c(date.notevoli, sub.matrice[3]  )   
      lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset + l.pos ,x.offset + l.pos), "y"=c( -5, (numero.stati+1)*y.intra.gap +5),"label.data"=sub.matrice[3], "label.durata"=sub.matrice[4])
      
      # Se è un END 
      if(sub.matrice[2]=="end" |  as.numeric(sub.matrice[4])==tempo.max ) {
        lista.points[[length(lista.points)+1]]<-list("x"=l.pos + x.offset,"y"=ypos.line)
      }
      # Se è un BEGIN
      if(sub.matrice[2]=="begin" & as.numeric(sub.matrice[4])!=tempo.max) {
        l.from <- l.pos
        l.to <- tempo.max
        lista.boxes[[length(lista.boxes)+1]]<-list( "x"=c( l.from ,l.to, l.to, l.from, l.from ) + x.offset, "y"=c( -thickness, -thickness, thickness, thickness , -thickness)+ypos.line )
      }
    }    
    
  }
  
  # plotta le verticali delle date
  number <- 1
  old.x <- c()
  for(i in seq(1, length(lista.date))) {
    if(! (lista.date[[i]]$x[1]  %in% old.x) ) {
      number <- number + 1 
      points(x =lista.date[[i]]$x, y = lista.date[[i]]$y , type='l', col="grey", lty = 4 )
      text(x = lista.date[[i]]$x , y = lista.date[[i]]$y[1] + (number * 10)-5, labels = str_replace_all(string = lista.date[[i]]$label.data,pattern = " ",replacement = "\n"), cex = text.date.cex, col='black')
      text(x = lista.date[[i]]$x , y = (numero.stati+1)*y.intra.gap + (number * 10) -25, labels = as.integer(as.numeric(lista.date[[i]]$label.durata)), cex = text.date.cex, col='black')
      if(number >= 3) number <- 0
      old.x <- c(old.x, lista.date[[i]]$x[1] )
    }
  }
  # plotta gli assi degli stati
  for( index in seq(1,length(arr.stati) )) {
    points( x = c(x.offset,x.offset+tempo.max), 
            y = c( (numero.stati+1)*y.intra.gap - index * y.intra.gap, (numero.stati+1)*y.intra.gap - index * y.intra.gap),
            type='l' , col= "grey")     
  }
  # plotta i GANTT
  for(i in seq(1, length(lista.points))) {
    points( x = lista.points[[i]]$x, 
            y = lista.points[[i]]$y,
            pch=13 , col= bar.border)     
    # points(x =lista.date[[i]]$x, y = lista.date[[i]]$y , type='l', col="grey", lty = 4 )
  }  
  for(i in seq(1, length(lista.boxes))) {
    points( x = lista.boxes[[i]]$x, 
            y = lista.boxes[[i]]$y,
            type='l' , col= bar.border)
    polygon( x = lista.boxes[[i]]$x, 
             y = lista.boxes[[i]]$y,
             col= bar.volume) 
  }    
  
  # list.computation.matrix
} 

plot.cc.KM <- function( KM.list , cols=c("red","darkgreen","blue","brown","orange"), 
                        plotCI = TRUE, main = "Kaplan-meier curves", lwd = 2) { 
  
  # Prendi il tempo massimo (per l'xlim del grafico)
  max.tempi <- 0
  for(i in 1:length(KM.list)) { max.tempi <- max(max.tempi, summary(KM.list[[i]])$time ) }
  
  # Fai un plot a vuoti, delle dimensioni giuste
  plot(NULL,xlim=c(0,max.tempi),ylim=c(0,1) , xlab="time",ylab="prob.", main = main)
  
  # Scorri tutti le KM
  for(i in 1:length(KM.list)) { 
    
    # Prendi i summary (le liste con i risultati)
    ooo <- summary(KM.list[[i]])
    x.p <- ooo$time; y.p <- ooo$surv; y.p.upper <- ooo$upper; y.p.lower <- ooo$lower;
    x.p <- c(0,x.p); y.p <- c(1,y.p); y.p.upper <- c(1,y.p.upper); y.p.lower <- c(1,y.p.lower)
    
    # punto per punti plotta la KM
    for( punti in 1:length(x.p) ) {
      # i punti della KM
      points( x=c( x.p[punti],x.p[punti+1]  ), y=c( y.p[punti],y.p[punti] ) , type='l' , col = cols[i] , lwd = lwd  )
      points( x=c( x.p[punti+1],x.p[punti+1]  ), y=c( y.p[punti],y.p[punti+1] ) , type='l', col = cols[i] , lwd = lwd )
      
      if( plotCI == TRUE ) { 
        # Intervallo di confidenza upper
        points( x=c( x.p[punti],x.p[punti+1]  ), y=c( y.p.upper[punti],y.p.upper[punti] ) , type='l' , col = cols[i] , lty = 4  )
        points( x=c( x.p[punti+1],x.p[punti+1]  ), y=c( y.p.upper[punti],y.p.upper[punti+1] ) , type='l', col = cols[i] , lty = 4 )
        # Intervallo di confidenza lower
        points( x=c( x.p[punti],x.p[punti+1]  ), y=c( y.p.lower[punti],y.p.lower[punti] ) , type='l' , col = cols[i] , lty = 4  )
        points( x=c( x.p[punti+1],x.p[punti+1]  ), y=c( y.p.lower[punti],y.p.lower[punti+1] ) , type='l', col = cols[i] , lty = 4 )
      }
    }
  }
  
}

