#' Some useful tools
#' 
#' @description  A class which provide some tools. pMineR intarnal use only.
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
  
  format.data.for.csv<-function(listaProcessi, lista.validi) { 
    big.csv<-c()
    ct <- 1
    for(i in names(listaProcessi)) {
      numeroElementi<-length(listaProcessi[[i]])
      # matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],rep("01/01/1999",numeroElementi),rep(as.character(lista.validi[ct]),numeroElementi) )
      array.Date <- as.character(format(as.Date("01/01/2000",format="%d/%m/%Y") + seq(1,numeroElementi) ,format="%d/%m/%Y") )
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
    "format.data.for.csv" = format.data.for.csv
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
dataProcessor<-function() {
  
  #=================================================================================
  # buildMMMatrices.and.other.structures
  # costruisce la MM matrix ed anche altra robaccia
  #=================================================================================    
  buildMMMatrices.and.other.structures<-function(mydata, EVENT.list.names, EVENTName, EVENTDateColumnName=NA, ID.act.group) {

    # costruisci la matrice
    MM<-matrix(0, ncol=length(unique(mydata[[EVENT.list.names]]))+2, nrow=length(unique(mydata[[EVENT.list.names]]))+2 )
    colnames(MM)<-c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]])))
    rownames(MM)<-colnames(MM)
    # Creiamo anche la matrice con le density dei tempi di transizione
    # (ma solo se c'e' un campo DATA TIME)
    MM.den.list<-list()

    # ora scorri la storia dei singoli pazienti per estrarre le ricorrenze
    # per ogni paziente
    for(patID in seq(1,length(ID.act.group))) {
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
        # tutti gli altri
        if( t < nrow(ID.act.group[[patID]])) {
          
          nomeCampo.pre<-ID.act.group[[patID]][t,EVENT.list.names]
          nomeCampo.post<-ID.act.group[[patID]][t+1,EVENT.list.names]
          MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
          # if(param.dateColumnName!='' & ! is.na(param.dateColumnName)){
          if(EVENTDateColumnName!='' & ! is.na(EVENTDateColumnName)){
            delta.date<-as.numeric(difftime(as.POSIXct(ID.act.group[[patID]][t+1,EVENTDateColumnName], format = "%d/%m/%Y"),as.POSIXct(ID.act.group[[patID]][t,EVENTDateColumnName], format = "%d/%m/%Y"),units = 'days'))
            if(length(MM.den.list[[ nomeCampo.pre]])==0) MM.den.list[[ nomeCampo.pre]]<-list()
            if(length(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]])==0) MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c()
            MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]],delta.date)
          }
        }    
      }
    }
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
                 "pat.process"=ID.act.group,
                 "wordSequence.raw"=wordSequence.TMP01) )    
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
    "createSequenceMatrix" = createSequenceMatrix
  ))
}