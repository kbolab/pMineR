#' dataLoader
#' 
#' @description  It is a general loader for log files
#' @export
dataLoader<-function() {
  arrayAssociativo<-''
  footPrint<-''
  MMatrix<-''
  pat.process<-''   
  wordSequence.raw<-''
  #=================================================================================
  # buildFootPrintTable
  #=================================================================================   
  buildFootPrintTable<-function( MM ) {
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
    return(FF);
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
  # raggruppa i dati, come sono da CSV in una maniera più consona ad essere analizzati
  #=================================================================================   
  groupPatientLogActivity<-function(mydata,ID.list.names) {
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
    colnames(MM)<-c("BEGIN","END",unique(mydata[[EVENT.list.names]]))
    rownames(MM)<-colnames(MM)
    
    # ora scorri la storia dei singoli pazienti per estrarre le ricorrenze
    # per ogni paziente
    for(patID in seq(1,length(ID.act.group))) {
      # su ogni elemento del percorso clinico
      # t è il "tempo" in senso di "step"
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
                 "footPrint"=buildFootPrintTable(MM),
                 "footPrint.plus"=buildFootPrintTable.plus(MM = MM, wordsSeq = wordSequence.TMP01),
                 "MMatrix"=MM,
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
  #=================================================================================
  # loader
  #=================================================================================  
  load<-function( nomeFile, IDName, EVENTName,quote="\"",sep = ",") {
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName
    # carica il file
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])
    
    # group the log of the patient in a structure easier to be handler
    ID.act.group<-groupPatientLogActivity(mydata,ID.list.names) 
    
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
  }
  #=================================================================================
  # loader
  #=================================================================================  
  getData<-function() {
    # MMatrix.perc
    MM<-MMatrix;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
    MMatrix.perc<-MM
    
    # MMatrix.perc.noLoop
    MM<-MMatrix;
    diag(MM)<-0;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
    MMatrix.perc.noLoop<-MM     

    return(list(
      "arrayAssociativo"=arrayAssociativo,
      "footPrint"=footPrint,
      "MMatrix"=MMatrix,
      "pat.process"=pat.process,
      "MMatrix.perc"=MMatrix.perc,
      "MMatrix.perc.noLoop"=MMatrix.perc.noLoop,
      "wordSequence.raw"=wordSequence.raw
    ))
  }
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    arrayAssociativo<<-''
    footPrint<<-''
    MMatrix<<-''
    pat.process<<-'' 
    wordSequence.raw<<-''
  }
  costructor();
  #================================================================================= 
  return(list(
    "load"=load,
    "getData"=getData,
    "setData"=setData,
    "buildSplittedLoaderDataAndTables"=buildSplittedLoaderDataAndTables
  ))
}