#' dataLoader
#' 
#' @description  It is a general loader for log files
#' @export
dataLoader<-function() {
  arrayAssociativo<-''
  footPrint<-''
  MMatrix<-''
  pat.process<-''   
  #=================================================================================
  # buildFootPrintTable
  #=================================================================================   
  buildFootPrintTable<-function( MM) {
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
  # loader
  #=================================================================================  
  load<-function( nomeFile, IDName, EVENTName,quote="\"",sep = ",") {
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName
    # carica il file
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])
    
    # prendi la lista di pazienti e
    # per ogni paziente costruisci i gruppi 
    ID.list<-unique(mydata[[ID.list.names]])
    ID.act.group<-list();
    for(i in ID.list) {
      ID.act.group[[i]]<-mydata[ which(mydata[[ID.list.names]]==i  ), ]
    }
    
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
    # costruisci una tabella associativa in cui per ogni esame c'è un ID
    arrayAssociativo<<-rownames(MM);
    footPrint<<-buildFootPrintTable(MM)
    MMatrix<<-MM
    pat.process<<-ID.act.group
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
      "MMatrix.perc.noLoop"=MMatrix.perc.noLoop
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
  }
  costructor();
  #================================================================================= 
  return(list(
    "load"=load,
    "getData"=getData
  ))
}