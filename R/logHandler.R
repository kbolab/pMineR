logHandler<-function() {
  behaviourTable<-c()
  #=================================================================================
  # sendLog
  # Send a Message Log according to the object policies for the type 
  # indicated in 'type'
  #=================================================================================
  sendLog<-function( msg , type="MSG" ) {
    if(length(msg)>1) messaggio<-paste(msg,collapse='')
    else messaggio<-msg 
    
    what2Do<-behaviourTable[which(behaviourTable[,"msg"]==type),"behaviour"]
    
    if(what2Do == "display")  {
      cat("\n",msg,":",messaggio)
    }
    if(what2Do == "stop")  {
      cat("\n",msg,":",messaggio)
      stop();
    }    
  }
  #=================================================================================
  # setBehaviour
  # change a single line in the behaviour table
  #=================================================================================
  setBehaviour<-function( msg , behaviour) {
    behaviourTable[which(behaviourTable[,"msg"]==msg),"behaviour"] <<- behaviour
  }
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function() {
    bht<-c()
    bht<-rbind(bht,c("WRN","display"))
    bht<-rbind(bht,c("MSG","display"))
    bht<-rbind(bht,c("ERR","display"))
    bht<-rbind(bht,c("NMI","stop"))
    colnames(bht)<-c("msg","behaviour")
    behaviourTable<<-bht
  }
  #=================================================================================
  costructor();
  #=================================================================================
  return(
    list(
      "sendLog"=sendLog,
      "setBehaviour"=setBehaviour
    )
  )
}
