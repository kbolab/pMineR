plotTimeline.ID.From.CSV<-function( nomeFile,  IDToPlot, IDName, EVENTName, DATAName = NA ) {
  # crea un oggetto 'loader'
  obj.L<-dataLoader()
  # di' al loader di caricare un file CSV usando le due colonne indicate per l'ID e l'EVENTO
  obj.L$load.csv(nomeFile = nomeFile,IDName = IDName,EVENTName = EVENTName)
  datiDaElaborare<-obj.L$getData()
  
  patientSubList <- datiDaElaborare$pat.process[[as.character(IDToPlot)]]

  matrice<-c()
  matrice<-cbind( matrice,  as.character(patientSubList[[DATAName]]), as.character(patientSubList[[EVENTName]])   )

  plotTimeline( eventTable = matrice )
}

plotTimeline<-function( eventTable , dataSep='/' ) {
  
  #   eventTable<-c("2001-01-10","RX torace")
  #   eventTable<-rbind(eventTable, c("2001-05-10","CT torace")   )
  #   eventTable<-rbind(eventTable,   c("2002-05-10","Dieta")   )
  colnames(eventTable)<-c("DATA","DES");
  df<-as.data.frame(eventTable)
  
  df$DATA<-as.character.factor(df$DATA)
  
  if ( dataSep == "-") df$YM <- as.Date(df$DATA, format="%Y-%m-%d")
  if ( dataSep == "/") df$YM <- as.Date(df$DATA, format="%Y/%m/%d")
  if ( dataSep == ":") df$YM <- as.Date(df$DATA, format="%Y:%m:%d")
  
  rangeYM <- range(df$YM)
  
  plot(NA,ylim=c(-1,1),xlim=rangeYM,ann=FALSE,axes=FALSE)
  abline(h=0,lwd=2,col="#5B7FA3")
  
  ypts <- rep_len(c(-1,-0.7,0.3,0.3,0.7,1), length.out=nrow(df))
  txtpts <- rep_len(c(1,3), length.out=nrow(df))
  segments(df$YM,0,df$YM,ypts,col="gray80")
  
  axis.Date(
    1,
    at=seq.Date(rangeYM[1],rangeYM[2],by="month"),
    format="%Y-%m",
    cex.axis=0.6,
    pos=0,
    lwd=0,
    lwd.tick=2,
    col="#5B7FA3",
    font=2
  )
  
  points(df$YM,y=ypts, pch="-", cex=1.5, col="#5B7FA3")
  par(xpd=NA)
  text(
    df$YM, y=ypts,
    labels=paste(df$DES,df$DATA,sep="\n"), cex=0.7, pos=txtpts
  )
  par(xpd=FALSE)
  
}