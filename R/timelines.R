#' Plot a patient's timeline
#' 
#' @description  it plot a patient's timeline given an event log well formatted in the input eventTable
#' @param eventTable a table containing the event logs. The table has to have as columnName \code{c('DATA','EVENT')}.
#' @param output.format.date the format of the passed date. The default value is \code{' d / m / Y' }
#' @param cex.axis cex for timeline-text
#' @param cex.text cex for event-text 
#' @import graphics
#' @export
#' 
plotTimeline<-function( eventTable , table.format.date="%d/%m/%Y", output.format.date = "%d/%m/%Y" ,cex.axis = 0.6, cex.text = 0.7) {
  
  colnames(eventTable)<-c("DATA","DES");
  df<-as.data.frame(eventTable)
  
  
  df$DATA<-as.character.factor(df$DATA)
  df$DES<-as.character.factor(df$DES)
  
  df$YM <- as.Date(df$DATA, format=table.format.date)
  
  min.data <- df$DATA[1]
  max.data <- df$DATA[length(df$DATA)]
  
  delta.date <- as.numeric(difftime(as.POSIXct(max.data, format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(min.data, format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
  arr.delta.date <- as.numeric(difftime(as.POSIXct(df$DATA, format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(rep(min.data,length(df$DATA)), format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
  
  # browser()
  color.bar = "#5B7FA3";
  col.vert.ar <- "#5B7FA3"
  col.stanga <- "gray80"
  
  plot(NA,ylim=c(-1,1),xlim=c(0,delta.date),ann=FALSE,axes=FALSE)
  abline(h=0,lwd=2,col=color.bar)
  
  segments(arr.delta.date,rep(-.05,length(arr.delta.date)),arr.delta.date,rep(.05,length(arr.delta.date))  ,pch='8',col=col.vert.ar )
  
  ypts <- rep_len(c(-1,-0.7,-0.3,0.3,0.7,1), length.out=nrow(df))
  txtpts <- rep_len(c(1,3), length.out=nrow(df))
  
  
  for( indice in seq(1,length(df$DATA))) {
    # label <- as.POSIXct(max.data, format = table.format.date)
    label.data <- as.character(format(as.POSIXct(df$DATA[indice], format = table.format.date),format=output.format.date))
    label.evt <- df$DES[indice]
    label <- paste( c( label.evt,"\n",label.data), collapse = ''  )
    # text(x = arr.delta.date[indice], y = ypts[indice], labels = label.data  , cex = cex.text )
    # text(x = arr.delta.date[indice], y = ypts[indice]+(.2 * (1-cex.axis) ), labels = label.evt  , cex = cex.text )
    text(x = arr.delta.date[indice], y = ypts[indice], labels = label  , cex = cex.text )
    
    segments( arr.delta.date[indice], ypts[indice], arr.delta.date[indice],0,col=col.stanga)
  }
} 

old.plotTimeline<-function( eventTable , output.format.date = "%d/%m/%Y" ,cex.axis = 0.6, cex.text = 0.7) {

  colnames(eventTable)<-c("DATA","DES");
  df<-as.data.frame(eventTable)
  
  df$DATA<-as.character.factor(df$DATA)
  
  df$YM <- as.Date(df$DATA, format=output.format.date)
  
  rangeYM <- range(df$YM)
  
  plot(NA,ylim=c(-1,1),xlim=rangeYM,ann=FALSE,axes=FALSE)
  abline(h=0,lwd=2,col="#5B7FA3")
  
  ypts <- rep_len(c(-1,-0.7,0.3,0.3,0.7,1), length.out=nrow(df))
  txtpts <- rep_len(c(1,3), length.out=nrow(df))
  segments(df$YM,0,df$YM,ypts,col="gray80")
  # browser()
  axis.Date(
    1,
    at=seq.Date(rangeYM[1],rangeYM[2],by="months"),
    format=output.format.date,
    cex.axis=cex.axis,
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
    labels=paste(df$DES,df$DATA,sep="\n"), cex=cex.text, pos=txtpts
  )
  par(xpd=FALSE)
  
}

