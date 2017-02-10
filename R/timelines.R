#' Plot a patient's timeline
#' 
#' @description  it plot a patient's timeline given an event log well formatted in the input eventTable
#' @param eventTable a table containing the event logs. The table has to have as columnName \code{c('DATA','EVENT')}.
#' @param output.format.date the format of the passed date. The default value is \code{' d / m / Y' }
#' @param cex.axis cex for timeline-text
#' @param cex.text cex for event-text 
#' @import graphics
#' @export
plotTimeline<-function( eventTable , output.format.date = "%d/%m/%Y" ,cex.axis = 0.6, cex.text = 0.7) {

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
  
  axis.Date(
    1,
    at=seq.Date(rangeYM[1],rangeYM[2],by="month"),
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