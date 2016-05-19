#' class for handling logs/warnings/errorss
#' 
#' @description  It handles messages from script to a chosen output (screen, file, etc.)
#' @export
logHandler<-function() {
  #=================================================================================
  # sendLog
  # Send a Message Log according to the object policies for the type 
  # indicated in 'type'
  #=================================================================================
  sendLog<-function( msg , type="MSG" ) {
    if(length(msg)>1) messaggio<-paste(msg,collapse='')
    else messaggio<-msg
    cat("\n",messaggio)
  }
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function() {
  }
  #=================================================================================
  costructor();
  #=================================================================================
  return(
    list(
      "sendLog"=sendLog,
      "handle"=handle
    )
  )
}
