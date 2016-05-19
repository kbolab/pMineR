#' class for parsing, checking and executing Petri Network
#' 
#' @description  bla bla bla bla
#'               ri bla bir bla bir bal
#' @export
#' @import XML 
petriNetworkModel<-function() {
  XML.model<-''
  loadModel.xml<-function( xmlModel ) {
    fa = newXMLNode("Main")
    parseXMLAndAdd(txt = xmlModel, parent = fa )
    XML.model<<-fa
  }
  replay<-function( arrayStatus , breakOn="error") {
    activeTokenList<-c("BEGIN")
    for( i in seq(1,length(arrayStatus))) {
      
      #cat(c("\n ------------------------------- \niteration ",i," TOKEN ",activeTokenList," before word ",arrayStatus[i]  ) )
      
      # Prendi la lista dei TOKEN teoricamente necessari all'attivazione
      stringaQuery<-paste( c('/Main/xml/graphStructure/links/link[@to="',arrayStatus[i],'"]'), collapse='')        
      queryRes<-xpathApply(XML.model,stringaQuery)
      if(length(queryRes)==0) { 
        return( list( "status"="error","msg"=paste(c(arrayStatus[i],' cannot be reached!'),collapse='')) )
      }
      # ora verifica che ognuno di questi sia presente, e consumalo
      # o dai errore se un token attesi per lo stato non è presente nella lista di quelli disponibili
      for( tokRunner in seq(1,length(queryRes))) {
        fromToken<-xmlGetAttr(queryRes[tokRunner][[1]],"from")
        if( ! (fromToken %in%  activeTokenList ))  { 
          return( list( "status"="error","msg"=paste(c(fromToken,' is missing but it needs to fire ',arrayStatus[i]),collapse='')) )
        }
        activeTokenList<- activeTokenList[which( !(activeTokenList==fromToken)  )]
      }
      
      # Soddisfatti i requisiti vedo allora di attivare i token a valle, ando errore
      # se lo stato non posizioni come conseguenti
      stringaQuery<-paste( c('/Main/xml/graphStructure/links/link[@from="',arrayStatus[i],'"]'), collapse='')
      queryRes<-xpathApply(XML.model,stringaQuery)
      if(length(queryRes)==0) {
        return( list( "status"="error","msg"=paste(c(' NO nodes reacheable from ',arrayStatus[i]),collapse='')) )
      }
      # scorrili tutti ed attiva i TOKEN
      for(indice in seq(1,length(queryRes))) {
        toToken<-xmlGetAttr(queryRes[indice][[1]],"to")
        # add the token if not present yet
        if(!(toToken %in% activeTokenList ))  activeTokenList<-c( activeTokenList , toToken)
      }
      
      # Ora fai una riflessione... è l'ultimo stato del log e non c'è il token 'END' come possibile
      # posizione? Se sì, allora fai un warning perchè la parola non è completa. Interrompi se è stato indicato 
      # di fermare dal "warning" in su.
      if(i == length(arrayStatus) & breakOn == "warning") {
        if(!("END" %in% activeTokenList ))  return( list( "status"="warning","msg"=paste(c('the log is not complete (last word is ',arrayStatus[i],'): no token in END position '),collapse='')) )
      }
      
      #cat(c("\niteration ",i," TOKEN ",activeTokenList," after word ",arrayStatus[i]  ) )
    }
    return( list( "status"="ok" , "msg"=paste(c("The word '",arrayStatus,"' fits with the PN model"),collapse='') ) )
  }
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    XML.model<<-''
  }
  costructor();
  #================================================================================= 
  return(list(
    "loadModel.xml"=loadModel.xml,
    "replay"=replay
  ))
}