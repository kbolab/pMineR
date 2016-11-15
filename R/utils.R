#' some useful tools
#' 
#' @description  A class which provide some utility tools
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
      matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],rep("01/01/1999",numeroElementi),rep(as.character(lista.validi[ct]),numeroElementi) )
      big.csv<-rbind(big.csv,matrice )
      ct <- ct + 1
    }
    colnames(big.csv)<-c("patID","event","date","valido")
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
