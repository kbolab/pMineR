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

