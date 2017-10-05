#' an Pseudo-Object Factory
#'
#' @description  A pseudo Object Factory to keep track of objects and IDs of pMineR objects
#' @export
pMiner.register<-function() {
  obj.list<-list()
  
  register <- function( var.name ) {
    stringa.get.class<-paste(c(var.name,"$getClass()"),collapse = '')
    obj.list[[ var.name ]] <<- list()
    obj.list[[ var.name ]]$ID <<- eval(expr = parse(text = stringa.get.class))[[ "obj.ID" ]]
    obj.list[[ var.name ]]$Class <<- eval(expr = parse(text = stringa.get.class))[[ "class" ]]
  }
  getObjName <- function( ID ) {
    for( nome in names(obj.list) ) {
      if( ID == obj.list[[ nome ]]$ID ) return(nome)
      else return(NA)
    }
  }
  getObjList <- function() {
    return(obj.list)
  }
  #=================================================================================
  costructor<-function() {
    obj.list<<-list()
  }
  costructor()
  #=================================================================================
  return(list(
    "register"=register,
    "getObjList"=getObjList,
    "getObjName"=getObjName
  ))
}
