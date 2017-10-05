#' an Pseudo-Object Factory
#'
#' @description  A pseudo Object Factory to keep track of objects and IDs of pMineR objects
#'              \itemize{
#'              \item \code{register( ... ) } Registers a pMineR object
#'              \item \code{getObjName( ... ) } Returns the name of the pMineR object with the indicated ID
#'              \item \code{getObjList() } Returns the list of the stored objs information
#'              } 
#' @export
pMiner.register<-function() {
  # Attributo che contiene le informazioni degli oggetti registrati
  obj.list<-list()

  #=================================================================================
  # register
  # Metodo per registrare un oggetto. 
  # Prende in ingresso IL NOME DELLA VARIABILE (in FORMATO STRINGA)
  #=================================================================================  
  register <- function( var.name ) {
    stringa.get.class<-paste(c(var.name,"$getClass()"),collapse = '')
    obj.list[[ var.name ]] <<- list()
    obj.list[[ var.name ]]$ID <<- eval(expr = parse(text = stringa.get.class))[[ "obj.ID" ]]
    obj.list[[ var.name ]]$Class <<- eval(expr = parse(text = stringa.get.class))[[ "class" ]]
  }
  #=================================================================================
  # getObjName
  # Restituisce il nome della variabile il cui oggetto ha l'ID indicato
  #=================================================================================    
  getObjName <- function( ID ) {
    for( nome in names(obj.list) ) {
      if( ID == obj.list[[ nome ]]$ID ) return(nome)
      else return(NA)
    }
  }
  #=================================================================================
  # getObjList
  # Restituisce la lista interna contenente gli oggetti che sono stati registrati
  #=================================================================================      
  getObjList <- function() {
    return(obj.list)
  }
  #=================================================================================
  # COSTRUTTORE
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
