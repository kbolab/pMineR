#' Load the event-logs serializing many dataLoaders objs
#' 
#' @description  This class is  more abstract than dataLoader and provide some facilities, in particular to cope with dictionaries and tranlations. Because it handles, internally, a set of dataLoader objects, any dataLoader object must be referred by 'view'.
#'              \itemize{
#'              \item \code{meta.dataLoader() } the costructor
#'              \item \code{load.csv( ... ) } loads the csv file into the \code{dataLoader} object
#'              \item \code{load.data.frame() } loads a data.frame into the \code{dataLoader} object
#'              \item \code{getData() } return the processed, previously-loaded, data
#'              \item \code{removeEvents() } remove the events in the array passed as argument (dual of \code{dataLoader::keepOnlyEvents()} )
#'              \item \code{keepOnlyEvents() } keep only the events in the array passed as argument (dual of \code{dataLoader::removeEvents()} )
#'              \item \code{addDictionary() } add a dictionary in order, afterward, to translate or group some event name
#'              \item \code{getTranslation() } perform a translation applying the given dictionary to the loaded csv or data.frame
#'              \item \code{plot.Timeline() } plot the timeline of the events regarding a single patient 
#'              } 
#'              In order to better undestand the use of such methods, please visit: www.pminer.info
#'              
#'              The consturctor admit the following parameters:
#' verbose.mode are some notification wished, during the computation? The defaul value is \code{true}
#' @param verbose.mode boolean. If TRUE some messages will appear in console, during the computation; otherwise the computation will be silent.
#' @import stringr utils stats           
#' @export
#' @examples \dontrun{
#' 
#' # create a Loader
#' obj.L<-meta.dataLoader();   
#'
#' # create a view
#' obj.L$createView(view.name = "mammella")
#' obj.L$createView(view.name = "retto")
#' 
#' # Load a .csv into the view 'mammella'
#' obj.L$load.csv(nomeFile = "../otherFiles/mammella.csv",
#' IDName = "CODICE_SANITARIO_ADT",
#' EVENTName = "DESC_REPARTO_RICOVERO",
#' dateColumnName = "DATA_RICOVERO", view="mammella")
#' 
#' # Load a .csv into the view 'retto'
#' obj.L$load.csv(nomeFile = "../otherFiles/mammella.csv",
#' IDName = "CODICE_SANITARIO_ADT",
#' EVENTName = "DESC_REPARTO_RICOVERO",
#' dateColumnName = "DATA_RICOVERO", view="retto")
#'
#' # get the data from the view 'retto'
#' aaa <- obj.L$getData(view = "retto")
#'
#' 
#' }
meta.dataLoader<-function( verbose.mode = TRUE ) {
  list.dataLoader <-list()
  param.verbose<-''
  obj.logHandler<-c()
  #=================================================================================
  # clearAttributes
  # this method clear all the attributes in order to make the object re-useable
  # for other issues ( dirty variables could have dramatic effetcs! )  
  #=================================================================================    
  clearAttributes<-function() {
    costructor();
  }
  #=================================================================================
  # load.csv
  # a method to load CSV files. This is a wrapper of the same method of the 
  # dataLoader class
  #=================================================================================  
  load.csv<-function( nomeFile, IDName, EVENTName,  quote="\"",sep = ",", dateColumnName=NA, view = "main") {
    # if the view does not exist, error
    if( !(view %in% names(list.dataLoader) ) )
      { obj.logHandler$sendLog( msg =c("ERROR: '",view,"' does not exist"), type="NMI" ); return; }
    # ok, load data
    list.dataLoader[[ view ]]$load.csv( nomeFile = nomeFile, IDName = IDName, EVENTName = EVENTName, quote = quote, sep = sep, dateColumnName = dateColumnName)
  }
  #=================================================================================
  # load.data.frame
  # a method to load data.frame. This is a wrapper of the same method of the 
  # dataLoader class  
  #=================================================================================  
  load.data.frame<-function( mydata, IDName, EVENTName, dateColumnName=NA, view = "main") {
    # if the view does not exist, error
    if( !(view %in% names(list.dataLoader) ) )
      { obj.logHandler$sendLog( msg =c("ERROR: '",view,"' does not exist"), type="NMI" ); return; }
    # ok, load data
    list.dataLoader[[ view ]]$load.data.frame(  mydata = mydata, IDName = IDName, EVENTName = EVENTName, dateColumnName = dateColumnName)
  }  
  #=================================================================================
  # getData
  # applies the ::getData() methods to the chosen view.
  #=================================================================================  
  getData<-function( view = "main") {
    return(  list.dataLoader[[ view ]]$getData()  )
  }
  #=================================================================================
  # copyView
  # it makes a copy of a view
  #=================================================================================  
  copyView<-function( view.name , from.view = "main" ) {
    # 'main' cannot be used :)
    if( view.name == "main") 
      { obj.logHandler$sendLog( msg ="ERROR: 'main' is not a reserved name ", type="NMI" ); return; }
    # An existent name cannot be used
    # An un-existent name is passed as source
    if( !(from.view %in% names(list.dataLoader) ) )
    { obj.logHandler$sendLog( msg =c("ERROR: '",from.view,"' does not exist"), type="NMI" ); return; }
    
    list.dataLoader[[ view.name ]] <<- list.dataLoader[[ from.view ]] 
  }  
  #=================================================================================
  # createView
  # creates a new, empty, view: if an existent one is specified, the one is overridden
  #=================================================================================  
  createView<-function( view.name , verbose.mode = TRUE) {
    # 'main' cannot be used :)
    if( view.name == "main") 
    { obj.logHandler$sendLog( msg ="ERROR: 'main' is not a reserved name ", type="NMI" ); return; }
    # create the view
    list.dataLoader[[ view.name ]] <<- dataLoader( verbose.mode = verbose.mode)
  }   
  #=================================================================================
  # removeEvents
  # applies the 'removeEvents' method of the dataLoader class, applied to the chosen view
  #=================================================================================    
  removeEvents<-function( array.events=NA, view='main') {  
    # Check if it exists
    if(  !(view %in% names(list.dataLoader) ) )   {
      { obj.logHandler$sendLog( msg ="ERROR: '",view,"' is not an existent view ", type="NMI" ); return; }
    }
    # remove the element from the indicated view
    list.dataLoader[[ view ]]$removeEvents( array.events = array.events)
  }
  #=================================================================================
  # keepOnlyEvents
  # applies the 'keepOnlyEvents' method of the dataLoader class, applied to the chosen view  
  #=================================================================================    
  keepOnlyEvents<-function( array.events=NA, view='main') {  
    # Check if it exists
    if(  !(view %in% names(list.dataLoader) ) )   {
      { obj.logHandler$sendLog( msg ="ERROR: '",view,"' is not an existent view ", type="NMI" ); return; }
    }
    # remove the element from the indicated view
    list.dataLoader[[ view ]]$keepOnlyEvents( array.events = array.events)
  }  
  #=================================================================================
  # addDictionary
  # adds a dictionary to a given view
  #=================================================================================  
  addDictionary<-function( fileName,  column.event.name , sep =',', dict.name='main', view='main'  ) {
    list.dataLoader[[ view ]]$addDictionary( fileName = fileName , sep = sep, dict.name= dict.name , column.event.name = column.event.name) 
  }  
  #=================================================================================
  # plot.Timeline
  # plots the event log timeline for the specified patientID
  #=================================================================================   
  plot.Timeline<-function( patID , view='main' ) {
    list.dataLoader[[ view ]]$plot.Timeline( patID = patID)
  }  
  #=================================================================================
  # translate
  # translate eventLog names according to a pre-loaded dictionary, eventually creating
  # a new view
  #=================================================================================  
  translate<-function( from.view, column.name, to.view='' , build.new.view = TRUE  ) {
    mydata <- list.dataLoader[[ from.view ]]$getTranslation(column.name = column.name)
    if(build.new.view == TRUE ) {
      altriDati <- list.dataLoader[[ from.view ]]$getData()
      listaCampi <- names(mydata)
      listaCampi <- listaCampi[ which( !( listaCampi %in% "pMineR.deltaDate" )) ]
      mydata <- mydata[,listaCampi]
      
      createView(view.name = to.view)

      load.data.frame(mydata = mydata,IDName = altriDati$csv.IDName,EVENTName = altriDati$csv.EVENTName, dateColumnName = altriDati$csv.dateColumnName,view = to.view)
    } else return(mydata)
  }   
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function( verboseMode  ) {
    param.verbose<<-verbose.mode
    list.dataLoader[[ "main" ]]<<- dataLoader(verbose.mode = verboseMode)
    obj.logHandler<<- logHandler();
  }
  costructor( verboseMode = verbose.mode )
  #================================================================================= 
  return(list(
    "load.csv"=load.csv,
    "load.data.frame"=load.data.frame,
    "getData"=getData,
    "keepOnlyEvents"=keepOnlyEvents,
    "removeEvents"=removeEvents,
    "translate"=translate,
    "addDictionary"=addDictionary,
    "createView" = createView,
    "plot.Timeline"=plot.Timeline
  ))
}