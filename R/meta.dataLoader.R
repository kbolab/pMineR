#' provides some more advanced features than dataLoader
#' 
#' @description  This class is  more abstract than dataLoader and provide some facilities. We strongly suggest to adopt meta.dataLoader for quite complex analysys: netherless a meta.dataLoader object can created in any moment starting from an existend dataLoader object
#'              \itemize{
#'              \item \code{meta.dataLoader() } the costructor
#'              \item \code{load.csv( ... ) } loads the a csv file into the \code{meta.dataLoader} object
#'              \item \code{load.data.frame( ... ) } loads the a csv file into the \code{meta.dataLoader} object
#'              \item \code{getData( ... ) } returns the loaded data
#'              } 
#'              There are two ways to use this class: directly using the methods previously 
#'              listed or via wrapping functions (called LD.<method name>). In the examples section you will find an example of both.
#' @useDynLib pMineR    
#' @import stringr utils stats           
#' @export
meta.dataLoader<-function( verbose.mode = TRUE ) {
  list.dataLoader <-list()
  param.verbose<-''
  obj.logHandler<-c()
  #=================================================================================
  # clearAttributes
  #=================================================================================    
  clearAttributes<-function() {
    costructor();
  }
  #=================================================================================
  # load.csv
  #=================================================================================  
  load.csv<-function( nomeFile, IDName, EVENTName,  quote="\"",sep = ",", dateColumnName=NA, view = "main") {

    # if not exist, error
    if( !(view %in% names(list.dataLoader) ) )
      { obj.logHandler$sendLog( msg =c("ERROR: '",view,"' does not exist"), type="NMI" ); return; }
    # ok, load data
    list.dataLoader[[ view ]]$load.csv( nomeFile = nomeFile, IDName = IDName, EVENTName = EVENTName, quote = quote, sep = sep, dateColumnName = dateColumnName)
  
  }
  #=================================================================================
  # load.data.frame
  #=================================================================================  
  load.data.frame<-function( mydata, IDName, EVENTName, dateColumnName=NA, view = "main") {
    # Carica i dati nell'oggetto main.dataLoader interno
    # if not exist, error
    if( !(view %in% names(list.dataLoader) ) )
      { obj.logHandler$sendLog( msg =c("ERROR: '",view,"' does not exist"), type="NMI" ); return; }
    # ok, load data
    list.dataLoader[[ view ]]$load.data.frame(  mydata = mydata, IDName = IDName, EVENTName = EVENTName, dateColumnName = dateColumnName)

  }  
  #=================================================================================
  # getData
  #=================================================================================  
  getData<-function( view = "main") {
    return(  list.dataLoader[[ view ]]$getData()  )
  }
  #=================================================================================
  # copyView
  #=================================================================================  
  copyView<-function( view.name , from.view = "main" ) {
    # 'main' cannot be used :)
    if( view.name == "main") 
      { obj.logHandler$sendLog( msg ="ERROR: 'main' is not a reserved name ", type="NMI" ); return; }
    # An existent name cannot be used
#     if( view.name %in% names(list.dataLoader) ) 
#       { obj.logHandler$sendLog( msg =c("ERROR: '",view.name,"' already exists"), type="NMI" ); return; }
    # An un-existent name is passed as source
    if( !(from.view %in% names(list.dataLoader) ) )
    { obj.logHandler$sendLog( msg =c("ERROR: '",from.view,"' does not exist"), type="NMI" ); return; }
    
    list.dataLoader[[ view.name ]] <<- list.dataLoader[[ from.view ]] 
  }  
  #=================================================================================
  # createView
  #=================================================================================  
  createView<-function( view.name , verbose.mode = TRUE) {
    # 'main' cannot be used :)
    if( view.name == "main") 
    { obj.logHandler$sendLog( msg ="ERROR: 'main' is not a reserved name ", type="NMI" ); return; }
    # An existent name cannot be used
#     if( view.name %in% names(list.dataLoader) ) 
#     { obj.logHandler$sendLog( msg =c("ERROR: '",view.name,"' already exists"), type="NMI" ); return; }
    
    # create the view
    list.dataLoader[[ view.name ]] <<- dataLoader( verbose.mode = verbose.mode)
  }   
  #=================================================================================
  # removeEvents
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
  # removeEvents
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
  #=================================================================================  
  addDictionary<-function( fileName,  column.event.name , sep =',', dict.name='main', view='main'  ) {
    list.dataLoader[[ view ]]$addDictionary( fileName = fileName , sep = sep, dict.name= dict.name , column.event.name = column.event.name) 
  }  
  #=================================================================================
  # plotTimeline
  #=================================================================================   
  plot.Timeline<-function( patID , view='main' ) {
    list.dataLoader[[ view ]]$plot.Timeline( patID = patID)
  }  
  #=================================================================================
  # addDictionary
  #=================================================================================  
  translate<-function( from.view, column.name, to.view='' , build.new.view = TRUE  ) {
    mydata <- list.dataLoader[[ from.view ]]$getTranslation(column.name = column.name)
    if(build.new.view == TRUE ) {
       # browser()
      altriDati <- list.dataLoader[[ from.view ]]$getData()
      
      listaCampi <- names(mydata)
      listaCampi <- listaCampi[ which( !( listaCampi %in% "pMineR.deltaDate" )) ]
      mydata <- mydata[,listaCampi]
      
#       for(indice in names(altriDati$pat.process)) {
#         listaCampi<-names(altriDati$pat.process[[ indice ]])
#         listaCampi <- listaCampi[ which( !( listaCampi %in% "pMineR.deltaDate" )) ]
#         altriDati$pat.process[[ indice ]]<-altriDati$pat.process[[ indice ]][listaCampi]
#       }  
      # browser()
      # mydata$pat.process <- altriDati
      
      # browser()
      createView(view.name = to.view)
      # browser()
      load.data.frame(mydata = mydata,IDName = altriDati$csv.IDName,EVENTName = altriDati$csv.EVENTName, dateColumnName = altriDati$csv.dateColumnName,view = to.view)
      # ct<- 1
      # browser()
      # ct<- 2
      
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