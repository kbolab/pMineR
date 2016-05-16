#' class for handling Process Mining issues
#' 
#' @description  Is the class for handling with Process Mining issues.
#'               Many methods has been developed: please see vignette for more details...
#' @useDynLib processMining               
#' @export
#' @import DiagrammeR 
PManager<-function() {
  listOfModels<-list()

  #=================================================================================
  # getProb - future
  #=================================================================================  
  getProb<-function( stepAttuale=0, maxNumStep =1, statoAttuale="BEGIN", statoGoal ="END", debug=0,debugString=NA,killAutoLoop=FALSE) {
    if(is.na(debugString)) debugString<-statoAttuale;
    if( killAutoLoop == FALSE ) matriceDaConsiderare <- "MMatrix.perc"
    if( killAutoLoop == TRUE ) matriceDaConsiderare <- "MMatrix.perc.noLoop"
    
    if( statoAttuale==statoGoal ) {
      cat('\n ',debugString);
      return( 1 );
    }  
    if( stepAttuale == maxNumStep & (statoAttuale!=statoGoal) ) {
      return( 0 );
    }  
    MMPerc<-getAttribute(attributeName = matriceDaConsiderare)
    prob<-0; 
    for( possibileNuovoStato in rownames(MMPerc)) {
      if(getAttribute(attributeName = matriceDaConsiderare)[statoAttuale,possibileNuovoStato]>0) {
        newdebugString<-paste(c(debugString,'=>',possibileNuovoStato),collapse='');
        addendo<-getAttribute(attributeName = matriceDaConsiderare)[statoAttuale,possibileNuovoStato] * getProb( stepAttuale+1, maxNumStep,  possibileNuovoStato ,  statoGoal , debug = debug,debugString = newdebugString, killAutoLoop=killAutoLoop);
        prob<-prob + addendo
      }
    }  
    return(prob);
  }
  #=================================================================================
  # createModel
  #=================================================================================  
  createModel<-function( modelName , kindOfModel, parameter = list()) {
    if( kindOfModel == "alphaAlgorithm") { 
      listOfModels[[ modelName ]]<<-alphaAlgorithm(); 
      return();
    }
    stop("Not yet implemented")
  }
  #=================================================================================
  # replay
  #=================================================================================  
  replay<-function( allModels = TRUE, modelName='', nomeFile , IDName , EVENTName  ) {
    obj.dataLoader<-dataLoader()
    # se tutti i modelli sono da addestrare in un colpo solo
    if(allModels == TRUE ) {
      listaModelliDaCaricare<-seq(1,length(listOfModels))
    } else listaModelliDaCaricare<-c(modelName)
    # carica il dataset
    obj.dataLoader$load(nomeFile = nomeFile,IDName = IDName,EVENTName = EVENTName);
    # prendine i valori
    loadedData<-obj.dataLoader$getData();   
    browser();
    for(i in listaModelliDaCaricare) {
      # carica il dataset in ogni modello
      listOfModels[[i]]$replay( 
        transMatrix=loadedData$MMatrix ,
        footPrintTable = loadedData$footPrint
      )    
    }
    return;
  }  
  #=================================================================================
  # trainModel
  #=================================================================================    
  trainModel<-function( allModels = TRUE, modelName='', nomeFile , IDName , EVENTName ) {
    obj.dataLoader<-dataLoader()
    # se tutti i modelli sono da addestrare in un colpo solo
    if(allModels == TRUE ) {
      listaModelliDaCaricare<-seq(1,length(listOfModels))
    } else listaModelliDaCaricare<-c(modelName)
    
    # carica il dataset
    obj.dataLoader$load(nomeFile = nomeFile,IDName = IDName,EVENTName = EVENTName);
    # prendine i valori
    loadedData<-obj.dataLoader$getData();
    # cicla per ogni modelli
    for(i in listaModelliDaCaricare) {
      # carica il dataset in ogni modello
      listOfModels[[i]]$loadDataset( 
              transMatrix=loadedData$MMatrix ,
              footPrintTable = loadedData$footPrint
              )
      # addestra il modello
      listOfModels[[i]]$trainModel();
    }
     
  }
  #=================================================================================
  # getModel
  #=================================================================================   
  getModel<-function( modelName , kindOfOutput ="XML") {
    listOfModels[[modelName]]$getModel( kindOfOutput = kindOfOutput);
  }
  #=================================================================================
  # plot
  #=================================================================================   
  plot<-function( modelName) {
    listOfModels[[modelName]]$plot()
  }
  #=================================================================================
  # Constructor
  #=================================================================================
  costructor<-function( ) {
    listOfModels<<-list()
  }
  costructor();
  
  return(list(
    "getProb"=getProb,
    "createModel"=createModel,
    "trainModel" = trainModel,
    "getModel" = getModel,
    "plot" = plot
    ))    
}