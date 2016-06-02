#' class for handling Process Mining issues
#' 
#' @description  Is the class for handling with Process Mining issues.
#'               Many methods has been developed: please see vignette for more details...
#' @useDynLib pMineR    
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
  createModel<-function( modelName , kindOfModel, parameters.list = list()) {
    if( kindOfModel == "alphaAlgorithm") { 
      listOfModels[[ modelName ]]<<-alphaAlgorithm( parameters.list = parameters.list); 
      return()
    }
    if( kindOfModel == "firstOrderMarkovModel") { 
      listOfModels[[ modelName ]]<<-firstOrderMarkovModel( parameters.list = parameters.list); 
      return()
    }    
    stop("Not yet implemented")
  }
  #=================================================================================
  # replay
  #=================================================================================  
  replay<-function( allModels = TRUE, modelName='', nomeFile = NA , IDName =NA , EVENTName =NA,
                    wordSequence.raw=NA ) {
    
    obj.dataLoader<-dataLoader()

    # se tutti i modelli sono da addestrare in un colpo solo
    if(allModels == TRUE ) {
      listaModelliDaCaricare<-seq(1,length(listOfModels))
    } else listaModelliDaCaricare<-c(modelName)
    # carica il dataset
    if(!is.na(nomeFile)) {
      obj.dataLoader$load.csv(nomeFile = nomeFile,IDName = IDName,EVENTName = EVENTName);
      # prendine i valori
      loadedData<-obj.dataLoader$getData();   
    } else {
      loadedData<-list();
      loadedData$wordSequence.raw<-wordSequence.raw
    }
    for(i in listaModelliDaCaricare) {
      # replay di ogni modello
      single.res<-listOfModels[[i]]$replay( wordSequence.raw = loadedData$wordSequence.raw ); 
    }
    return(single.res)
  }  
  #=================================================================================
  # play
  #=================================================================================  
  play<-function( numberOfPlays = 1, allModels = TRUE, modelName='' ) {
    
    # se tutti i modelli sono da addestrare in un colpo solo
    if(allModels == TRUE ) {
      listaModelliDaCaricare<-seq(1,length(listOfModels))
    } else listaModelliDaCaricare<-c(modelName)
    for(i in listaModelliDaCaricare) {
      # play di ogni modello
      res<-listOfModels[[i]]$play( numberOfPlays = numberOfPlays ); 
    }
    return(res)
  }    
  #=================================================================================
  # trainModel
  #=================================================================================    
  trainModel<-function( allModels = TRUE, modelName='', nomeFile = NA , IDName , EVENTName, 
                        transMatrix=NA, footPrintTable = NA) {
    obj.dataLoader<-dataLoader()
    
    # se tutti i modelli sono da addestrare in un colpo solo
    if(allModels == TRUE ) {
      listaModelliDaCaricare<-seq(1,length(listOfModels))
    } else listaModelliDaCaricare<-c(modelName)

    if( !is.na(nomeFile)) {
      # carica il dataset

      obj.dataLoader$load.csv(nomeFile = nomeFile,IDName = IDName,EVENTName = EVENTName);

      # prendine i valori
      loadedData<-obj.dataLoader$getData();
    } else {
      if(!is.matrix(transMatrix) | !is.matrix(footPrintTable)) stop("If you dont' pass the fileName you should at least pass 'transMatrix and 'footPrintTable ")
      loadedData<-list();
      loadedData$MMatrix<-transMatrix
      loadedData$footPrint<-footPrintTable
    }
    
    # cicla per ogni modelli
    for(i in listaModelliDaCaricare) {
      # carica il dataset in ogni modello
#       listOfModels[[i]]$loadDataset( 
#               transMatrix=loadedData$MMatrix ,
#               footPrintTable = loadedData$footPrint
#               )
      listOfModels[[i]]$loadDataset( dataList = loadedData )      
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
    "replay" = replay,
    "play" = play,
    "plot" = plot
    ))    
}