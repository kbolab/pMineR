test.01<-function() {
  
  # build a Project Manager Object
  obj<-PManager();
  # populate it with an Alpha Algoritm model
  obj$createModel(modelName = "1",kindOfModel = "alphaAlgorithm")
  
  
  # load the data separately ( we wan to split the dataset, so we
  # will not use the PManager::trainModel(); method    )
  obj.dataLoader<-dataLoader()

  # get the tables
  res<-obj.dataLoader$buildSplittedLoaderDataAndTables(nomeFile = "../otherFiles/rettoMammella.csv",IDName = "ID",EVENTName = "GROUP1",splitDataSet = c(.7,.3) );

  # train the model on the #1
  obj$trainModel( transMatrix = res[[1]]$MMatrix , footPrintTable = res[[1]]$footPrint  )

  obj$replay(wordSequence.raw = res[[1]]$wordSequence.raw)

  
  
}

