test.01<-function() {
  
  # build a Project Manager Object
  obj<-PManager();
  # populate it with an Alpha Algoritm model
  obj$createModel(modelName = "1",kindOfModel = "alphaAlgorithm")
  # load the data separately ( we wan to split the dataset, so we
  # will not use the PManager::trainModel(); method    )
  obj.dataLoader<-dataLoader()
  # load the dataset
  obj.dataLoader$buildSplittedLoader(nomeFile = "./otherFiles/rettoMammella.csv",IDName = "ID",EVENTName = "GROUP1",splitDataSet = c(.7,.3) );

  
  
  obj$trainModel(nomeFile = "../otherFiles/test_02.csv",IDName = "ID",EVENTName = "DES")
  obj$replay(nomeFile = "../otherFiles/test_02.csv", IDName = "ID",EVENTName = "DES")
  
  
}