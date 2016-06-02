
testDino<-function() {

  obj.retto<-firstOrderMarkovModel()
  obj.mammella<-firstOrderMarkovModel()
  
  obj.L<-dataLoader()
  obj.L$load.csv( nomeFile="../otherFiles/retto.csv",IDName = "ID",EVENTName = "GROUP2") 
  dati.retto<-obj.L$getData()
  obj.L$load.csv( nomeFile="../otherFiles/mammella.csv",IDName = "ID",EVENTName = "GROUP2") 
  dati.mammella<-obj.L$getData()  
    
  obj.retto$loadDataset( dati.retto )
  obj.mammella$loadDataset( dati.mammella )
  obj.retto$trainModel()
  obj.mammella$trainModel()  
  
  dist<-obj.retto$distanceFrom( obj.mammella); print(dist);
  
  # prendi tutte le parole della mammella e del retto
  parole.mammella<-dati.mammella$wordSequence.raw
  parole.retto<-dati.retto$wordSequence.raw
  
  listaParole<-list(  "mammella"=parole.mammella, "retto"= parole.retto)
  
  matriceDist<-c(); ct<-1
  for(qualeLista in names(listaParole)) {
    for( i in seq(1,length(listaParole[[qualeLista]]))) {
      singolaParola<-list( "1"=listaParole[[qualeLista]][[i]] ) 
      obj.single.MM<-firstOrderMarkovModel()
      obj.L$load.listOfSimpleWords(load.listOfSimpleWords = singolaParola,IDName = "ID",EVENTName = "Evento")
      datiOttenuti<-obj.L$getData()
      
      obj.single.MM$loadDataset( datiOttenuti )
      obj.single.MM$trainModel()
      
      dist.retto<-obj.single.MM$distanceFrom( obj.retto )
      dist.mammella<-obj.single.MM$distanceFrom( obj.mammella )
      matriceDist<-rbind( matriceDist, c(  dist.retto ,  dist.mammella, qualeLista, ''  )   )
      print(ct)
      ct<-ct+1
    }
  }
  colnames(matriceDist)<-c("dist.retto","dist.mammella","rightList","previsione")
  
  for(i in seq(1,nrow(matriceDist))) {
    valRetto<-mean(as.numeric(matriceDist[,1]))
    valMammella<-mean(as.numeric(matriceDist[,2]))
    if(valRetto<valMammella) matriceDist[,"previsione"] = "retto"
    else matriceDist[,"previsione"] = "mammella"
  }
  
  
  # uppa1<-list( "1"=c("CHT","ECO","IMAGING","IMAGING") )
  # uppa2<-list( "1"=c("CHT","RT","IMAGING","IMAGING","CHIR","IMAGING") )
  # obj.retto<-firstOrderMarkovModel()
  # obj.mammella<-firstOrderMarkovModel()
  # obj.L<-dataLoader()
  # obj.L$load.listOfSimpleWords(load.listOfSimpleWords = uppa1,IDName = "ID",EVENTName = "Evento")
  # aaa1<-obj.L$getData()
  # obj.L$load.listOfSimpleWords(load.listOfSimpleWords = uppa2,IDName = "ID",EVENTName = "Evento")
  # aaa2<-obj.L$getData()
  # obj.M1$loadDataset( aaa1 )
  # obj.M2$loadDataset( aaa2 )
  # obj.M1$trainModel()
  # obj.M2$trainModel()
  
}


generatore<-function() {
#   log<-list()
#   do( "RX Torace", log, ".8" )
#   do( "TAC Torace", log, "1" )
#   do( "Fibrobroncoscopia", log, ".8" )
#   do( "Citologico", log, ".8" )
#   do( "Agoaspirato_transtorarico", log, ".8" )
#   
#   "RX Torace","TAC Torace","Fibrobroncoscopia","Citologico Escreato","Agoaspirato_transtorarico"
#   
  
}


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

