# 
# obj<-PManager();
# obj$createModel(modelName = "1",kindOfModel = "alphaAlgorithm")
# obj$trainModel(nomeFile = "../otherFiles/rettoMammella.csv",IDName = "ID",EVENTName = "GROUP2")
# obj$trainModel(nomeFile = "../otherFiles/test_01.csv",IDName = "ID",EVENTName = "DES")
# model.xml<-obj$getModel(modelName = "1")
# obj$plot( modelName = "1" )
# # obj$replay(nomeFile = "../otherFiles/rettoMammella.csv", IDName = "ID",EVENTName = "GROUP2")
# obj$replay(nomeFile = "../otherFiles/test_01.csv", IDName = "ID",EVENTName = "DES")
# 
# obj<-PManager();
# obj$createModel(modelName = "1",kindOfModel = "alphaAlgorithm")
# obj$trainModel(nomeFile = "../otherFiles/rettoMammella.csv",IDName = "ID",EVENTName = "GROUP2")
# obj$plot( modelName = "1" )
# obj$replay(nomeFile = "../otherFiles/rettoMammella.csv", IDName = "ID",EVENTName = "GROUP2")

# 
# 
# obj<-PManager();
# #obj$loader(nomeFile = "./otherFiles/rettoMammella.csv",IDName = "ID",EVENTName = "GROUP1")
# #obj$loader(nomeFile = "./otherFiles/test_01.csv",IDName = "ID",EVENTName = "DES")
# obj$loader(nomeFile = "./otherFiles/test_02.csv",IDName = "ID",EVENTName = "DES")
# obj$getAttribute(attributeName = "MMatrix.perc")
# a<-obj$getProb(maxNumStep = 2,statoAttuale = "CHT",statoGoal = "RT",debug=5, killAutoLoop = FALSE)
# b<-obj$getProb(maxNumStep = 4,statoAttuale = "CHT",statoGoal = "RT",debug=5, killAutoLoop = TRUE)
# 
# cat('\n prob(a) = ',a)
# cat('\n prob(b) = ',b)
# 
# obj$alphaAlgorithm()
# 
# 
# 
# 
# 
# 
# obj<-PManager();
# obj$loader(nomeFile = "./otherFiles/test_01.csv",IDName = "ID",EVENTName = "DES")
# a<-obj$alphaAlgorithm()

# rm(obj)
# obj<-PManager();
# obj$loader(nomeFile = "./otherFiles/test_02.csv",IDName = "ID",EVENTName = "DES")
# obj$alphaAlgorithm()
# 
# rm(obj)
# obj<-PManager();
# obj$loader(nomeFile = "./otherFiles/test_03.csv",IDName = "ID",EVENTName = "DES")
# obj$alphaAlgorithm()
# 
# rm(obj)
# obj<-PManager();
# obj$loader(nomeFile = "./otherFiles/test_04.csv",IDName = "ID",EVENTName = "DES")
# obj$alphaAlgorithm()
# 
# rm(obj)
# obj<-PManager();
# obj$loader(nomeFile = "./otherFiles/test_05.csv",IDName = "ID",EVENTName = "DES")
# obj$alphaAlgorithm()
# 
# rm(obj)
# obj<-PManager();
# obj$loader(nomeFile = "./otherFiles/test_06.csv",IDName = "ID",EVENTName = "DES")
# obj$alphaAlgorithm()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
