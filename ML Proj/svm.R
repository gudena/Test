library(e1071)
fold = 10
svmAcc = 0
svmPrecision = 0
svmRecall = 0
svmFMeasure = 0
id = sample(1:fold, nrow(projtrain), replace = TRUE)
list = 1:fold
for(i in 1:fold){
  trainDataSet  = subset(projtrain, id %in% list[-i]);
  testDataSet = subset(projtrain, id %in% c(i));
  trainDataSet$Survived = as.factor(trainDataSet$Survived);
  svm.model	<- svm(Survived	~	.,	data	=	trainDataSet,	cost	=	18,	kernel = "radial", gamma = 0.09,tolerance = 0.01)
  svm.pred	<- predict(svm.model,	testDataSet[,-1])
  svm_Table <- table(testDataSet$Survived, svm.pred)
  svmAcc = svmAcc + sum(diag(svm_Table))/sum(svm_Table)
  svmPrecision = svmPrecision + diag(svm_Table)/rowSums(svm_Table)
  svmPrecision = mean(svmPrecision)
  svmRecall = svmRecall + diag(svm_Table)/colSums(svm_Table)
  svmRecall = mean(svmRecall)
}
svmAcc = svmAcc*100/fold
svmPrecision <- svmPrecision/fold
svmRecall <- svmRecall/fold
svmFMeasure <- 2*svmPrecision*svmRecall/(svmPrecision+svmRecall)
svmAcc
svmPrecision
svmRecall
svmFMeasure
