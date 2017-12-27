#install.packages("corrplot")
#install.packages("randomForest")
#install.packages("class")
#install.packages("e1071")
#install.packages("rpart")
#install.packages("caret")
#install.packages("adabag")

#Read the dataset
myData <- read.csv("C:/Dileep/Fall 2016/RLab/train.csv")

#Removing the redundant columns from data
myData <- myData[-c(1,4,9,10,11)]
View(myData)
head(myData)
summary(myData)

#Addressing the NAs in 'Embarked'
myData$Embarked <- as.numeric(as.factor(myData$Embarked))

#Replacing the NAs in age with the average age value
ageavg <- mean(na.omit(myData$Age))
myData <- myData[!is.na(myData$Embarked),]
myData[is.na(myData)] <- ageavg

#Categorizing the age value into 3 categories
myData[,3] <- ifelse(myData[,3]=="male",1, 2)
myData[,4] <- ifelse(myData[,4] <= 18,1, ifelse(myData[,4] <= 40,2,3))

#Correlation plot
library(corrplot)
p <- cor(myData)
corrplot(p,method = "circle")

fold <- 10;

id <- sample(1:fold, nrow(myData), replace = TRUE);
list <- 1:fold;

#RandomForest

library(randomForest)
set.seed(9)

TestAccuracy <- 0;
precisionRF <- 0;
recallRF <- 0;

for (i in 1:fold) {
  myDataTrain  <- subset(myData, id %in% list[-i]);
  myDataTest <- subset(myData, id %in% c(i));
  rfModel <- randomForest(as.factor(Survived)~., myDataTrain, importance = FALSE, ntree=300, proximity = TRUE);
  predRandomForestTesting <- predict(rfModel, myDataTest);
  TestRandomTable <- table(predRandomForestTesting, myDataTest$Survived);
  TestAccuracy <- TestAccuracy + sum(diag(TestRandomTable))/sum(TestRandomTable);
  precisionRF <- precisionRF + diag(TestRandomTable)/rowSums(TestRandomTable)
  precisionRF <- mean(precisionRF)
  recallRF <- recallRF + diag(TestRandomTable)/colSums(TestRandomTable)
  recallRF <- mean(recallRF)
}

TestAccuracy <- TestAccuracy*100/fold;
p <- precisionRF/fold
r <- recallRF/fold
f <- (2*r*p)/(r+p)
print("Accuracy:")
TestAccuracy
print("Precision:")
p
print("Recall")
r
print("F-measure")
f
TestRandomTable
predRandomForestTesting
plot(rfModel, log = "y")
importance(rfModel)

#KNN

library(class)

sum=0
sum_val <- c(0,0,0,0)
for (i in 1:fold)
{
  training<- subset(myData, id %in% list[-i])
  test <- subset(myData, id %in% c(i))
  knearest<- knn(training[,2:7], test[,2:7],training[,1],k=15)
  knear<- table("Predictions" = knearest, Actual = test[,1])
  err <- (sum(diag(knear)) / sum(knear))*100.0
  sum <- sum + err
  dt_table <- table("Predictions" = knearest, Actual = test[,1])
  accuracy <- (sum(diag(dt_table)) / sum(dt_table))*100.0
  precision <- diag(dt_table) / rowSums(dt_table)
  precision <- mean(precision)
  recall <- (diag(dt_table) / colSums(dt_table))
  recall <- mean(recall)
  FScore <- (2*precision*recall)/(precision+recall)
  FScore <- mean(FScore)
  sum_val[1] <- sum_val[1] + accuracy
  sum_val[2] <- sum_val[2] + precision
  sum_val[3] <- sum_val[3] + recall
  sum_val[4] <- sum_val[4] + FScore
  
}
accu<-sum/fold
accu
acc <- sum_val[1]/fold
print(paste(" Accuracy is ", acc))
precision <- sum_val[2]/fold
print(paste(" Precision is ", precision))
recall <- sum_val[3]/fold
print(paste(" Recall is ", recall))
FScore <- sum_val[4]/fold
print(paste(" FScore is ", FScore))

#SVM

library(e1071)
svmAcc = 0
svmPrecision = 0
svmRecall = 0
svmFMeasure = 0
for(i in 1:fold){
  trainDataSet  = subset(myData, id %in% list[-i]);
  testDataSet = subset(myData, id %in% c(i));
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

#Decision Tree
library(rpart)
library(caret)
sum=0
sum_val <- c(0,0,0,0)

for (i in 1:fold)
{
  training<- subset(myData, id %in% list[-i])
  test <- subset(myData, id %in% c(i))
  train1<-training
  train1$Survived <- as.factor(training$Survived)
  dtree <- rpart(Survived~.,training, method = "class",control = rpart.control(cp=0.001,minsplit= 30,maxdepth = 5,minbucket = 5))
  cmatrix<-predict(dtree,test, type = "class")
  cm<-table(test[,1],cmatrix)
  
  accuracy <- (sum(diag(cm)) / sum(cm))*100.0
  precision <- diag(cm) / rowSums(cm)
  precision <- mean(precision)
  recall <- (diag(cm) / colSums(cm))
  recall <- mean(recall)
  FScore <- (2*precision*recall)/(precision+recall)
  FScore <- mean(FScore)
  sum_val[1] <- sum_val[1] + accuracy
  sum_val[2] <- sum_val[2] + precision
  sum_val[3] <- sum_val[3] + recall
  sum_val[4] <- sum_val[4] + FScore
  
}
accurac<-confusionMatrix(cm)
accurac
acc <- sum_val[1]/fold
print(paste(" Accuracy is ", acc))
precision <- sum_val[2]/fold
print(paste(" Precision is ", precision))
recall <- sum_val[3]/fold
print(paste(" Recall is ", recall))
FScore <- sum_val[4]/fold
print(paste(" FScore is ", FScore))

#Boosting

library(adabag)

sum=0
sum_val <- c(0,0,0,0)

for (i in 1:fold)
{
  myDataTrain<- subset(myData, id %in% list[-i])
  myDataTest <- subset(myData, id %in% c(i))
  tempData<-myDataTrain
  tempData$Survived <- as.factor(myDataTrain$Survived)
  BoostModel <- boosting(Survived ~ .,data=tempData,mfinal=13,iter=10,
                         rpart.control(minsplit = 5,maxdepth=5))    
  predicted<-predict(BoostModel,myDataTest)
  dt_table<-predicted$confusion
  accuracy <- (sum(diag(dt_table)) / sum(dt_table))*100.0
  precision <- diag(dt_table) / rowSums(dt_table)
  precision <- mean(precision)
  recall <- (diag(dt_table) / colSums(dt_table))
  recall <- mean(recall)
  FMeasure <- (2*precision*recall)/(precision+recall)
  FMeasure <- mean(FMeasure)
  sum_val[1] <- sum_val[1] + accuracy
  sum_val[2] <- sum_val[2] + precision
  sum_val[3] <- sum_val[3] + recall
  sum_val[4] <- sum_val[4] + FMeasure
  
}
acc <- sum_val[1]/fold
print(paste(" Accuracy is: ", acc))
precision <- sum_val[2]/fold
print(paste(" Precision is: ", precision))
recall <- sum_val[3]/fold
print(paste(" Recall is: ", recall))
FMeasure <- sum_val[4]/fold
print(paste(" FMeasure is: ", FMeasure))

