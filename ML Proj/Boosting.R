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


