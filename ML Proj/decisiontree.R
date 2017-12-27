sum=0
sum_val <- c(0,0,0,0)




#decisiontree

for (i in 1:fold)
{
  training<- subset(myData, id %in% list[-i])
  test <- subset(myData, id %in% c(i))
  train1<-training
  train1$Survived <- as.factor(training$Survived)
dtree <- rpart(Survived~.,training_Data, method = "class",control = rpart.control(cp=0.001,minsplit= 30,maxdepth = 5,minbucket = 5))
plot(dtree)
text(dtree)
rpart.plot(dtree)
cmatrix<-predict(dtree,testing_Data, type = "class")
cm<-table(testing_Data[,1],cmatrix)

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
 

 







