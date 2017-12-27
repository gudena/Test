
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
