#Read the dataset
myData <- read_csv("C:/Dileep/Fall 2016/RLab/train.csv")

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

#RandomForest
#install.packages("randomForest")
library(randomForest)
set.seed(9)
fold <- 10;

id <- sample(1:fold, nrow(myData), replace = TRUE);
list <- 1:fold;
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