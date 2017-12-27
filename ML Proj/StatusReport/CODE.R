#Read the dataset
myData <- read.csv("D:/Academics/Fall_2016/ML/Assignments/Project/train.csv")
View(myData)

#Pre-processing
projtrain <- myData
#Removing the attributes that are not required
projtrain <- projtrain[-c(1,4,9,10,11)]
View(projtrain)
head(projtrain)
summary(projtrain)

projtrain$Embarked <- as.numeric(as.factor(projtrain$Embarked))
projtrain$Embarked
ageavg<-mean(na.omit(projtrain$Age))
ageavg
#Replacing the NAs in the age attribute with the average age value
projtrain[is.na(projtrain)]<-ageavg
projtrain[,3] = ifelse(projtrain[,3]=="male",1, 2)
projtrain[,4] = ifelse(projtrain[,4]<=18,1, ifelse(projtrain[,4]<=40,2,3))

#Finding the correlation plot
library(corrplot)
p<-cor(projtrain)
corrplot(p,method = "circle")

#Plotting the Histograms
hist(projtrain$Survived)
hist(projtrain$Pclass)
hist(projtrain$Sex)
hist(projtrain$Age)
hist(projtrain$SibSp)
hist(projtrain$Parch)
hist(projtrain$Embarked)

