#kaggele competition 
#Rahim delaviz 
#setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")
setwd("/Users/rahimdelaviz/Coursera/IntroDataScience/courseMaterial/kaggle/titanic")
inputTrainFile = "./data/train.csv"
trainData = read.csv(inputTrainFile)
#trainData$Survival <- NULL
#write.csv(trainData, inputTrainFile, row.names=F)
inputTestFile =  "./data/test.csv"
testData = read.csv(inputTestFile)

library(randomForest)
trainData$survived[1] <- 1
trinRF <- randomForest( survived ~ sex+age, data= trainData)

trainData$age <- na.roughfix(trainData$age)
trainData$survived[is.na(trainData$survived)] <- 1

trianRF <- randomForest( survived ~ sex+age+pclass+fare+sibsp+embarked+parch, data= trainData)

#plot(randomForest( survived ~ sex+age+pclass+fare+sibsp+embarked+parch, ntree=500, data= trainData) )
#trainData$embarked[trainData$embarked==""] <- "C"

trainRF <- randomForest( survived ~ sex+age+pclass+fare+sibsp+embarked+parch, ntree=500,mtry = 2,  importance=T,  data= trainData) 
#testData$embarked[testData$embarked==""] <- "C"

round(importance(trainRF),2)
tt <- predict(trainRF, trainData[1:10,])

predTest <- predict(trainRF, subset(testData, select=c(sex,age,pclass,fare,sibsp,embarked,parch))   , type="response")



