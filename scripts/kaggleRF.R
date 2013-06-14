#kaggele competition 
#Rahim delaviz 
#look at https://github.com/mattdelhey/kaggle-titanic
#setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")
setwd("/Users/rahimdelaviz/Coursera/IntroDataScience/courseMaterial/kaggle/titanic")
#trainDataClean = read.csv("./data/train_clean.csv")
#testDataClean  = read.csv("./data/test_clean.csv")

load("./data/train_clean_new_sex.RData")
load("./data/test_clean_new_sex.RData")

#unlist(lapply(lapply(trainDataClean , is.na), sum))

library(randomForest)
#+pclass+fare+sibsp+embarked+parch
trainRF <- randomForest(survived ~ sexByName+age+pclass+numFamily+fare, ntree=500,  importance=T,  data= rawTrainData)
summary(trainRF)
trainRF
#testData$embarked[testData$embarked==""] <- "C"

round(importance(trainRF),2)

predTest <- predict(trainRF, rawTestData  , type="response")
rawTestData$survived <- predTest
write.csv(rawTestData, file="./data/NaiveRFModelSexByName.csv", row.names=F)