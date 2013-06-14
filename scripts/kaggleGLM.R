#kaggele competition 
#Rahim delaviz 
#look at https://github.com/mattdelhey/kaggle-titanic
#setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")

rm(list=ls())
setwd("/Users/rahimdelaviz/Coursera/IntroDataScience/courseMaterial/kaggle/titanic")
#trainDataClean = read.csv("./data/train_clean.csv")
#testDataClean  = read.csv("./data/test_clean.csv")

load("./data/train_clean_new_sex.RData")
load("./data/test_clean_new_sex.RData")


glmModel <- glm(survived ~ sexByName+age+pclass+numFamily, data=rawTrainData, family = binomial(link = "logit"))

coef(glmModel)
exp(coef(glmModel))

summary(glmModel)

predTest <- predict(glmModel, rawTestData, type="response")
predTest[predTest < 0.5] <- 0
predTest[predTest >0.5] <- 1

rawTestData$survived <- predTest
write.csv(rawTestData, file="./data/NaiveRFModelGLM.csv", row.names=F)