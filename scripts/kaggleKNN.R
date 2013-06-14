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

library(kknn)
# data(iris)
# m <- dim(iris)[1]
# val <- sample(1:m, size = round(m/3), replace = FALSE,
# prob = rep(1/m, m))
# iris.learn <- iris[-val,]
# iris.valid <- iris[val,]
# iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
# kernel = "triangular")
# summary(iris.kknn)
# fit <- fitted(iris.kknn)
# table(iris.valid$Species, fit)
# pcol <- as.character(as.numeric(iris.valid$Species))
# pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
# [(iris.valid$Species != fit)+1])
# data(ionosphere)
# ionosphere.learn <- ionosphere[1:200,]
# ionosphere.valid <- ionosphere[-c(1:200),]
# fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
# table(ionosphere.valid$class, fit.kknn$fit)
# (fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15,
# kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
# table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
# (fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15,
# kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
# table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)
m <- dim(rawTrainData)[1]
val <- sample(1:m, size = round(m/3), replace = F, prob = rep(1/m, m))

rawTrainData.learn <- rawTrainData[-val,]
rawTrainData.valid <- rawTrainData[val,]
#rawTrainData.kknn <- kknn(survived ~ sexByName+age+pclass+numFamily+fare, rawTrainData.learn, rawTrainData.valid , distance = 1, kernel = "triangular")
rawTrainData.kknn <- kknn(survived ~ sexByName+age+numFamily, rawTrainData, rawTestData , distance = 1, k=15, kernel = "rectangular")

#predict(rawTrainData.kknn, rawTestData)


fit <- fitted(rawTrainData.kknn)
#table(rawTrainData.valid$survived, fit)

#simulation(survived ~ sexByName+age+pclass+numFamily+fare, rawTrainData.learn, runs = 20, k=15)
#simulation(survived ~ sexByName+age+pclass+numFamily, rawTrainData.learn, runs = 20, k=15)
#simulation(survived ~ sexByName+age+numFamily, rawTrainData.learn, runs = 20, k=15, kernel = "rectangular", replace=T)

#predTest <- predict(rawTrainData.kknn, rawTestData)
rawTestData$survived <- fit
write.csv(rawTestData, file="./data/NaiveRFModelKNN.csv", row.names=F)
#obtained score is 0.78947
