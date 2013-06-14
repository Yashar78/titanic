#reads and creates a new set of data where categorical variables are factorized and 
# missing values are imputed
# The URL https://github.com/mattdelhey/kaggle-titanic is perfect for learning, some of 
# the code is taken from there, check it for mor info.
rm(list=ls())
#setwd("/Users/rahimdelaviz/Coursera/IntroDataScience/courseMaterial/kaggle/titanic")
setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")
inputTrainFile = "./data/train.csv"
rawTrainData = read.csv("./data/train.csv", stringsAsFactor = F)

rawTrainData$Survival <- NULL
#colnames(rawTrainData)  <- paste("survived")

rawTestData = read.csv("./data/test.csv", stringsAsFactor = F)

# Create a survived variable in the test data set
# Set "0" (did not survive) as the default value

library(plyr)
library(foreign)

survived <- 0
rawTestData <- data.frame("survived"=survived, rawTestData)

# Convert catagorical variables to factors
rawTrainData$survived <- factor(rawTrainData$survived)
rawTrainData$sex <- factor(rawTrainData$sex)
rawTrainData$pclass <- factor(rawTrainData$pclass)
rawTrainData$embarked <- factor(rawTrainData$embarked)

rawTestData$survived <- factor(rawTestData$survived)
rawTestData$sex <- factor(rawTestData$sex)
rawTestData$pclass <- factor(rawTestData$pclass)
rawTestData$embarked <- factor(rawTestData$embarked)


#impute missing values 
#first let see how many are missing
unlist(lapply(lapply(rawTrainData , is.na), sum))
unlist(lapply(lapply(rawTestData , is.na), sum))


#age has the highest number of missing values.
# Combine the data sets for age/fare modeling
full <- join(rawTestData, rawTrainData, type = "full")

#builds linear models
age.mod <- lm(age ~ pclass + sex + sibsp + parch + fare, data = full)
fare.mod <- lm(fare ~ pclass + sex + sibsp + parch + age , data = full)


rawTrainData$age[is.na(rawTrainData$age)] <- 2+predict(age.mod, rawTrainData)[is.na(rawTrainData$age)]
rawTestData$age[is.na(rawTestData$age)] <- 2+predict(age.mod, rawTestData)[is.na(rawTestData$age)]

rawTrainData$fare[is.na(rawTrainData$fare)] <- predict(fare.mod, rawTrainData)[is.na(rawTrainData$fare)]
rawTestData$fare[is.na(rawTestData$fare)] <- predict(fare.mod, rawTestData)[is.na(rawTestData$fare)]

rawTrainData$catFare[rawTrainData$fare<10] <- "1"
rawTrainData$catFare[rawTrainData$fare<20] <- "2"
rawTrainData$catFare[rawTrainData$fare<30] <- "3"
rawTrainData$catFare[rawTrainData$fare>=30] <- "4"
rawTrainData$catFare <- factor(rawTrainData$catFare)

rawTestData$catFare[rawTestData$fare<10] <- "1"
rawTestData$catFare[rawTestData$fare<20] <- "2"
rawTestData$catFare[rawTestData$fare<30] <- "3"
rawTestData$catFare[rawTestData$fare>=30] <- "4"
rawTestData$catFare <- factor(rawTestData$catFare)

#age 
rawTrainData$catAge[rawTrainData$age<5] <- "1"
rawTrainData$catAge[rawTrainData$age<10] <- "2"
rawTrainData$catAge[rawTrainData$age<15] <- "3"
rawTrainData$catAge[rawTrainData$age<22] <- "4"
rawTrainData$catAge[rawTrainData$age<32] <- "5"
rawTrainData$catAge[rawTrainData$age<50] <- "6"
rawTrainData$catAge[rawTrainData$age>=50] <- "7"
rawTrainData$catAge <- factor(rawTrainData$catAge)



rawTestData$catAge[rawTestData$age<5] <- "1"
rawTestData$catAge[rawTestData$age<10] <- "2"
rawTestData$catAge[rawTestData$age<15] <- "3"
rawTestData$catAge[rawTestData$age<22] <- "4"
rawTestData$catAge[rawTestData$age<32] <- "5"
rawTestData$catAge[rawTestData$age<50] <- "6"
rawTestData$catAge[rawTestData$age>=50] <- "7"
rawTestData$catAge <- factor(rawTestData$catAge)

rawTrainData$numFamily <- rawTrainData$sibsp + rawTrainData$parch
rawTestData$numFamily <- rawTestData$sibsp + rawTestData$parch

# Replace missing values in embarked with most popular
rawTrainData$embarked[rawTrainData$embarked == ""] <- "S"
rawTrainData$embarked <- factor(rawTrainData$embarked)

rawTrainData$survived[is.na(rawTrainData$survived)] <- 1
rawTrainData$survived <- factor(rawTrainData$survived)

write.csv(rawTrainData, "./data/train_clean.csv", row.names=F)
write.csv(rawTestData, "./data/test_clean.csv", row.names=F)

save("rawTrainData", file="./data/train_clean.RData")
save("rawTestData", file="./data/test_clean.RData")

