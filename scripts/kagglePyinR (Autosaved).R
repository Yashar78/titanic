#kaggele competition 
#Rahim delaviz 
setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")
inputTrainFile = "./data/train.csv"
trainData = read.csv(inputTrainFile)

inputTestFile = "./data/test.csv"
testData = read.csv(inputTestFile)


cat("Dimenstion: ",dim(trainData))
cat("Names of the columns:\n", names(trainData))

surviveCol  = trainData$survived

number_passengers = length(surviveCol)
number_survided = sum(surviveCol==1)
number_dead = sum(surviveCol==0)
proportion_survivors = number_survided / number_passengers

#Woman and men data separated.

women_on_board = trainData[trainData$sex=="female",]
men_on_board = trainData[trainData$sex=="male",]

cat("proportion women survided:\t",
proportion_women_survived = sum(women_on_board$survived) / length(women_on_board$survived))

cat("proportion men survided:\t",
proportion_men_survived = sum(men_on_board$survived) / length(men_on_board$survived))

#read test data 

survived <- as.integer(testData$sex=="female")

testData <- data.frame("survived"=survived, testData)
write.csv(testData, "./data/genderbasedmodelpy.csv")
