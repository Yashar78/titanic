#kaggele competition 
#Rahim delaviz 
#setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")
setwd("/Users/rahimdelaviz/Coursera/IntroDataScience/courseMaterial/kaggle/titanic")
inputTrainFile = "./data/train.csv"
trainData = read.csv(inputTrainFile)

inputTestFile = "./data/test.csv"
testData = read.csv(inputTestFile)


############### start of pythonization 
# myTable <- with (trainData, table(survived))
# prop.table(myTable)
# 
# myTable <- xtabs(~ survived+ pclass , data= trainData)
# margin.table(myTable, 2)  
# prop.table(myTable, 1)
# addmargins(myTable)
# addmargins(prop.table(myTable))
# ##
# library(gmodels)
# CrossTable(trainData$survived, trainData$pclass)
# 
# trainData$col[trainData$pclass == 1] <-"blue"
# trainData$col[trainData$pclass == 2] <-"red"
# trainData$col[trainData$pclass == 3] <-"green"
# 
# plot(trainData$age , trainData$survived, col=ifelse (trainData$sex=="female", "blue", "red"))
# plot(trainData$pclass , trainData$survived, col=trainData$col )
# plot(trainData$fare , trainData$survived, col=ifelse (trainData$sex=="female", "blue", "red"))
# ticketPriceCut <- cut(trainData$fare, breaks=c(0,10,20,30,max(trainData$fare)))
library(hash)


price <- c()
sexType <- c()
cabin <- c()

for ( i in c(10,20,30,512)){
  for (j in c("male","female")){
    for ( k in c(1,2,3)){
      price <- c(price,i)
      sexType <- c(sexType,j)
      cabin <- c(cabin,k)
      
    }
  }
}


survivalCount <- rep(0,24)
deathCount <- rep(0,24)

dicDataFrame <- data.frame("price" = price, "sex"=sexType, "cabin"=cabin,
                           "survivalCount"=survivalCount, "deathCount" = deathCount)

for (i in 1:length(trainData[,1])){
  #print(trainData[i,])
  fare <- trainData[i,]$fare
  if (trainData[i,]$fare <10)
    fare = 10
  else if(trainData[i,]$fare <20)
    fare = 20
  else if (trainData[i,]$fare <30)
    fare = 30
  else
    fare=512
  sex <- trainData[i,]$sex
  passengerClass <- trainData[i,]$pclass
  if (!is.na(trainData[i,]$survived)){
  if (trainData[i,]$survived==1){
    t <- dicDataFrame$survivalCount[dicDataFrame$sex==sex & dicDataFrame$cabin==passengerClass & dicDataFrame$price==fare]
    dicDataFrame$survivalCount[dicDataFrame$sex==sex & dicDataFrame$cabin==passengerClass & dicDataFrame$price==fare] <- t+1
  }
  else {
    t <- dicDataFrame$deathCount[dicDataFrame$sex==sex & dicDataFrame$cabin==passengerClass & dicDataFrame$price==fare]
    dicDataFrame$deathCount[dicDataFrame$sex==sex & dicDataFrame$cabin==passengerClass & dicDataFrame$price==fare] <- t+1
  }
  }
}

survivalRatio <- dicDataFrame$survivalCount / (dicDataFrame$survivalCount+dicDataFrame$deathCount)
dicDataFrame <- data.frame(dicDataFrame, "survivalRatio"= survivalRatio)
testSurvived <- c()
for (i in 1:length(testData[,1])){
  fare <- trainData[i,]$fare
  if (trainData[i,]$fare <10)
    fare = 10
  else if(trainData[i,]$fare <20)
    fare = 20
  else if (trainData[i,]$fare <30)
    fare = 30
  else
    fare=512
  sex <- trainData[i,]$sex
  passengerClass <- trainData[i,]$pclass
  s <- dicDataFrame$survivalRatio[dicDataFrame$sex==sex & dicDataFrame$cabin==passengerClass & dicDataFrame$price==fare]
  if (is.na(s)){
    if (sex=="female")
      testSurvived <- c(testSurvived,1)
    else
      testSurvived <- c(testSurvived,0)
  }
  else if (s < 0.5) 
    testSurvived <- c(testSurvived,0)
  else
    testSurvived <- c(testSurvived,1)
}
  
testData <- data.frame("survived"=testSurvived, testData)
write.csv(testData, "./data/combinedModelR.csv", row.names=F)
