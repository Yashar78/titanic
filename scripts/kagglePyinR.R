#kaggele competition 
#Rahim delaviz 
#setwd("/Domain/tudelft.net/Users/rdelavizaghbolagh/Coursera/DataScience/kaggle/titanic")
setwd("/Users/rahimdelaviz/Coursera/IntroDataScience/courseMaterial/kaggle/titanic")
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


survived <- as.integer(testData$sex=="female")

t <- data.frame("survived"=survived, testData)
write.csv(t, "./data/genderbasedmodelR.csv", row.names=F)

############### start of pythonization 
myTable <- with (trainData, table(survived))
prop.table(myTable)

myTable <- xtabs(~ survived+ pclass , data= trainData)
margin.table(myTable, 2)  
prop.table(myTable, 1)
addmargins(myTable)
addmargins(prop.table(myTable))
##
library(gmodels)
CrossTable(trainData$survived, trainData$pclass)

trainData$col[trainData$pclass == 1] <-"blue"
trainData$col[trainData$pclass == 2] <-"red"
trainData$col[trainData$pclass == 3] <-"green"

plot(trainData$age , trainData$survived, col=ifelse (trainData$sex=="female", "blue", "red"))
plot(trainData$pclass , trainData$survived, col=trainData$col )
plot(trainData$fare , trainData$survived, col=ifelse (trainData$sex=="female", "blue", "red"))
ticketPriceCut <- cut(trainData$fare, breaks=c(0,10,20,30,max(trainData$fare)))
price <- rep(c(10,20,30,512), 6)

sexType <- rep(c("female", "male"), 12)
cabin <- rep(c(1,2,3), 8)
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
  print(trainData[i,]$survived) 
  if (trainData[i,]$survived==1){
    #dicDataFrame[dicDataFrame$sex==sex,dicDataFrame$cabin==passengerClass]$survivalCount #<- dicDataFrame[dicDataFrame$sex==sex,dicDataFrame$cabin==passengerClass]$survivalCount+1
    #print(trainData[i,]$survived) 
    t <- subset (dicDataFrame, sex==sex & cabin==passengerClass & price==fare, select = c(survivalCount))$survivalCount
    dicDataFrame[dicDataFrame$sex==sex & cabin==passengerClass & price==fare]

  }
  else {
    print (subset (dicDataFrame, sex==sex & cabin==passengerClass & price==fare, select = c(deathCount)))
    
  }

  
  
}

