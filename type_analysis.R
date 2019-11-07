library(jsonlite)
airplaneData <- jsonlite::fromJSON('E:/Disk partition/Syracuse ADS/Syracuse ADS/1st Semester/IST 687 Introduction to Data Science/project/fall2019-survey-M07.json')
View(airplaneData)
summary(airplaneData)
str(airplaneData)
#'data.frame':	10282 obs. of  32 variables:
colSums(is.na(airplaneData))
#na values :
#Departure.Delay.in.Minutes:225     Arrival.Delay.in.Minutes:259
#Flight.time.in.minutes: 259      freeText: 10000

library(mice)
md.pattern(airplaneData)

library(VIM)
mice_plot <- aggr(airplaneData, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(airplaneData), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

install.packages('DMwR')
library(DMwR)
View(airplaneData)
help(factor)
factor(airplaneData$Gender)
View(airplaneData$Gender)
airplaneData$Gender <- factor(airplaneData$Gender, labels = "")
airplaneData$Airline.Status <- factor(airplaneData$Airline.Status, labels = "")
airplaneData$Type.of.Travel <- factor(airplaneData$Type.of.Travel, labels = "")
airplaneData$Class <- factor(airplaneData$Class, labels = "")
airplaneData$Partner.Code <- factor(airplaneData$Partner.Code, labels = "")
airplaneData$Partner.Name<- factor(airplaneData$Partner.Name, labels = "")
airplaneData$Origin.City<- factor(airplaneData$Origin.City, labels = "")
airplaneData$Origin.State<- factor(airplaneData$Origin.State, labels = "")
airplaneData$Destination.City<- factor(airplaneData$Destination.City, labels = "")
airplaneData$Destination.State<- factor(airplaneData$Destination.State, labels = "")

colSums(is.na(airplaneData))

predictorFeatures <- airplaneData[-airplaneData$Likelihood.to.recommend]

predictorFeatures <- airplaneData[ , -which(names(airplaneData) %in% c("Likelihood.to.recommend","freeText"))]
View(predictorFeatures)


