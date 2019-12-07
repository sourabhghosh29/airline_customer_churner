##### LOGISTIC REGRESSION #####
df_new<-read.csv("E:\\Disk partition\\Syracuse ADS\\Syracuse ADS\\1st Semester\\IST 687 Introduction to Data Science\\project\\airline_customer_churner\\data_new_detractor.csv")
df_new<-df_new[,-1]
library(caTools)

set.seed(88)
split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)
model <- glm(Likelihood.to.recommend ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")

fitted.results <- predict(model,newdata=subset(test),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Likelihood.to.recommend,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))

###"Accuracy 0.756420233463035"


############################threshold variation
fitted1.results_lr <- predict(model,newdata=subset(test),type='response')
fitted1.results_lr <- ifelse(fitted1.results_lr > 0.5,1,0)

misClasificError1 <- mean(fitted1.results_lr != test$Likelihood.to.recommend,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError1))
#"Accuracy 0.75875486381323"


y_train <- dresstrain$Likelihood.to.recommend

x_train <-dresstrain%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
                             ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
                             ,Flight.Distance,Flight.cancelled) %>% data.matrix()
library(ROCR)
p <- predict(model, newx=subset(x_train), type="response")
pr <- prediction(p, y_train)
auc <- performance(pr, measure = "auc")
auc
#without arrival delay 
#0.8409551
#with arrival delay
#0.8428508
######################################hyper parameter tuning#####################################
library('caret')
set.seed(88)

split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
dresstrain <- subset(df_new, split == TRUE)
dresstest <- subset(df_new, split == FALSE)
modelLookup('glm')
method = 'glm'
library(glmnet)
library(tidyverse)
#install.packages('glmnet')
#C:\Users\soura\AppData\Local\Temp\RtmpE9LKxe\downloaded_packages

y <- dresstrain$Likelihood.to.recommend

x<-dresstrain%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
                        ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
                        ,Flight.Distance,Flight.cancelled) %>% data.matrix()
typeof(x)
str(x)
#fit = glmnet(x,y,alpha = 0.2, nlambda = 20)
#fitted.values(1)


# set up the grid
tuneGrid <- expand.grid(.alpha = seq(0, 1, 0.05), .lambda = seq(0, 2, 0.05))
## 10-fold CV ##
#fitControl <- trainControl(method = 'cv', number = 10, classProbs = TRUE, summaryFunction = twoClassSummary) 

#works with a single factor variable  (ignore warnings based on small sample size)
mod_elnet<-cv.glmnet(x,y, data=dresstrain, method="glmnet", 
                     family="binomial", tuneGrid = tuneGrid, metric = "ROC",type.measure = "auc")
mod_elnet
#min lmabda after arrival delay binning
#0.0007238316
cbind(mod_elnet$lambda,mod_elnet$cvm)

# best parameter
mod_elnet$lambda.min

# best coefficient
coef(mod_elnet, s = "lambda.min")

#cross validation 
#x_test<-dresstest %>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
#                        ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
#                       ,Flight.Distance,Flight.cancelled) %>% data.matrix()
pred = predict(mod_elnet, newx = x_test, type = 'response',s ="lambda.min")
#auc(y,pred)


#install.packages('AUC')
library(AUC)
library(ROC)
ROCRpred <- prediction(response,y)
help(AUC)
auc(y,response)
y_test <- dresstest$Likelihood.to.recommend

x_test <-dresstest%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
                        ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
                        ,Flight.Distance,Flight.cancelled) %>% data.matrix()
fitted.results <- predict(mod_elnet,newx=subset(x_test),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != y_test,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))
"Accuracy 0.702334630350194"
library(ROCR)
p <- predict(mod_elnet, newx=subset(x_test), type="response")
pr <- prediction(p, y_test)
auc <- performance(pr, measure = "auc")
auc
# 0.7657839
################################################ best model logistic determined ##########################################