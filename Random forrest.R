#Random forest 
df_new<-read.csv("E:\\Disk partition\\Syracuse ADS\\Syracuse ADS\\1st Semester\\IST 687 Introduction to Data Science\\project\\airline_customer_churner\\data_new2.csv")
df_new<-df_new[,-1]
# Create model with default paramters
library(randomForest)
library(mlbench)
library(caret)
   
par(mar = c(5, 8, 1, 1))
library(randomForest)

set.seed(88)
#splitting data into training and test data set for logistic regression 
split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)

#function that returns random forest feature importance plots
create_rfplot <- function(rf_default, type){
  
  imp <- importance(rf_default, type = type, scale = F)
  
  featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[,1])
  
  p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.65) +
    coord_flip() + 
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, color = "black"),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = 15, color = "black"),
          axis.text.y  = element_text(size = 15, color = "black")) 
  return(p)
}

#####Random forest model after parameter tuning the best model performs with 200 trees
rf1 <- randomForest(
  Likelihood.to.recommend ~ .,  
  ntree = 200,
  data = train,
  nodesize = 1, 
  replace = FALSE,
  importance = TRUE
)     
#printing output of random forest
print(rf1)
#plotting feature importance graph of random forest
create_rfplot(rf1, type = 2)

#Calculating AUC for the model
library(ROCR)
p <- predict(rf1, newdata=subset(test), type="response")
pr <- prediction(p, test$Likelihood.to.recommend)
auc <- performance(pr, measure = "auc")
print(auc)
#AUC 0.8471364

#calculating Accuracy of the model
fitted.results_rf <- predict(rf1,newdata=subset(test),type='response')
fitted.results_rf <- ifelse(fitted.results_rf > 0.4,1,0)

misClasificError <- mean(fitted.results_rf != test$Likelihood.to.recommend,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))
#"Accuracy 0.761089494163424"
