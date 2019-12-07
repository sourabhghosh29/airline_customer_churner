# Cluster
df_new<-read.csv("E:\\Disk partition\\Syracuse ADS\\Syracuse ADS\\1st Semester\\IST 687 Introduction to Data Science\\project\\airline_customer_churner\\data_new2.csv")
df_new<-df_new[,-1]
install.packages("gbm")
install.packages("xgboost")
library(gbm)
library(xgboost)
library(caTools)

set.seed(88)
split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)
set.seed(123)
# for reproducibility
set.seed(123)


# train GBM model
gbm.fit2 <- gbm(
  formula = Likelihood.to.recommend ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
print(gbm.fit2)
# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)

# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])
## [1] 23112.1

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")

par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
library(ROCR)
p <- predict(gbm.fit2, newdata=subset(test), type="response")
pr <- prediction(p, test$Likelihood.to.recommend)
auc <- performance(pr, measure = "auc")
print(auc)


fitted.results_gbm <- predict(gbm.fit2, newdata=subset(test), type="response")
fitted.results_gbm <- ifelse(fitted.results_gbm > 0.5,1,0)

misClasificError <- mean(fitted.results_gbm != test$Likelihood.to.recommend,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))
"Accuracy 0.763424124513619"
########################################
