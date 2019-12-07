# Cluster
df_new<-read.csv("E:\\Disk partition\\Syracuse ADS\\Syracuse ADS\\1st Semester\\IST 687 Introduction to Data Science\\project\\airline_customer_churner\\data_new2.csv")
df_new<-df_new[,-1]
View(df_new)
require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')
library(xgboost)

library(caTools)

set.seed(88)
split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)
set.seed(123)
# for reproducibility
set.seed(123)

sparse_matrix <- sparse.model.matrix(Likelihood.to.recommend~.-1, data = train)
output_vector = train$Likelihood.to.recommend

bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)
xgb.plot.importance(importance_matrix = importanceRaw[1:10])


y_train <- train$Likelihood.to.recommend

x_train <-train%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
                               ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
                               ,Flight.Distance,Flight.cancelled) %>% data.matrix()


importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
dtrain <- xgb.DMatrix(x_train, label = y_train)
y_test <- test$Likelihood.to.recommend

x_test <-test%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
                        ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
                        ,Flight.Distance,Flight.cancelled) %>% data.matrix()

sparse_matrix_test <- sparse.model.matrix(Likelihood.to.recommend~.-1, data = test)
output_vector_test = test$Likelihood.to.recommend
pred <- predict(bst, sparse_matrix_test)
pr <- prediction(pred, output_vector_test)
auc <- performance(pr, measure = "auc")
print(auc)

fitted.results_gbm <- predict(bst,sparse_matrix_test)
fitted.results_gbm <- ifelse(fitted.results_gbm > 0.5,1,0)

misClasificError <- mean(fitted.results_gbm != test$Likelihood.to.recommend,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))
#"Accuracy 0.754474708171206
# ############################boost 1
# 
# bst_new <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)
# 
# 
# y_test <- test$Likelihood.to.recommend
# 
# x_test <-test%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
#                           ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
#                           ,Flight.Distance,Flight.cancelled) %>% data.matrix()
# pred <- predict(bst_new, x_test)
# pr <- prediction(pred, y_test)
# auc <- performance(pr, measure = "auc")
# print(auc)
# 
# 
# #################################3boost 2
bst2 <- xgboost(data = dtrain, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
y_test <- test$Likelihood.to.recommend

x_test <-test%>% select(Partner.Name,Age,Gender,Airline.Status,Price.Sensitivity,Loyalty,Type.of.Travel,Total.Freq.Flyer.Accts
                        ,Class,Flights.Per.Year,Departure.Delay.in.Minutes,Arrival.Delay.in.Minutes,Flight.time.in.minutes
                        ,Flight.Distance,Flight.cancelled) %>% data.matrix()
pred <- predict(bst2, x_test)
pr <- prediction(pred, y_test)
auc <- performance(pr, measure = "auc")
print(auc)

# dtest <- xgb.DMatrix(data = x_test, label=y_test)
# watchlist <- list(train=dtrain, test=dtest)
# 
# bst3 <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval.metric = "auc", eval.metric = "logloss", objective = "binary:logistic")
# bst4 <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")
# bst5 <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval.metric = "auc", eval.metric = "logloss", objective = "binary:logistic")
# head(importance)

      


####AUC



