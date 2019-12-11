#installing packages for clustering
install.packages("clustMixType")
df_new<-read.csv("E:\\Disk partition\\Syracuse ADS\\Syracuse ADS\\1st Semester\\IST 687 Introduction to Data Science\\project\\airline_customer_churner\\data_new2.csv")
df_new<-df_new[,-1]
library(clustMixType)
install.packages("wss")
library(wss)
data <- df_new
# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- na.omit(data) # to remove the rows with NA's

#K prototype clustering
wss <- sapply(1:k.max, 
              function(k){kproto(data, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

t1<-Sys.time()
#elbow curve hits descent at cluster 4 hence formulating relations with 4 clusters
no_of_clusters<-5
set.seed(12345)
fit_clust2 <- kproto(data, no_of_clusters, lambda = 1.804845  ,iter.max=60)
print("Time taken for k-prototypes")
t2<-Sys.time()
t2-t1

#viewing cluster plots
a<-clprofiles(fit_clust2, data)  


