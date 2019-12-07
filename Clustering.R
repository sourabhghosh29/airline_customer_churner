install.packages("clustMixType")
df_new<-read.csv("C:\\Users\\Krishna\\Desktop\\data.csv")
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
wss <- sapply(1:k.max, 
              function(k){kproto(data, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

t1<-Sys.time()
no_of_clusters<-5
set.seed(12345)
fit_clust2 <- kproto(data, no_of_clusters, lambda = 1.804845  ,iter.max=60)
print("Time taken for k-prototypes")
t2<-Sys.time()
t2-t1

a<-clprofiles(fit_clust2, data)  
datawithclusters<-cbind(data,fit_clust2$cluster)
write.csv(datawithclusters,"C:\\Users\\Krishna\\Desktop\\datawithclusters.csv")

