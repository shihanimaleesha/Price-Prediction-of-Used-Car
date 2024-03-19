setwd("C:/Users/Shihani Dayaratne/OneDrive/Desktop/Assignment_DataMining/Data_Sets")
car_data<-read.csv("used_cars.csv",header = TRUE)
names(car_data)
head(car_data)
tail(car_data)
summary(car_data)
str(car_data)
nrow(car_data)
ncol(car_data)
dim(car_data)
install.packages("cluster") 
library(cluster) 
install.packages("factoextra")
library(factoextra)
install.packages("ggplot2")
library(ggplot2)
pairs(car_data[,-1])
plot(miles~price,data = car_data)
with(car_data,text(miles~price, labels = brand))
normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}
brand<-car_data[,1]
car_data_n<-car_data[,2:9] 
car_data_n<-as.data.frame(lapply(car_data_n,normalise))
car_data_n$brand<-brand
names(car_data_n)
car_data_n<-car_data_n[,c(1,9,7,2,3,4,5,6,8)]
head(car_data_n)
distance <- dist(car_data_n,method = "euclidean",)
print(distance)
print(distance,digits = 3)
fviz_dist(distance)
head(car_data_n)
rownames(car_data_n)<-make.unique(car_data_n$brand)
car_data_n$brand<-NULL 
head(car_data_n)
distance <- dist(car_data_n,method = "euclidean")
fviz_dist(distance)
kc<-kmeans(car_data[,-1],3)
kc
clusplot(car_data, kc$cluster, color=TRUE, shade=TRUE, lines=0) 
fviz_nbclust(car_data_n, kmeans, method = "wss")

car_data<-car_data[,c(1,3,9)]
head(car_data)
install.packages("reshape2") 
library(reshape2)
install.packages("factoextra")
library(factoextra)
car_pivot <- dcast(car_data, miles~price, sum, value.var ="miles")
head(car_pivot)
rownames(car_pivot) <- car_pivot[,1]
car_pivot[,1] <- NULL
head(car_pivot)
normalise <- function(df)
{ 
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}
brand<-rownames(car_pivot)
car_pivot_n<-as.data.frame(lapply(car_pivot,normalise))
install.packages("tidyr")
library(tidyr)
rownames(car_pivot_n) <- car_pivot$miles
tendency <- get_clust_tendency(car_pivot_n, 30, graph = TRUE)
tendency$hopkins_stat
install.packages("NbClust")
library(NbClust)
library(factoextra)
car_data_n_matrix <- as.matrix(car_data_n)
fviz_nbclust(car_data_n_matrix, kmeans, method = "wss")



library(factoextra)
library(cluster)
library(ggplot2)
# Determine the optimal number of clusters using the elbow method
fviz_nbclust(car_pivot_n, kmeans, method = "wss")

# Perform k-means clustering with 3 clusters
set.seed(123)
km.fit <- kmeans(car_pivot_n, 3, nstart = 20)

# Print the cluster assignments and cluster sizes
km.fit$cluster
km.fit$size

# Visualize the clustering results
fviz_cluster(km.fit, geom = "point", data = car_pivot_n)

library(factoextra)
library(cluster)
library(ggplot2)
set.seed(123)
k <- fviz_nbclust(car_data_n, kmeans, method = "wss")
k
kmeans_cluster <- kmeans(car_data_n, centers = 3, nstart = 25)
fviz_cluster(kmeans_cluster, data = car_data_n)

car_data_n <- as.matrix(car_data_n)
car_pivot_n <- as.data.frame(car_pivot_n)
fviz_cluster(km.fit, geom = "point", data = car_data_n, choose.vars = c("miles", "price"))
