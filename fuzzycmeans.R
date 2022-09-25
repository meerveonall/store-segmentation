library(readxl)
segment <- read.csv("C:/Users/mervo/OneDrive/Masaüstü/veri_segm.csv")
segment[1]<- NULL
library(ppclust)
library(cluster)
library(clValid)
library(clusterCrit)
library(e1071)
library(clusterSim)
library(fpc)
#for 3 cluster
data_C3<-cmeans(segment,3,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C3

distance<- dist(segment, method="euclidean")
sil_index_C3<-silhouette(data_C3$cluster,distance)
sil_index_C3
plot(sil_index_C3)
dunn_index_C3<-dunn(distance,data_C3$cluster,segment,method="euclidean")
dunn_index_C3
segment_matrix<-as.matrix(segment)
as.numeric(segment_matrix)
Xie_Beni_index3<-intCriteria(segment_matrix,data_C3$cluster,"Xie_Beni")
Xie_Beni_index3
DBI3 <- index.DB(segment, data_C3$cluster, p=2)
DBI3
Calinski3 <- calinhara(segment, data_C3$cluster)
Calinski3
#for 4 cluster
data_C4<-cmeans(segment,4,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C4
distance<- dist(segment, method="euclidean")
sil_index_C4<-silhouette(data_C4$cluster,distance)
sil_index_C4
dunn_index_C4<-dunn(distance,data_C4$cluster,segment,method="euclidean")
dunn_index_C4
plot(sil_index_C4)
Xie_Beni_index4<-intCriteria(segment_matrix,data_C4$cluster,"Xie_Beni")
Xie_Beni_index4
DBI4 <- index.DB(segment, data_C4$cluster, p=2)
DBI4
Calinski4 <- calinhara(segment, data_C4$cluster)
Calinski4
#for 5 cluster
data_C5<-cmeans(segment,5,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C5
distance<- dist(segment, method="euclidean")
sil_index_C5<-silhouette(data_C5$cluster,distance)
sil_index_C5
plot(sil_index_C5)
dunn_index_C5<-dunn(distance,data_C5$cluster,segment,method="euclidean")
dunn_index_C5
Xie_Beni_index5<-intCriteria(segment_matrix,data_C5$cluster,"Xie_Beni")
Xie_Beni_index5
DBI5 <- index.DB(segment, data_C5$cluster, p=2)
DBI5
Calinski5 <- calinhara(segment, data_C5$cluster)
Calinski5
#for 6 cluster
data_C6<-cmeans(segment,6,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C6
distance<- dist(segment, method="euclidean")
sil_index_C6<-silhouette(data_C6$cluster,distance)
sil_index_C6
plot(sil_index_C6)
dunn_index_C6<-dunn(distance,data_C6$cluster,segment,method="euclidean")
dunn_index_C6
Xie_Beni_index6<-intCriteria(segment_matrix,data_C6$cluster,"Xie_Beni")
Xie_Beni_index6
DBI6 <- index.DB(segment, data_C6$cluster, p=2)
DBI6
Calinski6 <- calinhara(segment, data_C6$cluster)
Calinski6
#for 7 cluster
data_C7<-cmeans(segment,7,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C7
distance<- dist(segment, method="euclidean")
sil_index_C7<-silhouette(data_C7$cluster,distance)
sil_index_C7
plot(sil_index_C7)
dunn_index_C7<-dunn(distance,data_C7$cluster,segment,method="euclidean")
dunn_index_C7
Xie_Beni_index7<-intCriteria(segment_matrix,data_C7$cluster,"Xie_Beni")
Xie_Beni_index7
DBI7 <- index.DB(segment, data_C7$cluster, p=2)
DBI7
Calinski7 <- calinhara(segment, data_C7$cluster)
Calinski
#for 8 cluster
data_C8<-cmeans(segment,8,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C8
plot(data_C8)
distance<- dist(segment, method="euclidean")
sil_index_C8<-silhouette(data_C8$cluster,distance)
sil_index_C8
plot(sil_index_C8)
dunn_index_C8<-dunn(distance,data_C8$cluster,segment,method="euclidean")
dunn_index_C8
Xie_Beni_index8<-intCriteria(segment_matrix,data_C8$cluster,"Xie_Beni")
Xie_Beni_index8
DBI8 <- index.DB(segment, data_C8$cluster, p=2)
DBI8
Calinski8 <- calinhara(segment, data_C8$cluster)
Calinski8
#for 9 cluster
data_C9<-cmeans(segment,9,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C9
distance<- dist(segment, method="euclidean")
sil_index_C9<-silhouette(data_C9$cluster,distance)
sil_index_C9
plot(sil_index_C9)
dunn_index_C9<-dunn(distance,data_C9$cluster,segment,method="euclidean")
dunn_index_C9
Xie_Beni_index9<-intCriteria(segment_matrix,data_C9$cluster,"Xie_Beni")
Xie_Beni_index9
DBI9 <- index.DB(segment, data_C9$cluster, p=2)
DBI9
Calinski9 <- calinhara(segment, data_C9$cluster)
Calinski9
#for 10 cluster
data_C10<-cmeans(segment,10,iter.max = 1000,dist = "euclidean",verbose = FALSE,method = "cmeans")
data_C10
distance<- dist(segment, method="euclidean")
sil_index_C10<-silhouette(data_C10$cluster,distance)
sil_index_C10
plot(sil_index_C10)
dunn_index_C10<-dunn(distance,data_C10$cluster,segment,method="euclidean")
dunn_index_C10
Xie_Beni_index10<-intCriteria(segment_matrix,data_C10$cluster,"Xie_Beni")
Xie_Beni_index10
DBI10 <- index.DB(segment, data_C10$cluster, p=2)
DBI10
Calinski10 <- calinhara(segment, data_C10$cluster)
Calinski10
dunn_index_C3
dunn_index_C4
dunn_index_C5
dunn_index_C6
dunn_index_C7
dunn_index_C8
dunn_index_C9
dunn_index_C10


Xie_Beni_index2
Xie_Beni_index3
Xie_Beni_index4
Xie_Beni_index5
Xie_Beni_index6
Xie_Beni_index7
Xie_Beni_index8
Xie_Beni_index9
Xie_Beni_index10

DBI3
DBI4
DBI5
DBI6
DBI7
DBI8
DBI9
DBI10

Calisnki3
Calisnki4
Calisnki5
Calisnki6
Calisnki7
Calisnki8
Calisnki9
Calisnki10
library(base)


#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- as.matrix(segment)
wss <- sapply(3:k.max, 
              function(k){cmeans(data, k,iter.max = 1000, verbose = FALSE,
                                 dist = "euclidean", method = "cmeans") $tot.withinss})
wss
plot(3:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



fviz_cluster(data_C8, geom = "point", data = segment) + ggtitle("k = 8")
res.fcm2 <- ppclust2(data_C8, "kmeans")
factoextra <- fviz_cluster(res.fcm2, data = segment, 
                         ellipse.type = "convex",
                         palette = "jco",
                         repel = TRUE)
