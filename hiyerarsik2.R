library(factoextra)
library(cluster)
library(cluster)
library(clValid)
library(clusterCrit)
library(clusterSim)
library(fpc)
library(readxl)
segment <- read.csv("C:/Users/mervo/OneDrive/Masaüstü/veri_segm.csv")
df <- segment
df <- na.omit(df)
df <- scale(df)
head(df)
m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")
ac <- function(x) {
  agnes(df, method = x)$ac
}

clust <- agnes(df, method = "ward")
sapply(m, ac)
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
d <- dist(df, method = "euclidean")
final_clust <- hclust(d, method = "ward.D2" )
groups <- cutree(final_clust, k=10)
fviz_cluster(list(data = df, cluster = groups))
table(groups)
final_data <- cbind(segment, cluster = groups)
head(final_data)
final_data
aggregate(final_data, by=list(cluster=final_data$cluster), mean)
distance<- dist(df,"euclidean")
sil_index<-silhouette(final_data$cluster,distance)
sil_index
plot(sil_index)
dunn_index<-dunn(distance,final_data$cluster,segment,method="euclidean")
dunn_index
segment_matrix<-as.matrix(df)
Xie_Beni_index<-intCriteria(segment_matrix,final_data$cluster,"Xie_Beni")
Xie_Beni_index
calisnki <- calinhara(df, final_data$cluster)
calisnki
DBI <- index.DB(df, final_data$cluster, p=2)
DBI