library(readxl)
segment <- read.csv("C:/Users/mervo/OneDrive/Masaüstü/veri_segm.csv")


plot(segment$Store,segment$bill_amount)
plot(segment$Store,segment$geo_reg_code)
plot(segment$Store,segment$stock_tr)
segment[1]<- NULL

library(cluster)
library(clValid)
library(clusterCrit)
library(fpc)
library(clusterSim)

#for K=3
data_seg3 <- kmeans(segment,3,iter.max = 1000,nstart = 1)
data_seg3

clusplot(segment,data_seg3$cluster,color=T,shade=F, labels=2, lines=0)

distance<- dist(segment,"euclidean")
sil_index3<-silhouette(data_seg3$cluster,distance)
sil_index3
plot(sil_index3)
dunn_index_3<-dunn(distance,data_seg3$cluster,segment,method="euclidean")
dunn_index_3
segment_matrix<-as.matrix(segment)
Xie_Beni_index3<-intCriteria(segment_matrix,data_seg3$cluster,"Xie_Beni")
Calinski3 <- calinhara(segment, data_seg3$cluster)
Calinski3
DBI3 <- index.DB(segment, data_seg3$cluster, p=2)
DBI3
#for K=4
data_seg4 <- kmeans(segment,4,iter.max = 1000,nstart = 1)
data_seg4
clusplot(segment,data_seg4$cluster,color=T,shade=F, labels=2, lines=0)
distance<- dist(segment,"euclidean")
sil_index4<-silhouette(data_seg4$cluster,distance)
sil_index4
plot(sil_index4)
dunn_index_4<-dunn(distance,data_seg4$cluster,segment,method="euclidean")
dunn_index_4
segment_matrix<-as.matrix(segment)
Xie_Beni_index4<-intCriteria(segment_matrix,data_seg4$cluster,"Xie_Beni")
Calinski4 <- calinhara(segment, data_seg4$cluster)
Calinski4
DBI4 <- index.DB(segment, data_seg4$cluster, p=2)
DBI4
#for K=5
data_seg5 <- kmeans(segment,5,iter.max = 1000,nstart = 1)
data_seg5
clusplot(segment,data_seg5$cluster,color=T,shade=F, labels=2, lines=0)
distance<- dist(segment,"euclidean")
sil_index5<-silhouette(data_seg5$cluster,distance)
sil_index5
plot(sil_index5)
dunn_index_5<-dunn(distance,data_seg5$cluster,segment,method="euclidean")
dunn_index_5
segment_matrix<-as.matrix(segment)
Xie_Beni_index5<-intCriteria(segment_matrix,data_seg5$cluster,"Xie_Beni")
Calinski5 <- calinhara(segment, data_seg5$cluster)
Calinski5
DBI5 <- index.DB(segment, data_seg5$cluster, p=2)
DBI5
#for K=6
data_seg6 <- kmeans(segment,6,iter.max = 1000,nstart = 1)
data_seg6
clusplot(segment,data_seg6$cluster,color=T,shade=F, labels=2, lines=0)
distance<- dist(segment,"euclidean")
sil_index6<-silhouette(data_seg6$cluster,distance)
sil_index6
plot(sil_index6)
dunn_index_6<-dunn(distance,data_seg6$cluster,segment,method="euclidean")
dunn_index_6
Xie_Beni_index6<-intCriteria(segment_matrix,data_seg6$cluster,"Xie_Beni")
Calinski6 <- calinhara(segment, data_seg6$cluster)
Calinski6
DBI6 <- index.DB(segment, data_seg6$cluster, p=2)
DBI6
#for K=7
data_seg7 <- kmeans(segment,7,iter.max = 1000,nstart = 1)
data_seg7
clusplot(segment,data_seg7$cluster,color=T,shade=F, labels=2, lines=0)
distance<- dist(segment,"euclidean")
sil_index7<-silhouette(data_seg7$cluster,distance)
sil_index7
plot(sil_index7)
dunn_index_7<-dunn(distance,data_seg7$cluster,segment,method="euclidean")
dunn_index_7
Xie_Beni_index7<-intCriteria(segment_matrix,data_seg7$cluster,"Xie_Beni")
Calinski7<- calinhara(segment, data_seg7$cluster)
Calinski7
DBI7 <- index.DB(segment, data_seg7$cluster, p=2)
DBI7
 # grafik cizdirme
fviz_cluster(data_seg7, data = segment[, -5],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#3399cc", "#6600ff","#33ffcc","#993333"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
#digeri
fviz_cluster(data_seg7, geom = "point", data = segment) + ggtitle("k = 7")
#for K=8
data_seg8 <- kmeans(segment,8,iter.max = 1000,nstart = 1)
data_seg8

clusplot(segment,data_seg8$cluster,color=T,shade=F, labels=2, lines=0)

distance<- dist(segment,"euclidean")
sil_index8<-silhouette(data_seg8$cluster,distance)
sil_index8
plot(sil_index8)
dunn_index_8<-dunn(distance,data_seg8$cluster,segment,method="euclidean")
dunn_index_8
Xie_Beni_index8<-intCriteria(segment_matrix,data_seg8$cluster,"Xie_Beni")
Calinski8 <- calinhara(segment, data_seg8$cluster)
Calinski8
DBI8 <- index.DB(segment, data_seg8$cluster, p=2)
DBI8

#for K=9
data_seg9 <- kmeans(segment,9,iter.max = 1000,nstart = 1)
data_seg9

clusplot(segment,data_seg9$cluster,color=T,shade=F, labels=2, lines=0)

distance<- dist(segment,"euclidean")
sil_index9<-silhouette(data_seg9$cluster,distance)
sil_index9
plot(sil_index9)
dunn_index_9<-dunn(distance,data_seg9$cluster,segment,method="euclidean")
dunn_index_9
Xie_Beni_index9<-intCriteria(segment_matrix,data_seg9$cluster,"Xie_Beni")
Calinski9 <- calinhara(segment, data_seg9$cluster)
Calinski9
DBI9 <- index.DB(segment, data_seg9$cluster, p=2)
DBI9
# for K=10
data_seg10 <- kmeans(segment,10,iter.max = 1000,nstart = 1)
data_seg10
clusplot(segment,data_seg10$cluster,color=T,shade=F, labels=2, lines=0)
distance<- dist(segment,"euclidean")
sil_index10<-silhouette(data_seg10$cluster,distance)
dunn_index_10<-dunn(distance,data_seg10$cluster,segment,method="euclidean")
dunn_index_10
sil_index10
Xie_Beni_index10<-intCriteria(segment_matrix,data_seg10$cluster,"Xie_Beni")
Calinski10 <- calinhara(segment, data_seg10$cluster)
Calinski10
DBI10 <- index.DB(segment, data_seg10$cluster, p=2)
DBI10
# FOR K 11
data_seg11 <- kmeans(segment,11,iter.max = 1000,nstart = 1)
data_seg11

clusplot(segment,data_seg11$cluster,color=T,shade=F, labels=2, lines=0)

distance<- dist(segment,"euclidean")
sil_index11<-silhouette(data_seg11$cluster,distance)
Xie_Beni_index11<-intCriteria(segment_matrix,data_seg11$cluster,"Xie_Beni")
plot(sil_index11)

# k 12
data_seg12 <- kmeans(segment,12,iter.max = 1000,nstart = 1)
data_seg12
sil_index12<-silhouette(data_seg12$cluster,distance)
plot(sil_index12)

plot(sil_index10)
dunn_index_10<-dunn(distance,data_seg10$cluster,segment,method="euclidean")
dunn_index_10
dunn_index_3
dunn_index_4
dunn_index_5
dunn_index_6
dunn_index_7
dunn_index_8
dunn_index_9
dunn_index_10
Xie_Beni_index3
Xie_Beni_index4
Xie_Beni_index5
Xie_Beni_index6
Xie_Beni_index7
Xie_Beni_index8
Xie_Beni_index9
Xie_Beni_index10

Calinski3
Calinski4
Calinski5
Calinski6
Calinski7
Calinski8
Calinski9
Calinski10

DBI3
DBI4
DBI5
DBI6
DBI7
DBI8
DBI9
DBI10
library(factoextra)
fviz_cluster(list(data = segment_matrix, cluster=data_seg7$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())
fviz_cluster(data_seg7, data = segment[, -5],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#3399cc", "#6600ff","#33ffcc","#993333"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
#digeri
fviz_cluster(data_seg7, geom = "point", data = segment) + ggtitle("k = 7")
library(base)
set.seed(101)
k.max <- 11
data <- segment
wss <- sapply(3:k.max, 
              function(k){kmeans(segment, k, nstart=1,iter.max = 1000 )$tot.withinss})
plot(3:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

wss