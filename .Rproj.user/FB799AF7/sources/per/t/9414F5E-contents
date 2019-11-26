#author:yaxin yu
#Apply K-Means and Hierarchical clustering to cluster the data

set.seed(45)
#2.1 Load the preprocessed data file from Task 1 into a data frame.
ILPD_Origin <- readRDS("./Data/ilpd_preprocessed.Rda")
ILPD_DF <- ILPD_Origin[,c(-1,-2,-10,-11)]

#2.2 Rescale the values of every column to the range of (0,1). 
ILPD_Rescale <- apply(ILPD_DF,MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

#2.3 Cluster the data into 2 clusters (i.e. k = 2) using K-Means clustering using the default parameters for the function
#Plot the results of the clusters as a 2D plot where the x-axis is Alkphos and the y-axis is TP. 
ILPD_clusters <- kmeans(ILPD_Rescale, 2)
ILPD_COL <- (ILPD_clusters$cluster)
jpeg(filename="./Plot/task2.3_Rplot_k2.jpeg")
plot(ILPD_Rescale[,c("Alkphos", "TP")], main="k=2", col=ILPD_COL)
dev.off()

#2.4 Plot another 2D plot with the same dimensions above, but color the points according to the Class column. 
jpeg(filename="./Plot/task2.4_Rplot.jpeg")
plot(ILPD_Rescale[,c("Alkphos", "TP")], col = ILPD_Origin$Class)
dev.off() 

#2.6 Cluster the data into 3 clusters using K-Means clustering and plot all the clustering results. 
ILPD_clusters_3 <- kmeans(ILPD_Rescale, 3)
ILPD_COL_3 <- (ILPD_clusters_3$cluster)
jpeg(filename="./Plot/task2.6_Rplot_k3.jpeg")
plot(ILPD_Rescale[,c("Alkphos", "TP")], main="k=3", col=ILPD_COL_3)
dev.off()

#2.6 Cluster the data into 4 clusters using K-Means clustering and plot all the clustering results. 
ILPD_clusters_4 <- kmeans(ILPD_Rescale, 4)
ILPD_COL_4 <- (ILPD_clusters_4$cluster)
jpeg(filename="./Plot/task2.6_Rplot_k4.jpeg")
plot(ILPD_Rescale[,c("Alkphos", "TP")], main="k=4", col=ILPD_COL_4)
dev.off()

#2.6 Cluster the data into 5 clusters using K-Means clustering and plot all the clustering results. 
ILPD_clusters_5 <- kmeans(ILPD_Rescale, 5)
ILPD_COL_5 <- (ILPD_clusters_5$cluster)
jpeg(filename="./Plot/task2.6_Rplot_k5.jpeg")
plot(ILPD_Rescale[,c("Alkphos", "TP")], main="k=5", col=ILPD_COL_5)
dev.off()

#2.7 Compare the plots and Sum of Squared Error (SSE) obtained in the previous task and provide your comments on the quality of clustering. 
a <- "when k=2,SSE="
b <- "when k=3,SSE="
c <- "when k=4,SSE="
d <- "when k=5,SSE="
print(paste(a,ILPD_clusters$tot.withinss,sep=""))
print(paste(b,ILPD_clusters_3$tot.withinss,sep=""))
print(paste(c,ILPD_clusters_4$tot.withinss,sep=""))
print(paste(d,ILPD_clusters_5$tot.withinss,sep=""))

#2.8 Apply hierarchical clustering to the data using the function with default parameters and plot the corresponding dendrogram.
#default parameters
distance_HC <- dist(ILPD_Rescale)
HC <- hclust(distance_HC)
jpeg(filename="./Plot/task2.8_Dendrogram_default.jpeg")
plot(HC, hang = -1,main="default dendrogram")
dev.off()

#2.8 k=2
distance_HC <- dist(ILPD_Rescale)
HC <- hclust(distance_HC)
jpeg(filename="./Plot/task2.8_Dendrogram_k2.jpeg")
plot(HC, hang = -1,main="k2")
rect.hclust(HC,k=2)
dev.off()

#2.8 k=3
distance_HC <- dist(ILPD_Rescale)
HC <- hclust(distance_HC)
jpeg(filename="./Plot/task2.8_Dendrogram_k3.jpeg")
plot(HC, hang = -1,main="k3")
rect.hclust(HC,k=3)
dev.off()

#2.8 k=4
distance_HC <- dist(ILPD_Rescale)
HC <- hclust(distance_HC)
jpeg(filename="./Plot/task2.8_Dendrogram_k4.jpeg")
plot(HC, hang = -1,main="k4")
rect.hclust(HC,k=4)
dev.off()

#2.8 k=5
distance_HC <- dist(ILPD_Rescale)
HC <- hclust(distance_HC)
jpeg(filename="./Plot/task2.8_Dendrogram_k5.jpeg")
plot(HC, hang = -1,main="k5")
rect.hclust(HC,k=5)
dev.off()

#2.10 Try different agglomeration methods in hierarchical clustering
#2.10 MAX
distance_MAX <- dist(ILPD_Rescale)
HC_max <- hclust(distance_MAX, method="complete")
jpeg(filename="./Plot/task2.10_HC_max.jpeg")
plot(HC_max, hang=-1, main="max")
dev.off()

#2.10 MIN
distance_MIN <- dist(ILPD_Rescale)
HC_min <- hclust(distance_MIN, method="single")
jpeg(filename="./Plot/task2.10_HC_min.jpeg")
plot(HC_min, hang=-1, main="min")
dev.off()

#2.10 Average
distance_AV <- dist(ILPD_Rescale)
HC_av <- hclust(distance_AV, method="average")
jpeg(filename="./Plot/task2.10_HC_average.jpeg")
plot(HC_av, hang=-1, main="average")
dev.off()

