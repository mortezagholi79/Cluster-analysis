library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization


Data<- read.table("~/4Proj.txt", header = T)
Data<-Data[,-1]

Data <- na.omit(Data)

Data <- scale(Data)


k2 <- kmeans(Data, centers = 2, nstart = 11)
str(k2)

k2
fviz_cluster(k2, data = Data)


k3 <- kmeans(Data, centers = 3, nstart = 11)
k4 <- kmeans(Data, centers = 4, nstart = 11)
k5 <- kmeans(Data, centers = 5, nstart = 11)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = Data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = Data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = Data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = Data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


set.seed(555)
fviz_nbclust(Data, kmeans, method = "wss")


fviz_nbclust(Data, kmeans, method = "silhouette")


# compute gap statistic
set.seed(555)
gap_stat <- clusGap(Data, FUN = kmeans, nstart = 11,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")


fviz_gap_stat(gap_stat)


# Compute k-means clustering with k = 4
set.seed(555)
final <- kmeans(Data, 4, nstart = 11)
print(final)


fviz_cluster(final, data = Data)
