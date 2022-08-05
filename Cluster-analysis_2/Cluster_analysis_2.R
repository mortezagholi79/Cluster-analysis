library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
data= read.csv("~/prot.csv")
data1=data[,2:10]

k2 <- kmeans(data1, centers = 2, nstart = 25)
k3 <- kmeans(data1, centers = 3, nstart = 25)
k4 <- kmeans(data1, centers = 4, nstart = 25)
k5 <- kmeans(data1, centers = 5, nstart = 25)
# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data1) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = data1) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = data1) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = data1) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)



#Determining Optimal Clusters
set.seed(123)
# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(data1, k, nstart = 25 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 5
k.values <- 1:5
# extract wss for 2-5 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


######
set.seed(123)
fviz_nbclust(data1, kmeans, method = "wss")






#Similar to the elbow method
fviz_nbclust(data1, kmeans, method = "silhouette")




#Gap Statistic Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(data1, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
