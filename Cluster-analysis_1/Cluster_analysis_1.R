data=c(16.5,4.2,11.6,18.1,6.9,13.0,24.8,13.3,
       24.7,34.2,41.5,35.7,106,122,340,184,173,477,147,90,242,293,191,220,1112,982,808,1668,1534,
       1566,905,669,609,901,1368,1183,494,954,645,602,780,788)

jorm=matrix(data,nrow = 6,ncol = 7)

# distance matrix
distance=dist(jorm,upper = TRUE,diag = TRUE)
distance

# Hierarchical clustering using single Linkage 
hc_single <- hclust(d=distance, method = "single" )
plot(hc_single, cex = 0.6, hang = -1)


# Hierarchical clustering using Complete Linkage
hc_complete <- hclust(d=distance, method = "complete" )
plot(hc_complete, cex = 0.6, hang = -1)


# Hierarchical clustering using average Linkage
hc_average <- hclust(d=distance, method = "average" )
plot(hc_average, cex = 0.6, hang = -1)


# Hierarchical clustering using ward Linkage
hc_ward <- hclust(d=distance, method = "ward.D2" )
plot(hc_ward, cex = 0.6, hang = -1)



################# 16 City   #################
data=read.table("~/city.txt",header = T)
data2<- data[,-1]


# distance matrix
distance2=dist(data2,upper = TRUE,diag = TRUE)


# Hierarchical clustering using single Linkage 
hc_single <- hclust(d=distance2, method = "single" )
plot(hc_single, cex = 0.6, hang = -1)


# Hierarchical clustering using Complete Linkage
hc_complete <- hclust(d=distance2, method = "complete" )
plot(hc_complete, cex = 0.6, hang = -1)


# Hierarchical clustering using average Linkage
hc_average <- hclust(d=distance2, method = "average" )
plot(hc_average, cex = 0.6, hang = -1)


# Hierarchical clustering using ward Linkage
hc_ward <- hclust(d=distance2, method = "ward.D2" )
plot(hc_ward, cex = 0.6, hang = -1)

par(mfrow = c(2, 2))
plot(hc_single, cex = 0.6, hang = -1)
plot(hc_complete, cex = 0.6, hang = -1)
plot(hc_average, cex = 0.6, hang = -1)
plot(hc_ward, cex = 0.6, hang = -1)