# kmeans clustring 
# specify in advance the number of clusters
setwd(projectdir)
seg_data <- read.csv(file = "SegmentationData.csv",row.names=1)
str(seg_data)


# remove any missing
seg_data <- na.omit(seg_data)


# standardization
std_seg_data <- scale(seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")])


# matrix from factoextra 
library(factoextra)
dist <- get_dist (std_seg_data)
fviz_dist(dist, gradient = list(low="#00AFBB", mid = "white", high = "#FC4E07"))


# the optimal number of segments
library(NbClust)
set.seed(1990)
NbClust(data=std_seg_data[,1:5], min.nc=3, max.nc=15, index="all", method="kmeans")


#  algorithm works on our data for 3 segments
set.seed(1990)
car_Cluster3 <-kmeans(std_seg_data, 3, iter.max=100,nstart=100)
car_Cluster3


# allocating membors to cluster
seg_data$cluster <- car_Cluster3$cluster


#  rename those clusters according to their characteristics
Kmean_Cluster<-factor(car_Cluster3$cluster,levels = c(1,2,3),labels = c("Perf. KM", "Comfort KM", "Appearance KM"))
seg_data$clustername <- Kmean_Cluster




# Demographics
library(gmodels)
CrossTable(seg_data$MBA,Kmean_Cluster,prop.chisq = FALSE, prop.r = T, prop.c = T, prop.t = F,chisq = T)

# Choice
CrossTable(Kmean_Cluster,seg_data$Choice,prop.chisq = FALSE, prop.r = T, prop.c = T,prop.t = F,chisq = T)


 
