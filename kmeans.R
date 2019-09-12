# kmeans clustring 
# specify in advance the number of clusters
projectdir <- "C:/rezawork/study/r-studio/edx-marketing analytics/week2-marketsegmentation"
setwd(projectdir)
seg_data <- read.csv(file = "SegmentationData.csv",row.names=1)
head(seg_data)

# standardization
std_seg_data <- scale(seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")])

#  algorithm works on our data for 3 segments
set.seed(1990)
car_Cluster3 <-kmeans(std_seg_data, 3, iter.max=100,nstart=100)
car_Cluster3

#  rename those clusters according to their characteristics
Kmean_Cluster<-factor(car_Cluster3$cluster,levels = c(1,2,3),labels = c("Perf. KM", "Comfort KM", "Appearance KM"))

# the optimal number of segments
install.packages("NbClust")
library(NbClust)
set.seed(1990)
NbClust(data=std_seg_data[,1:5], min.nc=3, max.nc=15, index="all", method="kmeans")

# Demographics
library(gmodels)
CrossTable(seg_data$MBA,Kmean_Cluster,prop.chisq = FALSE, prop.r = T, prop.c = T, prop.t = F,chisq = T)

# Choice
CrossTable(Kmean_Cluster,seg_data$Choice,prop.chisq = FALSE, prop.r = T, prop.c = T,prop.t = F,chisq = T)


 
