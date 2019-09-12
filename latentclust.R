# Latent class clust
# aims to maximize the likelihood 
library(mclust)
library(NbClust)

projectdir <- "C:/rezawork/study/r-studio/edx-marketing analytics/week2-marketsegmentation"
setwd(projectdir)
seg_data <- read.csv(file = "SegmentationData.csv",row.names=1)
head(seg_data)

# standardization
std_seg_data <- scale(seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")])

# Find the optimal model
set.seed(1990)
mclustBIC(std_seg_data[,1:5],verbose=F)

# obtain more details about the optimal model and mclust
set.seed(1990)
lca_clust <- Mclust(std_seg_data[,1:5],verbose = FALSE)
summary(lca_clust)

# interpret each cluster and rename
lca_clusters <- lca_clust$classification
lca_clust_summary <- aggregate(std_seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")],by=list(lca_clusters),FUN=mean)
lca_clust_summary
lca_clusters<-factor(lca_clusters,levels = c(1,2),labels = c("Reliability LCA", "Comfort LCA"))

# Demographics
library(gmodels)
CrossTable(seg_data$MBA,lca_clusters,prop.chisq = FALSE, prop.r = T, prop.c = T, prop.t = F,chisq = T)

# Choice
CrossTable(lca_clusters,seg_data$Choice,prop.chisq = FALSE, prop.r = T, prop.c = T,prop.t = F,chisq = T)

# compare with Hierarchical Clustering
CrossTable(h_cluster,lca_clusters,prop.chisq = FALSE, prop.r = T, prop.c = T, prop.t = F,chisq = T)

# compare with K-Means Clustering
CrossTable(Kmean_Cluster,lca_clusters,prop.chisq = FALSE, prop.r = T, prop.c = T, prop.t = F,chisq = T)

 