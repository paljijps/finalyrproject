
rm(list = ls())
library(factoextra)
library(cluster)
library(ggthemes)
data = read.csv("fraud_cluster.csv")

rownames(data) = data$state
data = data[-1]
# data
data.scaled = scale(data)
# data.scaled
# data.scaled

dist.eucl = dist(data.scaled, method = "euclidean")
dist = as.matrix(dist.eucl)

# displot = fviz_dist(dist.eucl)

#kemans

wssplot = fviz_nbclust(data.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+theme_clean()

km.res <- kmeans(data.scaled, 2 , nstart = 25)
kmclus1 = data[names(km.res$cluster[km.res$cluster == 1]),]
kmclus2 = data[names(km.res$cluster[km.res$cluster == 2]),]

# mean vector of cluster 1
km.c1mean = colMeans(kmclus1)
km.c2mean = colMeans(kmclus2)
# aggregate(data, by=list(cluster=km.res$cluster), mean)

# dd <- cbind(data, cluster = km.res$cluster)
# dd

clusplot = fviz_cluster(km.res, data = data,
             palette = c( "blue4", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_clean() )



## Algglomerative
res.hc <- hclust(d = dist.eucl, method = "ward.D2")
# res.hc
fviz_dend(res.hc, cex = 0.5)
res.coph <- cophenetic(res.hc)
cor(dist.eucl, res.coph)


res.hc2 <- hclust(d = dist.eucl, method = "average")
res.hc2
plt = fviz_dend(res.hc2, cex = 0.5)
res.coph2 <- cophenetic(res.hc2)
cor(dist.eucl, res.coph2)


# better dendrogram

grp <- cutree(res.hc, k = 2)
head(grp, n = 5)
table(grp)
rownames(data)[grp == 1]


aglodend = fviz_dend(res.hc, k = 2, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c( "blue4", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE ,# Add rectangle around groups
          ggtheme = theme_clean()
)




aglocp = fviz_cluster(list(data = data, cluster = grp),
             palette = c("#2E9FDF", "blue4", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_clean())


#divisive clustering
res.diana <- diana(x = data, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean") 
                   
divdend = fviz_dend(res.diana, cex = 0.5) 

dianadend = fviz_dend(res.diana, k = 2, # Cut in four groups
                     cex = 0.5, # label size
                     k_colors = c( "blue4", "#FC4E07"),
                     color_labels_by_k = TRUE, # color labels by groups
                     rect = TRUE, # Add rectangle around groups
                     ggtheme = theme_clean()
)



# #agglomerative
res.agnes <- agnes(x = data, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)
# # fviz_dend(res.agnes, cex = 0.5)
# 
# res.agnes$order

ia = res.agnes$order[1:17]
ja = res.agnes$order[18:length(res.agnes$order)]

cameans1 = colMeans( data[ia,] )
cameans2 = colMeans( data[ja,] )

