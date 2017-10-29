library(GGally)

#k - number of clusters
#x - variable on x-axis
#y - variable on y-axis
#psize - size of points 
#ksize - size of centroids
#hull alpha
kmeans_sp <- function(data, k, x, y, psize, ksize, hullfa) {
  set.seed(1)
  df <- data
  km <- kmeans(df, k)
  centroids <- as.data.frame(km$centers)
  
  centroids$cluster <- k + 1
  df$cluster <- km$cluster
  
  centroids$size <- 2
  df$size <- 1
  
  df1 <- df
  dfs <- list()
  for (i in 1:k) {
    dfsdf[df$cluster == i,])
  }
  cntrs <- centroids
  hull_cluster <- list()
  for (i in 1:k) {
    print(dfs[i])
    hull_cluster <- c(hull_cluster, chull(x = dfs[i][,x], y = dfs[i][,y] ))
  }
  df <- rbind(df, centroids)
  df$cluster <- as.factor(df$cluster)
  
  p <- ggplot() + 
    geom_point(data = df, aes_string(x = df[,x], df[,y], color = cluster, size = size)) + 
    scale_size_continuous(range = c(psize, ksize)) +
    theme_classic()
  for (i in 1:k) {
    p <- p + geom_polygon(data = dfs[i][hull_cluster[1],], aes_string(dfs[i][hull_cluster[1],x], dfs[i][hull_cluster[1],y], alpha = hullfa, fill = cntrs[i,]$cluster)) 
  }
  print(p)
}

kmeans_sp(iris[1:4], 3, x=3, y=4, psize = 2, ksize = 5,  hullfa = 0.3)

set.seed(1)
df <- iris
df$specnum <- as.numeric(iris$Species)
km <- kmeans(iris[,1:4], 3)
predicted <- km$cluster

centroids <- as.data.frame(km$centers)
centroids$Species <- "CLUSTER"
centroids$cluster <- 0:(nrow(centroids)-1)
centroids$cluster <- as.factor(centroids$cluster)
centroids$Species <- as.factor(centroids$Species)
centroids$size <- 2
centroids$id <- NULL
centroids$specnum <-99

result <- df
result$size <- 1
result$cluster <- as.data.frame(predicted)[,1]
result[result$cluster == 3, "cluster"] <- 4
result[result$cluster == 2, "cluster"] <- 3
result[result$cluster == 4, "cluster"] <- 2
result <- rbind(result, centroids)
result$cluster <- as.factor(result$cluster)
result$id = 1:nrow(result)
result$check <- as.numeric(result$specnum ==  result$cluster)
result[151:153,]$check <- 1
result[result$check == 0,]$check <- 15
result[result$check == 1,]$check <- result[result$check == 1,]$specnum
result$check <- as.factor(result$check)
result$size <- as.factor(result$size)

datalower <- result
datadiag <- result[1:150,]
dataupper <- result[1:150,]
cols <- 1:4
ltcolor <- ncol(result)
diagcolor <- 5
upcolor <- 5
ltsize <- 7
ltshape <- 5

plots <- list()
cntr <- 0
for (i in cols) {
  for (j in cols) {
    cntr <- cntr + 1 
    if(i == j){ #DIAGONAL
      plots[[cntr]] <- ggally_densityDiag(datadiag, mapping = ggplot2::aes_string(x = names(datalower)[j], alpha = 0.5, color = datadiag[,diagcolor])) + scale_fill_brewer(palette = "Set1")
    } else if (i < j){ # UPPER TRIANGLE
      plots[[cntr]] <- ggally_cor(dataupper, mapping = ggplot2::aes_string(x = names(dataupper)[j], y = names(dataupper)[i], color = names(dataupper)[upcolor])) + scale_color_brewer(palette = "Set1")
    } else{ #LOWER TRIANGLE
      plots[[cntr]] <- ggally_points(datalower,
                                     mapping = ggplot2::aes_string(
                                       x = names(datalower)[j],
                                       y = names(datalower)[i],
                                       color = names(datalower)[ltcolor],
                                       size = names(datalower)[ltsize],
                                       shape = names(datalower)[ltshape])) + 
        scale_size_manual(values = c(3,5))
        
    }
  }
}

p <- ggmatrix(plots, 4, 4, xAxisLabels = names(datalower)[cols], yAxisLabels = names(datalower)[cols]) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA))

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_color_manual(values = c("#e41a1c", "#377eb8","#4daf4a", "#000000", "#984ea3")) +
      scale_shape_manual(values = c(15, 16, 18, 17))
  }
}
print(p)
ggsave("~/Dokumentumok/Szakdoga/kepek/kmeans_splom_0.pdf",p, width = 10, height = 6, useDingbats=FALSE)

datalower1 <- datalower[1:150,]
cluster1 <- datalower1[datalower1$cluster == 1, ]
cluster2 <- datalower1[datalower1$cluster == 2, ]
cluster3 <- datalower1[datalower1$cluster == 3, ]
datalower2 <- datalower[151:153,]

hull_cluster1 <- chull(x = cluster1[,1], y = cluster1[,3] )
hull_cluster2 <- chull(x = cluster2[,1], y = cluster2[,3] )
hull_cluster3 <- chull(x = cluster3[,1], y = cluster3[,3] )

p <- ggplot() + 
  geom_polygon(data = cluster1[hull_cluster1,], aes(cluster1[hull_cluster1,1], cluster1[hull_cluster1,3], alpha = 0.3, fill = datalower2[1,]$cluster)) +
  geom_polygon(data = cluster2[hull_cluster2,], aes(cluster2[hull_cluster2,1], cluster2[hull_cluster2,3], alpha = 0.3, fill = datalower2[2,]$cluster)) +
  geom_polygon(data = cluster3[hull_cluster3,], aes(cluster3[hull_cluster3,1], cluster3[hull_cluster3,3], alpha = 0.3, fill = datalower2[3,]$cluster)) +
  geom_point(datalower, mapping = ggplot2::aes_string(
    x = names(datalower)[1],
    y = names(datalower)[3],
    color = names(datalower)[ltcolor],
    size = names(datalower)[ltsize],
    shape = names(datalower)[ltshape])) + 
  scale_alpha_continuous(range = c(0.3, 0.3)) +
  scale_color_manual(values = c("#e41a1c", "#377eb8","#4daf4a", "#000000", "#984ea3")) +
  scale_fill_manual(values = c("#e41a1c", "#377eb8","#4daf4a", "#000000", "#984ea3")) +
  scale_shape_manual(values = c(15, 16, 18, 17)) +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = names(datalower)[1], y = names(datalower)[3])
print(p)
ggsave("~/Dokumentumok/Szakdoga/kepek/kmeans_simple.pdf",p, width = 4, height = 4, useDingbats=FALSE)
 
ggplot() + geom_point(data = iris, aes(x = iris$Sepal.Length, y = iris$Petal.Length))

#############


set.seed(1)
pca <- prcomp(iris[1:4],center = T, scale = T)
km <- kmeans(as.data.frame(pca$x[,1:2]), 3)

pcadata <- data.frame(pca$x[,1:2], km$cluster, as.numeric(iris$Species)+3, iris$Species)
names(pcadata) <- c("x", "y", "Cluster", "Original_num", "Original")
pcadata[pcadata$Cluster == 2,]$Cluster <- 5
pcadata[pcadata$Cluster == 3,]$Cluster <- 6
pcadata[pcadata$Cluster == 1,]$Cluster <- 4
pcadata$check <- as.numeric(pcadata$Cluster == pcadata$Original_num)
pcadata[pcadata$check == 1,]$check <- pcadata[pcadata$check == 1,]$Cluster

centroids <- as.data.frame(km$centers)
names(centroids) <- c("x", "y")
centroids$Cluster <- 99
centroids$Original_num <- 99
centroids$Original <- "CENTROID"
centroids$Cluster <- 99
centroids$check <- 7

pcadata <- rbind(pcadata, centroids)
pcadata$check <- as.factor(pcadata$check)

pcadata1 <- pcadata[1:150,]
pcadata2 <- datalower[151:153,]
cluster1 <- pcadata1[pcadata1$Cluster == 4, ]
cluster2 <- pcadata1[pcadata1$Cluster == 5, ]
cluster3 <- pcadata1[pcadata1$Cluster == 6, ]

hull_cluster1 <- chull(x = cluster1[,1], y = cluster1[,2] )
hull_cluster2 <- chull(x = cluster2[,1], y = cluster2[,2] )
hull_cluster3 <- chull(x = cluster3[,1], y = cluster3[,2] )



p <- ggplot(pcadata) + geom_point(aes(x = x, y = y, color = check, size = pcadata$check==7, shape = Original)) + 
  geom_polygon(data = cluster1[hull_cluster1,], aes(cluster1[hull_cluster1,1], cluster1[hull_cluster1,2], alpha = 0.3, fill = pcadata2[1,]$cluster)) +
  geom_polygon(data = cluster2[hull_cluster2,], aes(cluster2[hull_cluster2,1], cluster2[hull_cluster2,2], alpha = 0.3, fill = pcadata2[3,]$cluster)) +
  geom_polygon(data = cluster3[hull_cluster3,], aes(cluster3[hull_cluster3,1], cluster3[hull_cluster3,2], alpha = 0.3, fill = pcadata2[2,]$cluster)) +
  scale_color_manual(values = c("#000000","#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_shape_manual(values = c(15, 16, 18, 17))+
  theme_classic() + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title =element_text(size=10)) +
  labs(x = "Principal component 1", y = "Principal component 2") + 
  scale_alpha_continuous(range = c(0.3, 0.3)) +
  scale_fill_manual(values = c("#e41a1c", "#4daf4a","#377eb8", "#000000", "#984ea3"))
print(p)
ggsave("~/Dokumentumok/Szakdoga/kepek/kmeans_pca.pdf",p, width = 3, height = 3, useDingbats=FALSE)











