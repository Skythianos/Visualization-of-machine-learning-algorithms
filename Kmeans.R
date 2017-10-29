library(reshape2)
library(data.table)
library(ggplot2)
library(dummies)
library(caret)

normalize <- function(x) {
  min <- min(x)
  max <- max(x)
  
  (x-min)/(max - min)
}

kmpca <- function(data, k, shape= NULL){
  set.seed(123)
  data1 <- data
  
  km <- kmeans(data1, k)
  
  data1 <- cbind(data1, dummy(as.factor(km$cluster)))

  pca <- prcomp(data1 ,center = T, scale = T)
  result <- as.data.frame(pca$x[,1:2])
  
  result$cluster <- km$cluster
  
  ggplot() + geom_point(data = result, aes(x = result[,1], y = result[,2], color = as.factor(cluster), shape = shape)) + scale_color_brewer(palette = "Set1")
}

kmpcp <- function(data, k) {
  set.seed(123)
  data1 <- data
  
  km <- kmeans(data1, k)
  centers <- as.data.frame(km$centers)
  
  ticks <- c(1,0.75,0.5,0.25, 0)
  ticks <- data.frame(ticks)
  numbers <- c()
  for (i in 1:ncol(data1)) {
    if(!is.factor(data1[,i])){
      numbers <- c(numbers, colnames(data1)[i])
      max <- max(data1[,i])
      ticks <- cbind(ticks, c(round(max), round(max*3/4), round(max/2), round(max/4), 0))
      data1[,i] <- normalize(data1[,i])
      centers[,i] <- normalize(centers[,i])
    }
  }
  names(ticks) <-c("id", numbers)
  ticks <- melt(ticks, c("id"))



  data1$id <- 1:nrow(data1)
  centers$id <- 1:nrow(centers)
  data1$cluster <- as.factor(km$cluster)
  melted_result <- melt(data1, c("id", "cluster"))
  melted_centers <- melt(centers, c("id"))

  p <- ggplot(melted_result) + geom_line(aes(x = variable, y = value, group = id, color = cluster)) + scale_color_brewer(palette = "Set1")
  p <- p + geom_line(data = melted_centers, aes(x = variable, y = value, group = id ))
  p <- p + geom_label(data = ticks, aes(x = variable, y = id, label = value) )

  
  p <- p + facet_grid(cluster ~.)
  
  print(p)
}
