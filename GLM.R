library(ggplot2)
library(dummies)
library(caret)
library(reshape2)
library(tabplot)
library(GGally)
library(RColorBrewer)

normalize <- function(x) {
  min <- min(x)
  max <- max(x)
  
  (x-min)/(max - min)
}

glmpca <- function(x, y, object) {
  set.seed(1)
  data1 <- as.data.frame(x)
  data2 <- as.data.frame(x)

  predicted <- predict(object, newdata=x, type="response")
  predicted <- (predicted > 0.5)
  predicted <- as.data.frame(predicted)

  data2$predicted <- rep(levels(y)[1], nrow(data2))
  data2[predicted[,1],]$predicted <- rep(levels(y)[2], sum(as.numeric(predicted[,1])))
  data2$predicted <- as.factor(data2$predicted)
  data2$original <- y

  data2$check <- data2$predicted == data2$original
  data2$display <- as.character(data2$predicted)
  data2[!data2$check,]$display <- "Wrong prediction"
  data2$display <- as.factor(data2$display)
  
  print(sum(as.numeric(data2$check))/nrow(data2))
  
  data1$predicted <- data2$predicted
  
  cols <- list()
  for (i in 1:ncol(data1)) {
    if(is.factor(data1[,i])){
      cols[[i]] <- as.data.frame(dummy(data1[,i]))
    } else{
      cols[[i]] <- data1[,i]
    }
  }
  data1 <- data.frame(cols)
  
  pca <- prcomp(data1 ,center = T, scale = T)
  
  result <- as.data.frame(pca$x[,1:2])
  result <- cbind(result, data2$display, data2$original)
  names(result) <- c("x", "y", "color", "Original")
  result$color <- as.factor(result$color)
  
  print(ggplot(result) + geom_point(aes(x = x, y = y, color = color, shape = Original)) + theme_classic() + 
          theme(axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title =element_text(size=10)) +
          labs(x = "Principal component 1", y = "Principal component 2") +
          scale_color_brewer(palette = "Set1")
        
  ) 
}

glmparcord <- function(x, y, object, alpha, facet = T, ncol = NULL){
  if(is.null(alpha)){
    alpha = 0.02
  }
  if(is.null(ncol)){
    ncol <- ncol(x)
  }
  data1 <- x
  data2 <- y
  data3 <- cbind(data1, data2)
  data <- cbind(data1, data2)
  
  ## calculate importance
  col_order <- filterVarImp(data1, as.numeric(data2))
  col_order <- rownames(col_order[order(col_order$Overall, decreasing = TRUE), ,drop = FALSE])
  
  result <- data[,col_order[1:ncol]]
  
  predicted <- predict(object, newdata=x, type="response")
  predicted <- (predicted > 0.5)
  predicted <- as.data.frame(predicted)
  
  ##normalization and label collection
  ticks <- c(1,0.75,0.5,0.25, 0)
  ticks <- data.frame(ticks)
  ticksc <- data.frame(c("col"),c(1), c("a"))
  ticksc[,1] <- as.character(ticksc[,1])
  ticksc[,2] <- as.numeric(ticksc[,2])
  ticksc[,3] <- as.character(ticksc[,3])
  numbers <- c()
  for (i in 1:ncol(result)) {
    if(!is.factor(result[,i]) & !is.character(result[,i])){
      numbers <- c(numbers, colnames(result)[i])
      max <- max(result[,i])
      min <- min(result[,i])
      ticks <- cbind(ticks, c(round(max), round(max*3/4), round(max/2), round(max/4), 0))
      result[,i] <- normalize(result[,i])
    } else{
      if(length(levels(result[,i])) < 9){
        n <- length(levels(result[,i]))
        name <- names(result)[i]
        lvl <- levels(result[,i])
        for (j in 1:n) {
          ticksc <- rbind(ticksc, c(name, normalize2(j,1,n), lvl[j] ))
        }
      }else{
        print("Categorical variable with more than 9 levels. Labels wont be displayed.")
      }
      levels(result[,i]) <- 1:length(levels(result[,i]))
      result[,i] <- as.numeric(result[,i])
      result[,i] <- normalize(result[,i])
      result[,i] <- result[,i] + runif(nrow(result), -0.025, 0.025)
    }
  }
  names(ticks) <-c("id", numbers)
  ticks <- melt(ticks, c("id"))
  ticksc <- tail(ticksc, nrow(ticksc)-1)
  
  predicted <- predicted[,1]
  ## Mark where the predicted value equals to the original
  result$check <- as.numeric(y) == (as.numeric(predicted)+1)
  result$id <- 1:nrow(result)

  result$predicted <- y
  result$color <- as.numeric(predicted)
  result$x <- as.numeric(predicted)
  for (i in levels(y)) {
    tmp <-(result$check == F & result$predicted == i)
    if(nrow(result[tmp,]) > 0){
      result[tmp,]$color <- length(levels(y))+1
    }
  }

  colors <- brewer.pal(length(levels(y))+1, "Set1")
  colors <- c(colors[2:length(colors)], colors[1])
  

  result$predicted <- as.factor(result$predicted)
  result$color <- as.factor(result$color)
  result$x <- as.factor(result$x)
  levels(result$x) <- levels(as.factor(predicted))
  levels(result$predicted) <- levels(as.factor(predicted))

  vlines <- col_order
  vlines <- as.data.frame(vlines)
  
  if(facet){
    labels <- c()
    for (i in 1:length(levels(y))) {
      tmp <- round(1 - nrow(result[as.numeric(result$predicted) == i & result$check == T,]) / nrow(result[as.numeric(result$predicted) == i,]), digits = 4)
      labels <- c(labels, paste(levels(as.factor(predicted))[i], "(error:", tmp, ")"))
    } 
    labels <- c(labels, round(1 - nrow(result[result$check == T,]) / nrow(result),4))
    levels(result$color) <- labels
    
  }else{
    levels(result$color) <- c(levels(y), "Misclassified")
  }
  
  melted_result <- melt(result, id.vars = c("id","check", "predicted","color","x"))
  
  names(ticksc) <- c("r", "y", "labels")
  ticksc$y <- as.numeric(ticksc$y)
  p <- ggplot() + geom_line(data = subset(melted_result, ), aes(x = variable, y = value, group = id, color = color, alpha = "a")) + 
    geom_vline(data = vlines, xintercept = as.numeric(vlines[,1])) +
    scale_color_manual(values = colors, name="Prediction")
  p <- p + geom_label(data = ticks, aes(x = variable, y = id, label = value) )
  p <- p + geom_label(data = ticksc, aes(x = r, y = y, label = labels) )
  
  if(facet){
    p <- p + facet_grid(x ~ predicted) + labs(x = "Actual value", y = "Predicted value")
  }
  p <- p + theme_classic() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(legend.position="bottom", panel.border = element_rect(colour = "black", fill=NA)) +
    scale_alpha_discrete(range = c(alpha,alpha), guide = F)
  
  print(p)
}

glmparcordFacet <- function(x, y, object, alpha = 0.2, ncols = NULL) {
  data1 <- as.data.frame(x)
  data2 <- as.data.frame(y)
  data3 <- cbind(data1, data2)
  if(is.null(ncols)){
    ncols <- ncol(data1)
  }
  
  ## calculate importance
  col_order <- filterVarImp(data3[,1:ncol(data1)], as.numeric(data3[,ncol(data1)+1]))
  col_order <- rownames(col_order[order(col_order$Overall, decreasing = TRUE), ,drop = FALSE])
  col_order <- c(col_order[1:ncols])
  
  result <- data3[,col_order]

  predicted <- predict(object, newdata=x, type="response")
  predicted <- (predicted > 0.5)
  predicted <- as.data.frame(predicted)
  
  result$predicted <- rep(levels(y)[1], nrow(result))
  result[predicted[,1],]$predicted <- rep(levels(y)[2], sum(as.numeric(predicted[,1])))
  result$predicted <- as.factor(result$predicted)


  ticks <- c(1,0.75,0.5,0.25, 0)
  ticks <- data.frame(ticks)
  ticksc <- data.frame(c("col"),c(1), c("a"))
  ticksc[,1] <- as.character(ticksc[,1])
  ticksc[,2] <- as.numeric(ticksc[,2])
  ticksc[,3] <- as.character(ticksc[,3])
  numbers <- c()
  for (i in 1:length(col_order)) {
    if(!is.factor(result[,i]) & !is.character(result[,i])){
      numbers <- c(numbers, colnames(result)[i])
      max <- max(result[,i])
      ticks <- cbind(ticks, c(round(max), round(max*3/4), round(max/2), round(max/4), 0))
      result[,i] <- normalize(result[,i])
    } else{
      if(length(levels(result[,i])) < 9){
        n <- length(levels(result[,i]))
        name <- names(result)[i]
        lvl <- levels(result[,i])
        for (j in 1:n) {
          ticksc <- rbind(ticksc, c(name, normalize2(j,1,n), lvl[j] ))
        }
      }
      levels(result[,i]) <- 1:length(levels(result[,i]))
      result[,i] <- as.numeric(result[,i])
      result[,i] <- normalize(result[,i])
    }
  }
  names(ticks) <-c("id", numbers)
  ticks <- melt(ticks, c("id"))
  ticksc <- tail(ticksc, nrow(ticksc)-1)

  ## Mark where the predicted value equals to the original
  result$check <- y == result$predicted
  result$id <- 1:nrow(result)

  
  ## TODO: eliminate magic
  lvls <- levels(result$predicted)
  result$predicted <- as.factor(as.numeric(result$predicted))
  result$color <- as.numeric(result$predicted)
  result$x <- as.numeric(result$predicted)
  
  tmp <- (result$check == F & result$predicted == "2")
  result[result$check == F & result$predicted == "1",]$color <- 3
  result[result$check == F & result$predicted == "1",]$x <- 1
  result[result$check == F & result$predicted == "1",]$predicted <- "2"
  
  result[tmp,]$predicted <- "1"
  result[tmp,]$color <- 3
  result[tmp,]$x <- 2
  result$color <- as.factor(result$color)
  result$x <- as.factor(result$x)
  levels(result$x) <- lvls
  levels(result$predicted) <- lvls
  
  vlines <- names(data1)
  vlines <- as.data.frame(vlines)
  
  
  ## calculating errors 
  yes_error <- 1 - nrow(result[result$predicted == T & result$check == T,]) / nrow(result[result$predicted == T,])
  no_error <- 1 - nrow(result[result$predicted == F & result$check == T,]) / nrow(result[result$predicted == F,])
  error <- 1 - nrow(result[result$check == T,]) / nrow(result)
  levels(result$color) <- c(paste("No (error:", no_error, ")"), paste("Yes (error:", yes_error, ")"), paste("Incorrect (overall error:", error, ")") )
  
  melted_result <- melt(result, id.vars = c("id","check", "predicted","color","x"))
  
  names(ticksc) <- c("r", "y", "labels")
  ticksc$y <- as.numeric(ticksc$y)
  p <- ggplot(melted_result) + geom_line(aes(x = variable, y = value, group = id, color = color, alpha = "a")) +
    geom_vline(data = vlines, xintercept = as.numeric(vlines[,1])) +
    scale_color_manual(values = c("#377eb8", "#4daf4a", "#e41a1c"))
  p <- p + geom_label(data = ticks, aes(x = variable, y = id, label = value) )
  p <- p + geom_label(data = ticksc, aes(x = r, y = y, label = labels) )
  p <- p + facet_grid(x ~ predicted) + scale_alpha_discrete(range = c(alpha,alpha), guide = F) +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + theme(legend.position="bottom", panel.border = element_rect(colour = "black", fill=NA)) 
  
  print(p)
}

glmtable <- function(x, y, object, predicted_first = T){
  set.seed(1)
  data1 <- as.data.frame(x)
  data2 <- as.data.frame(y)
  data3 <- cbind(data1, data2)
  
  ## calculate importance
  col_order <- filterVarImp(data3[,1:ncol(data1)], as.numeric(data3[,ncol(data1)+1]))
  col_order <- rownames(col_order[order(col_order$Overall, decreasing = TRUE), ,drop = FALSE])

  predicted <- predict(object, newdata=x, type="response")
  predicted <- (predicted > 0.5)
  predicted <- as.data.frame(predicted)
  
  result <- data1[,col_order]
  result$predicted <- rep(levels(y)[1], nrow(result))
  result[predicted[,1],]$predicted <- rep(levels(y)[2], sum(as.numeric(predicted[,1])))
  result$predicted <- as.factor(result$predicted)
  
  result$correct <- as.factor(data2[,1] == result$predicted)
  
  if(predicted_first){
    result <- result[,c("predicted", "correct", col_order)]
  } else{
    result <- result[,c("correct","predicted", col_order)]
  }

  tableplot(result)
}

glmsplom <- function(x, y, object, ncols){
  data1 <- as.data.frame(x)
  data2 <- as.data.frame(x)
  data4 <- as.data.frame(y)
  data3 <- cbind(data1, data4)
  
  ## calculate importance
  col_order <- filterVarImp(data3[,1:ncol(data1)], as.numeric(data3[,ncol(data1)+1]))
  col_order <- rownames(col_order[order(col_order$Overall, decreasing = TRUE), ,drop = FALSE])
  

  predicted <- predict(object, newdata=x, type="response")
  predicted <- (predicted > 0.5)
  predicted <- as.data.frame(predicted)
  
  data2$predicted <- rep(levels(y)[1], nrow(data2))
  data2[predicted[,1],]$predicted <- rep(levels(y)[2], sum(as.numeric(predicted[,1])))
  data2$predicted <- as.factor(data2$predicted)
  
  
  data2 <- data2[,c(col_order[1:ncols],"predicted")]
  data2$original <- data4[,1]
  data2$check <- as.character(data2$predicted) == as.character(data2$original)
  data2$display <- as.character(data2$predicted)
  data2[!data2$check,]$display <- "Wrong prediction"
  data2$display <- as.factor(data2$display)
  
  print(sum(as.numeric(data2$check))/nrow(data2))
  
  source("/home/user/Dokumentumok/Szakdoga/Titanic_analysis/Mosaicplot.R")
  plots <- list()
  cntr <- 0
  for (i in 1:ncols) {
    for (j in 1:ncols) {
      cntr <- cntr + 1 
      if(i == j){ #DIAGONAL
        if(is.factor(data2[,i])){
          plots[[cntr]] <- Mosaicplot(data2[,i], data2[,i], "", "", fill = data2$display, F)
        } else {
          plots[[cntr]] <- ggally_densityDiag(data2, mapping = ggplot2::aes_string(x = names(data2)[i], alpha = 0.1, color = "display")) + scale_color_brewer(palette = "Set1")
        }
      } else if (i < j){ # UPPER TRIANGLE
        if(is.factor(data2[,i]) | is.factor(data2[,j])){
          plots[[cntr]] <- ggally_blank()
        } else{
          plots[[cntr]] <- ggally_blank()       
          #plots[[cntr]] <- ggally_cor(data2, mapping = ggplot2::aes_string(x = names(data2)[j], y = names(data2)[i], color = "display")) + scale_color_brewer(palette = "Set1")  
        }
      } else{ #LOWER TRIANGLE
        if(is.factor(data2[,i]) | is.factor(data2[,j])){
          print
          plots[[cntr]] <- (ggplot() + geom_jitter(data = data2, ggplot2::aes_string(x = names(data2)[j], y = names(data2)[i], color = "display", shape = "original"))) + scale_color_brewer(palette = "Set1")  
        } else{
          plots[[cntr]] <- ggally_points(data2,
                                         mapping = ggplot2::aes_string(
                                           x = names(data2)[j],
                                           y = names(data2)[i],
                                           color = "display")) + scale_color_brewer(palette = "Set1")  
        }
      }
    }
  }
  
  p <- ggmatrix(plots, ncols, ncols, xAxisLabels = names(data2)[1:ncols], yAxisLabels = names(data2)[1:ncols]) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA))
  print(p)
  
}
