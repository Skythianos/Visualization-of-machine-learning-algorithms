library(randomForest)
library(ggplot2)
library(reshape2)
library(data.table)

normalize <- function(x) {
  min <- min(x)
  max <- max(x)
  
  (x-min)/(max - min)
}


#Random forest parallel coordinates plot.
pcordPlot <- function(data, object, naxes){
  if(do.call(ncol,c(object$call$x)) < naxes || naxes < 2)  return("NAXES NOT CORRECT")
  rf_col_names <- (do.call(names, c(object$call$x)))
  
  rearrange_cols <- names(sort(object$importanceSD[,ncol(object$importanceSD)],decreasing = T))
  rf_cols <- data[c(rearrange_cols)]
  rf_cols <- rf_cols[,1:naxes]
  
  rf_cols[,ncol(rf_cols)+1] <- object$predicted
  colnames(rf_cols)[ncol(rf_cols)] <- "predicted"
  rf_cols$id <- 1:nrow(rf_cols)

  melted_data <- melt.data.table(data.table(rf_cols), id.vars = c('id', 'predicted'))
  
  print(ggplot(melted_data) + geom_line(aes(x = variable, y = value, group = id, color = predicted)) + scale_color_brewer(palette = "Set1"))
}

#Random forest parallel coordinates plot.
pcordPlotFacet <- function(data, object, naxes){
  if(do.call(ncol,c(object$call$x)) < naxes || naxes < 2)  return("NAXES NOT CORRECT")
  rf_col_names <- (do.call(names, c(object$call$x)))
  rearrange_cols <- names(sort(object$importanceSD[,ncol(object$importanceSD)],decreasing = T))
  result <- data[c(rearrange_cols)]
  result <- result[,1:naxes]

  for (i in 1:ncol(result)) {
    if(!is.factor(result[,i]) & !is.character(result[,i])){
      result[,i] <- normalize(result[,i])
    } else{
      levels(result[,i]) <- 1:length(levels(result[,i]))
      result[,i] <- as.numeric(result[,i])
      result[,i] <- normalize(result[,i])
    }
  }
  
  predicted <- object$predicted
  d <- (do.call(as.data.frame, c(object$call$y)))
  result$check <- d[,1] == predicted
  result$id <- 1:nrow(result)
  result$predicted <- predicted
  result$color <- predicted
  wrong <- nlevels(result$color)
  result$color <- as.numeric(result$color)
  result[result$check == F,]$color <- wrong + 1
  result$color <- as.factor(result$color)
  
  yes_error <- 1 - nrow(result[result$predicted == "Yes" & result$check == T,]) / nrow(result[result$predicted == "Yes",])
  no_error <- 1 - nrow(result[result$predicted == "No" & result$check == T,]) / nrow(result[result$predicted == "No",])
  error <- 1 - nrow(result[result$check == T,]) / nrow(result)
  
  levels(result$color) <- c(paste("No (Error rate:", no_error, ")"), paste("Yes (Error rate:", yes_error, ")"), paste("Incorrect (Overall error rate:", error,")"))

   melted_result <- melt(result, id.vars = c("id","check", "predicted","color"))
  
  p <- ggplot(melted_result) + geom_line(aes(x = variable, y = value, group = id, color = color)) + scale_color_brewer(palette = "Set1")
  p <- p + facet_grid(predicted ~ .) + 
    theme()
  
  print(p)
  ggsave("~/retek.png", width = 15, height = 7)
}

myplot <- function(data, tree, x, y, xmax, ymax, plot, row = 1, xmin = 0, ymin = 0){
  if(nrow(tree[tree$id == row,]) == 0 ){
    if(row == 1){
      return(NULL)
    }
    return(plot)
  }

  if(tree[tree$id == row,3] == x){
    plot <- myplot(data, tree, x, y, plot = plot, row = tree[row,1], xmin = xmin, xmax = tree[row, x], ymin = ymin, ymax = ymax)
    plot <- myplot(data, tree, x, y, plot = plot, row = tree[row,2], xmin = tree[row, x], xmax = xmax, ymin = ymin, ymax = ymax)
  } else if(tree[tree$id == row,3]== y){
    plot <- myplot(data, tree, x, y, plot = plot, row = tree[row,1], xmin = xmin, xmax = xmax, ymin = ymin, ymax = tree[row, y])
    plot <- myplot(data, tree, x, y, plot = plot, row = tree[row,2], xmin = xmin, xmax = xmax, ymin = tree[row, y], ymax = ymax)
  } else{
    
    plot <- rbind(plot, c(tree[tree$id == row,]$prediction , xmin, xmax, ymin, ymax)) 
  }
  return(plot)
}

RFdecisionlayers <- function(data, object){
  
  rearrange_cols <- names(sort(object$importanceSD[,ncol(object$importanceSD)],decreasing = T))
  rf_cols <- data[c(rearrange_cols)]
  rf_cols <- rf_cols[,1:2]
  
  varnames <- names(rf_cols)
  splitvar1 <- which( colnames(data)==varnames[1])
  splitvar2 <- which( colnames(data)==varnames[2])
  
  p1 <- data.frame(fill = numeric(0), xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0))

  for (i in 1:object$ntree) {
    tr <- getTree(object,i)
    tr <- as.data.frame(tr)
    tmp <- names(tr)
    tr$id <- 1:nrow(tr)
    names(tr) <- c(tmp, "id")
  
    tr <- tr[(tr$`split var`== splitvar1 | tr$`split var`== splitvar2 | tr$`split var`== 0 ) ,]
    
    plot <- data.frame(c(1), c(2), c(3),c(4),c(5))
    p <- myplot(iris, tr, splitvar1, splitvar2, xmax = max(rf_cols[,1] +0.1), ymax = max(rf_cols[,2] +0.1), plot = plot)
    if(!is.null(p)) {
      p <- p[2:nrow(p),]
      p[,1] <- as.factor(p[,1])
      names(p) <- c("fill", "xmin", "xmax", "ymin", "ymax")
      
      p1 <- rbind(p1, p)
      
    }
  }
  p <- p1
  levels(p[,1]) <- object$classes
  
  plot <- ggplot() + xlim(0,max(data[,splitvar1])+0.1) + ylim(0,max(data[,splitvar2])+0.1) + 
    geom_rect( aes(xmin = 0, xmax = 0, ymin = 0, ymax = 0, fill = p[1,1], alpha = 1 ))
  plot = plot + geom_rect(data = p, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, alpha = (1/object$ntree) ))
  #geom_vline(data = p, aes(xintercept = xmin)) +
  # geom_vline(data = p, aes(xintercept = xmax)) + geom_hline(data = p, aes(yintercept = ymin)) + geom_hline(data = p, aes(yintercept = ymin))
  plot <- plot + geom_point(data = data, aes(x = data[,splitvar1], y = data[,splitvar2], shape = do.call(as.vector, c( object$call$y)))) 
  plot <- plot + labs(x = varnames[1], y = varnames[2])
  
   print(plot)
}

RFheatmap <- function(data, object, resolution){
  
  rearrange_cols <- names(sort(object$importanceSD[,ncol(object$importanceSD)],decreasing = T))
  rf_cols <- data[c(rearrange_cols)]
  rf_cols1 <- rf_cols
  rf_cols <- rf_cols[,1:2]
  
  varnames <- names(rf_cols)
  splitvar1 <- which( colnames(data)==varnames[1])
  splitvar2 <- which( colnames(data)==varnames[2])
  
  xmax <- max(rf_cols[,1] +0.1)
  ymax <- max(rf_cols[,2] +0.1)
  
  xpoints <- seq(0, xmax, by=xmax/(resolution+2))
  ypoints <- seq(0, ymax, by=ymax/(resolution+2))
  
  d <- expand.grid(x = xpoints[2:(length(xpoints)-1)], y = ypoints[2:(length(ypoints)-1)])
  

  for(i in 3:ncol(rf_cols1)){
    d[,i] <- 0
  }
  names(d) <- rearrange_cols
  
  predicted <- predict(object, d, type = "vote")
  
  predicted <- as.data.frame(predicted)


  class <- apply(predicted,1, function(x) which.max(x))
  value <- apply(predicted,1, function(x) max(x))
  
  predicted$value <- value
  predicted$class <- class
  
  d <- d[,1:2]
  d <- cbind(d, value, class)

  d[,4] <- as.factor(d[,4])
  levels(d[,4]) <- object$classes

  plot <- ggplot(data = d, aes(d[,1], d[,2])) + geom_raster(aes(fill = d[,4], alpha = d[,3])) + labs(x = varnames[1], y = varnames[2]) + scale_fill_brewer(palette = "Set1")
  plot <- plot + geom_point(data = data, aes(x = data[,splitvar1], y = data[,splitvar2], shape = do.call(as.vector, c( object$call$y))))
  print(plot)
  
}

colMaxes <- function(data) sapply(data, max, na.rm = TRUE)
colMins <- function(data) sapply(data, min, na.rm = TRUE)


getparcord_lines <- function(tree, mins, maxes, row_id = 1){
  row <- tree[row_id,]
  if(row[5] == -1){
    res <- rbind(mins, maxes)
    res <- as.data.frame(res)
    res$predicted <- row[6]
    res$num <- row_id
    rownames(res) <- NULL
    return(res)
  } else{
    newmins <- mins
    newmins[row[3]] <- row[4]
    
    newmaxes <- maxes
    newmaxes[row[3]] <- row[4]
    return(rbind(getparcord_lines(tree, mins, newmaxes, row_id = row[1]), getparcord_lines(tree, newmins, maxes, row_id = row[2])))
  }
}

pcordPlot2 <- function(data, object){
  
  maxes <- do.call(colMaxes, list(object$call$x))
  mins <- do.call(colMins, list(object$call$x))
  
  tree <- getTree(object)
  
  lines <- getparcord_lines(tree, mins, maxes)
  lines <- as.data.frame(lines)
  lines$id <- 1:nrow(lines)
  lines$predicted <- as.factor(lines$predicted)
  levels(lines$predicted) <- object$classes
  
  d <- melt(lines, id.vars = c('id', 'predicted', 'num'))
  
  p <- ggplot(d) + geom_line(aes(x = variable, y = value, group = id, color = predicted)) + scale_color_brewer(palette = "Set1") 
  
  print(p)
  
  p <- p + facet_grid(num ~ .)
  
  print(p)
  
}



