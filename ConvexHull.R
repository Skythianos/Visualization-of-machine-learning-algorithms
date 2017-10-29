library(ggplot2)

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)


stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ggplot(iris, aes(Petal.Length, Petal.Width)) + 
#   geom_point() + 
#   stat_chull(fill = NA, colour = "black")


###########

# library(rgeos)
# 
# ## Convert data.frame of segment coordinates to a list of SpatialLines objects
# ll <- apply(voronoi$dirsgs, 1, FUN=function(X) {
#   readWKT(sprintf("LINESTRING(%s %s, %s %s)", X[1], X[2], X[3], X[4]))
# })
# 
# ## Convert SpatialLines list to SpatialPolygons object
# pp <- gPolygonize(ll)
# 
# ## Plot to check that it works    
# set.seed=11
# plot(pp, col=sample(colors(), length(pp)))