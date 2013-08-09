nb2edgelist <- function(nb) {
  el <- c()
  for (i in 1:length(nb)) {
    neighbors <- nb[[i]]
    new.neighbors <- neighbors[neighbors > i]
    if (length(new.neighbors) > 0) {
      el <- rbind(el, cbind(i, new.neighbors))
    }
  }
  return(el)
}

# Could certainly be sped up by creating all the lines once
edgesIntersect <- function(edges, centroids, barrier, crs) {
  edgeToLine <- createEdgeToLine(centroids, crs)
  apply(edges,
        1,
        FUN=function(x) {
          gIntersects(edgeToLine(x), barrier)
        }
        )
}
  
createEdgeToLine <- function(centroids, crs) {
  edgeToLine <- function(edge) {
    l <- rbind(centroids[edge[1],],
               centroids[edge[2],])
    l <- Line(l)
    l <- SpatialLines(list(Lines(l, ID="l")),
                      proj4string = CRS(crs))
  }
  return(edgeToLine)
}


