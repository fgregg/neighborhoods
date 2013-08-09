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

edgesIntersect <- function(edges, centroids, barrier, crs) {
  edgeToLine <- createEdgeToLine(centroids, crs)
  apply(edges,
        1,
        FUN=function(x) {
          gIntersects(edgeToLine(x), barrier)
        }
        )
  
}

