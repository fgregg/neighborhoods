edgeStatistics <- function(edges) {
  angles <- lineAngles(edges)
  centroids <- rgeos::gCentroid(edges, byid=TRUE)

  return(list(angles=angles, centroids=centroids))
}

bufferNodes <- function(feature_edges, nodes) {
  buffer <- rgeos::gBuffer(feature_edges, width=1000)

  buffer_nodes <- nodes[as.vector(rgeos::gIntersects(nodes,
                                                     buffer,
                                                     byid=TRUE)),]
  return(buffer_nodes)
}


minDistance <- function(nodes, features) {
  min_distance <- rep(0, length(nodes$angles))
  
  for (i in 1:length(nodes$angles)) {
    aligned_angle <- abs(nodes$angles[i] - features$angles) < pi/4
    min_distance[i] <- min(spDistsN1(features$centroids[aligned_angle],
                                     nodes$centroids[i]))
  }

  min_distance[min_distance > 1000] <- 1000

  min_distance <- min_distance/1000

  return(min_distance)
}


distanceFromBorder <- function(nodes, node_edgelist, intersections) {

  feature_edges <- common::extractBorder(node_edgelist[intersections > 1,],
                                         nodes)$lines
  features <- edgeStatistics(feature_edges)

  buffer_nodes <- bufferNodes(feature_edges,
                              nodes)
  buffer_node_edgelist <- edgeList(buffer_nodes, all_blocks=FALSE)

  results <- common::extractBorder(buffer_node_edgelist,
                                   buffer_nodes)
  

  node_edges <- edgeStatistics(results$lines)
  spatial_lines <- results$spatial_lines

  buffer_distance <- rep(1, length(spatial_lines))

  min_distance <- minDistance(node_edges, features)

  buffer_distance[spatial_lines] <- min_distance
  
  alignment <- match(paste(buffer_nodes@data[buffer_node_edgelist[,1],
                                             "TRACT_BLOC"],
                           buffer_nodes@data[buffer_node_edgelist[,2],
                                             "TRACT_BLOC"]),
                     paste(nodes@data[node_edgelist[,1],
                                      "TRACT_BLOC"],
                           nodes@data[node_edgelist[,2],
                                      "TRACT_BLOC"]))

  print(alignment)
  
  distances <- rep(1, dim(node_edgelist)[1])
  
  distances[alignment] <- buffer_distance

  return(distances)
}

lineAngles <- function(lines) {
  coords <- lapply(slot(lines, "lines"), function(x) {
    lapply(slot(x, "Lines"), slot, "coords")})

  angles <- sapply(coords, function(x) {
    common::minimum_bounding_box(x[[1]])$theta})

  angles <- angles %% pi

  return(angles)
}
