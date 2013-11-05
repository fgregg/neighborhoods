library(rgeos)
library(devtools)
library(spdep)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

adjacentEdges <- function(nodes) {
  if (exists('all_edges')) {
    edges <- all_edges$lines
  } else {
    node_neighbors <-spdep::poly2nb(nodes,
                                    queen=FALSE,
                                    foundInBox=rgeos::gUnarySTRtreeQuery(nodes))
    node_edgelist <- common::nb2edgelist(node_neighbors)

    edges <- common::extractBorder(node_edgelist, nodes)$lines
  }

  buffered_edges <- rgeos::gBuffer(edges, byid=TRUE)

  edge_neighbors <- spdep::poly2nb(buffered_edges,
                                   queen=FALSE,
                                   foundInBox=rgeos::gUnarySTRtreeQuery(buffered_edges),
                                   snap=0.01)

  return(edge_neighbors)
}
