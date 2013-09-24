library(rgeos)
library(spdep)
library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

borders <- function(nodes) {
  # Topology of Block Connectivity
  neighbors <- spdep::poly2nb(nodes,
                              foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  # Calculate 'edge features' will be node features in training
  edgelist <- common::nb2edgelist(neighbors)


  # Merge labels 
  plabels <- read.csv("/home/fgregg/academic/neighborhoods/code/interchange/potts_labels.csv")
  nodes@data = data.frame(nodes@data, plabels)

  # Is the edge a border between two different 'hoods
  border <- (nodes@data[edgelist[,1], "label"]
             != 
             nodes@data[edgelist[,2], "label"])
  border <- as.numeric(border)

  # Line graph, the 'edges' between the edges
  primal_graph <- igraph::graph.data.frame(edgelist, directed=FALSE)

  nabes <- unlist(igraph::neighborhood(primal_graph, 2))

  penalties <- border
  penalties[nabes] <- 0.5



  return(list(border=border, penalty=penalties))
}

if (!common::from_source()) {
  border <- borders(populated.blocks)
  write.csv(border$border,
            file="../interchange/border.csv", row.names=FALSE)
  write.csv(border$penalty,
            file="../interchange/penalty.csv", row.names=FALSE)
}
