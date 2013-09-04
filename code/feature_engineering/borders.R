library(rgeos)
library(spdep)
library(devtools)

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

  return(border)
}

if (!common::from_source()) {
  border <- borders(block.groups.poly)
  write.csv(border,
            file="../interchange/border.csv", row.names=FALSE)
}
