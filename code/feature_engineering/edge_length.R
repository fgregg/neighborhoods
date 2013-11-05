library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


edgeLengths <- function(nodes) {
  edgelist <- common::edgeList(nodes)
  
  if (exists('all_edges')) {
    results <- all_edges
  } else {
    results <- common::extractBorder(edgelist, nodes)
  }


  lengths <- SpatialLinesLengths(results$lines)

  return(lengths)
}

if (!common::from_source()) {
  nodes = blocks.poly

  lengths <- edgeLengths(nodes)

  write.csv(lengths, file="../interchange/edge_lengths.csv", row.names=FALSE)
  
  
}
