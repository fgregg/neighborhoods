library(rgeos)
library(devtools)
library(spdep)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

lineGraph <- function(nodes) {
  neighbors <-spdep::poly2nb(nodes,
                             foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  edgelist <- common::nb2edgelist(neighbors)

  # Line graph, the 'edges' between the edges
  primal_graph <- igraph::graph.data.frame(edgelist, directed=FALSE)
  line_graph <- igraph::line.graph(primal_graph)

  return(line_graph)
}

if (!common::from_source()) {
  line_graph <- lineGraph(block.groups.poly)
  igraph::write.graph(line_graph,
                      "../interchange/line_graph_edges.txt",
                      format="edgelist")
}
