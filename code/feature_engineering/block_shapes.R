library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


blockShapes <- function(nodes) {

  coords <- lapply(slot(nodes, "polygons"), function(x) {
    lapply(slot(x, "Polygons"), slot, "coords")}
                   )

  angles <- lapply(coords, function(x) {common::minimum_bounding_box(x[[1]])$theta})

  angles <- unlist(angles) %% pi



  # Topology of Block Connectivity
  neighbors <- spdep::poly2nb(nodes,
                              foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  # Calculate 'edge features' will be node features in training
  edgelist <- common::nb2edgelist(neighbors)
    

  angle_difference <- abs(angles[edgelist[,1]] - angles[edgelist[,2]])

  if (interactive() & !common::from_source()) {
    png("block_orientation.png")
    classes <- classInt::classIntervals(angles, 8, style="kmeans")
    colcode <- classInt::findColours(classes, c("wheat", "red"))

    plot(nodes, col=colcode, border='transparent')
    border_lines <- common::plotBorders(edgelist)
    lines(border_lines)
    dev.off()
  }

  return(angle_difference)
}

if (!common::from_source()) {
  nodes <- populated.blocks
  angle_differences <- blockShapes(nodes)
  write.csv(angle_differences,
            file="../interchange/block_angles.csv", row.names=FALSE)
}


