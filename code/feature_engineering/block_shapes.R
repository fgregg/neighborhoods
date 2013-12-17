library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


blockShapes <- function(nodes, cached_edges, make_plots=FALSE) {

  coords <- lapply(slot(nodes, "polygons"), function(x) {
    lapply(slot(x, "Polygons"), slot, "coords")}
                   )

  angles <- lapply(coords, function(x) {common::minimum_bounding_box(x[[1]])$theta})

  angles <- unlist(angles) %% pi



  # Calculate 'edge features' will be node features in training
  edgelist <- common::edgeList(nodes, edges=cached_edges)
    

  angle_difference <- abs(angles[edgelist[,1]] - angles[edgelist[,2]])

  if (make_plots) {
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


