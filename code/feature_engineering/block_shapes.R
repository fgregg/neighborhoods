library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


blockShapes <- function(nodes, edgelist) {

  coords <- lapply(slot(nodes, "polygons"), function(x) {
    lapply(slot(x, "Polygons"), slot, "coords")}
                   )

  angles <- lapply(coords, function(x) {common::minimum_bounding_box(x[[1]])$theta})

  angles <- unlist(angles) %% pi

  angle_difference <- abs(angles[edgelist[,1]] - angles[edgelist[,2]])

  return(angle_difference)
}


