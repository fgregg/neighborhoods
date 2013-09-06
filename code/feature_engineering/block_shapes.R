library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


# http://richardburcher.com/2012/05/01/minumum-bounding-rectangle-as-proxy-for-internal-metrics-of-drumlins-or-other-geometries/

minimum_bounding_box <- function(points) {
  points <- points[chull(points),]

  n_points <- nrow(points)
  
  # Complete the ring
  points <- rbind(points, points[1,])
  

  minimum_box_area <- Inf
  minimum_box_theta <- NA
  minimum_rotated_points <- NA
  for (i in 1:n_points) {
    theta <- pi - atan2(points[i+1,2]-points[i,2],
                        points[i+1,1]-points[i,1])
    rotated_points <- rotate(points, theta)
    box_area <- (diff(range(rotated_points[,1]))
                         * diff(range(rotated_points[,2]))
                         )
    if (box_area < minimum_box_area) {
      minimum_box <- cbind(
        c(min(rotated_points[,1]),max(rotated_points[,1]),
          max(rotated_points[,1]),min(rotated_points[,1])),
        c(min(rotated_points[,2]),min(rotated_points[,2]),
          max(rotated_points[,2]),max(rotated_points[,2]))
        )
      minimum_box <- rbind(minimum_box,
                           minimum_box[1,])
      minimum_box <- rotate(minimum_box, -theta)

      minimum_rotated_points <- rotated_points
      minimum_box_area <- box_area
      minimum_box_theta <- theta
    }
  }
  return(list(minbba=minimum_box_area,
              theta=minimum_box_theta,
              pts=minimum_rotated_points,
              box=minimum_box))
}


rotate = function(pts,angle){
  co = cos(angle)
  si = sin(angle)
  cbind(co * pts[,1] - si * pts[,2], si * pts[,1] + co * pts[,2])
}


blockShapes <- function(nodes) {

  coords <- lapply(slot(nodes, "polygons"), function(x) {
    lapply(slot(x, "Polygons"), slot, "coords")}
                   )

  angles <- lapply(coords, function(x) {minimum_bounding_box(x[[1]])$theta})

  angles <- unlist(angles) %% pi


  # Topology of Block Connectivity
  neighbors <- spdep::poly2nb(nodes,
                              foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  # Calculate 'edge features' will be node features in training
  edgelist <- common::nb2edgelist(neighbors)
    

  angle_difference <- abs(angles[edgelist[,1]] - angles[edgelist[,2]])

  return(angle_difference)
}

if (!common::from_source()) {
  angle_differences <- blockShapes(populated.blocks)
  write.csv(angle_differences,
            file="../interchange/block_angles.csv", row.names=FALSE)
}
