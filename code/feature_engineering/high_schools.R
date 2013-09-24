library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

highSchools <- function(nodes) {

  high_schools <- rgdal::readOGR("/home/fgregg/academic/neighborhoods/phenomena/CPS_HighSchool_AttendanceBoundaries_13_14.shp",
                                 "CPS_HighSchool_AttendanceBoundaries_13_14")

  high_schools <- high_schools[as.vector(rgeos::gIntersects(high_schools, bbx, byid=TRUE)),]

  block_neighbors <-spdep::poly2nb(nodes,
                                   foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  block_edgelist <- common::nb2edgelist(block_neighbors)

  crosses <- common::crossesPolygons(block_edgelist,
                                     coordinates(nodes),
                                     high_schools,
                                     common::projection)

  distances <- common::distanceFromBorder(nodes, block_edgelist, crosses)

  return(distances)

}

if (!common::from_source()) {
  distances <- highSchools(blocks.poly)
  write.csv(distances,
            file="../interchange/high_schools_distances.csv", row.names=FALSE)
}
