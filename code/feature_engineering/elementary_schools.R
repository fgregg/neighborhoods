library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

elementarySchools <- function(nodes) {

  elementary_schools <- rgdal::readOGR("/home/fgregg/academic/neighborhoods/phenomena/CPS_ElementarySchool_AttendanceBoundaries_SY13_14.shp",
                                       "CPS_ElementarySchool_AttendanceBoundaries_SY13_14")

  elementary_schools <- elementary_schools[as.vector(rgeos::gIntersects(elementary_schools, bbx, byid=TRUE)),]

  block_neighbors <-spdep::poly2nb(nodes,
                                   foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  block_edgelist <- common::nb2edgelist(block_neighbors)

  crosses <- common::crossesPolygons(block_edgelist,
                                     coordinates(nodes),
                                     elementary_schools,
                                     common::projection)
  return(crosses)
}

if (!common::from_source()) {
  crosses <- elementarySchools(block.groups.poly)
  write.csv(as.numeric(crosses > 1),
            file="../interchange/elementary_schools_crosses.csv", row.names=FALSE)
}


