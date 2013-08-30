library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

high_schools <- rgdal::readOGR("../../phenomena/CPS_HighSchool_AttendanceBoundaries_13_14.shp",
                         "CPS_HighSchool_AttendanceBoundaries_13_14")

high_schools <- high_schools[as.vector(rgeos::gIntersects(high_schools, bbx, byid=TRUE)),]

block_neighbors <-spdep::poly2nb(blocks.poly,
                                 foundInBox=rgeos::gUnarySTRtreeQuery(blocks.poly))

block_edgelist <- common::nb2edgelist(block_neighbors)

crosses <- common::crossesPolygons(block_edgelist,
                                   coordinates(blocks.poly),
                                   high_schools,
                                   common::projection)

write.csv(as.numeric(crosses > 1),
          file="../interchange/high_schools_crosses.csv", row.names=FALSE)

