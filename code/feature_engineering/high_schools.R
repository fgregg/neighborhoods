library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

highSchools <- function(nodes, cached_edges, edge_lines) {

  high_schools <- rgdal::readOGR("/home/fgregg/academic/neighborhoods/phenomena/CPS_HighSchool_AttendanceBoundaries_13_14.shp",
                                 "CPS_HighSchool_AttendanceBoundaries_13_14")

  containing <- gCovers(high_schools, high_schools, byid=TRUE)

  high_schools <- high_schools[colSums(containing) < 2, ]

  crosses <- rowSums(rgeos::gIntersects(high_schools,
                                        edge_lines,
                                        byid=TRUE))

  return(list(crosses=crosses))
}

if (!common::from_source()) {
  results <- highSchools(blocks.poly)
  write.csv(results$distances,
            file="../interchange/high_schools_distances.csv", row.names=FALSE)
}
