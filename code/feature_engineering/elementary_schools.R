library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

elementarySchools <- function(nodes, cached_edges, edge_lines) {

  elementary_schools <- rgdal::readOGR("/home/fgregg/academic/neighborhoods/phenomena/CPS_ElementarySchool_AttendanceBoundaries_SY13_14.shp",
                                       "CPS_ElementarySchool_AttendanceBoundaries_SY13_14")

  containing <- gCovers(elementary_schools, elementary_schools, byid=TRUE)

  elementary_schools <- elementary_schools[colSums(containing) < 2, ]

  crosses <- rowSums(rgeos::gIntersects(elementary_schools,
                                        edge_lines,
                                        byid=TRUE))

  return(list(crosses=crosses))
}

if (!common::from_source()) {
  results <- elementarySchools(blocks.poly)
  write.csv(as.numeric(results$crosses > 1),
            file="../interchange/elementary_schools_crosses.csv", row.names=FALSE)
  write.csv(results$distances,
            file="../interchange/elementary_schools_distances.csv", row.names=FALSE)
}


