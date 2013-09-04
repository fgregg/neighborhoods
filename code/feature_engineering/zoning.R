library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

zoningChanges <- function(nodes) {
  zoning <- rgdal::readOGR("/home/fgregg/academic/neighborhoods/phenomena/Zoning_nov2012.shp",
                           "Zoning_nov2012")

  zoning <- zoning[as.vector(rgeos::gIntersects(zoning, bbx, byid=TRUE)),]

  zone_types <- list(
    commercial = c(1,2,7,8,9),
    industrial = c(3,6),
    residential = c(4,9),
    planned.development = c(5),
    transport = c(11),
    park = c(12)
    )

  dissolved_SPDFS = list()

  for (i in 1:length(zone_types)) {
    dissolved_SPDFS[i] <- common::dissolve(zoning,
                                           "ZONE_TYPE",
                                           zone_types[[i]],
                                           names(zone_types)[i])
  }

  dissolved_SPDFS <- lapply(dissolved_SPDFS,
                            FUN=function(x) {
                              sp::spChFIDs(x,
                                           as.character(sample(100000000,
                                                               length(x))))
                            })

  dissolved_zones <- do.call(rbind, dissolved_SPDFS)

  block_neighbors <-spdep::poly2nb(nodes,
                                   foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  block_edgelist <- common::nb2edgelist(block_neighbors)

  crosses <- common::crossesPolygons(block_edgelist,
                                     coordinates(nodes),
                                     dissolved_zones,
                                     common::projection)
  return(crosses)
}


if (!common::from_source()) {
  crosses <- zoningChanges(block.groups.poly)
  write.csv(as.numeric(crosses > 1),
            file="../interchange/zoning_crosses.csv", row.names=FALSE)
}

