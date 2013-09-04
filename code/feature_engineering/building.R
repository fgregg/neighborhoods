library(rgdal)
library(rgeos)
library(devtools)
library(spdep)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

aggregateSPDF <- function(colname, by, fun, source_spdf, dest_spdf) {
  rows <- data.frame(row.num=1:dim(dest_spdf)[1])
  
  ag <- aggregate(source_spdf[, colname],
                  list(source_spdf[, by]),
                  FUN = fun)

  ag$x[is.infinite(ag$x)] <- NA

  ag <- merge(rows, ag, by.x="row.num", by.y="Group.1", all.x=TRUE)

  return(ag)
}

medianWithoutGarages <- function(x) {
  x <- na.omit(x)
  x <- x[x > 610]
  return(median(x))
}

buildingDifference <- function(nodes) {

  buildings <- rgdal::readOGR("../../phenomena/north_side_buildings.shp",
                              "north_side_buildings")
  sp::proj4string(buildings) <- sp::proj4string(nodes) 

  buildings@data$YEAR_BUILT[buildings@data$YEAR_BUILT == 0] <- NA
  buildings@data$NO_OF_UNIT[buildings@data$NO_OF_UNIT == 0] <- NA
  buildings@data$NO_STORIES[buildings@data$NO_STORIES == 0] <- NA
  buildings@data$BLDG_SQ_FO[buildings@data$BLDG_SQ_FO == 0] <- NA
  buildings@data$STORIES[buildings@data$STORIES == 0] <- NA

  buildings$block <- over(buildings, nodes)

  buildings$block[is.na(buildings$block)] <- 0

  oldest_building <- aggregateSPDF("YEAR_BUILT",
                                   "block",
                                   function(x) {median(na.omit(x))},
                                   buildings@data,
                                   nodes)

  footprint_size <- aggregateSPDF("SHAPE_AREA",
                                  "block",
                                  medianWithoutGarages,
                                  buildings@data,
                                  nodes)

  #grade <- colorRampPalette(c("black", "white"))
  #colors <- c(grade(100), rep("red", 100))
  #plot(nodes, col=colors[oldest_building$x-1854], border="transparent")

  #grade <- colorRampPalette(c("black", "white"))
  #plot(nodes, col=grade(70)[log(footprint_size$x)*10-54], border="transparent")

  # Topology of Block Connectivity
  neighbors <-spdep::poly2nb(nodes,
                             foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  # Calculate 'edge features' will be node features in training
  edgelist <- common::nb2edgelist(neighbors)


  diff_size <- abs(log(footprint_size[edgelist[,1], "x"]) -
                   log(footprint_size[edgelist[,2], "x"]))
  diff_size[is.na(diff_size)] <- 0

  diff_age <- abs(oldest_building[edgelist[,1], "x"]
                  -
                  oldest_building[edgelist[,2], "x"])
  diff_age[is.na(diff_age)] <- 0

  building_differences <- list(size=diff_size,
                               age=diff_age)
  
  return(building_differences)
}

if (!common::from_source()) {
  building_diffs <- buildingDifference(block.groups.poly)
  write.table(building_diffs$size,
              file="../interchange/diff_footprints.txt",
              row.names = FALSE)
  write.table(building_diffs$age,
              file="../interchange/diff_age.txt",
              row.names = FALSE)
}
