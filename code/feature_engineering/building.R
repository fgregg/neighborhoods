library(rgdal)
library(devtools)
library(spdep)



pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)



projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Lat/Lon of Bounding Box
range = cbind(c(-87.7, -87.6), c(41.89,41.98))
range = rgdal::project(range, projection)

buildings <- rgdal::readOGR("../../phenomena/north_side_buildings.shp",
                            "north_side_buildings")

buildings@data$YEAR_BUILT[buildings@data$YEAR_BUILT == 0] <- NA
buildings@data$NO_OF_UNIT[buildings@data$NO_OF_UNIT == 0] <- NA
buildings@data$NO_STORIES[buildings@data$NO_STORIES == 0] <- NA
buildings@data$BLDG_SQ_FO[buildings@data$BLDG_SQ_FO == 0] <- NA
buildings@data$STORIES[buildings@data$STORIES == 0] <- NA

buildings$block <- over(buildings, blocks.poly)

buildings$block[is.na(buildings$block)] <- 0

aggregateSPDF <- function(colname, by, fun, source_spdf, dest_spdf) {
  rows <- data.frame(row.num=1:dim(dest_spdf)[1])
  
  ag <- aggregate(source_spdf[, colname],
                  list(source_spdf[, by]),
                  FUN = fun)

  ag$x[is.infinite(ag$x)] <- NA

  ag <- merge(rows, ag, by.x="row.num", by.y="Group.1", all.x=TRUE)

  return(ag)
}


oldest_building <- aggregateSPDF("YEAR_BUILT",
                                 "block",
                                 function(x) {median(na.omit(x))},
                                 buildings@data,
                                 blocks.poly)


medianWithoutGarages <- function(x) {
  x <- na.omit(x)
  x <- x[x > 610]
  return(median(x))
}

footprint_size <- aggregateSPDF("SHAPE_AREA",
                                "block",
                                medianWithoutGarages,
                                buildings@data,
                                blocks.poly)


grade <- colorRampPalette(c("black", "white"))
colors <- c(grade(100), rep("red", 100))
plot(blocks.poly, col=colors[oldest_building$x-1854], border="transparent")

grade <- colorRampPalette(c("black", "white"))
plot(blocks.poly, col=grade(70)[log(footprint_size$x)*10-54], border="transparent")

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(blocks.poly,
                    foundInBox=gUnarySTRtreeQuery(blocks.poly))
# plot(blocks.poly, col=colors()[blocks.poly@data$label]) # check alignment of potts labels

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)


diff_size <- abs(log(footprint_size[edgelist[,1], "x"]) -
                 log(footprint_size[edgelist[,2], "x"]))
diff_size[is.na(diff_size)] <- 0

write.table(diff_size,
            file="../interchange/diff_footprints.txt",
            row.names = FALSE)
