source('utils.R')

projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Lat/Lon of Bounding Box
range = cbind(c(-87.7, -87.6), c(41.89,41.98))
range = project(range, projection)

bbx <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(range[1,1],
                                                          range[1,1],
                                                          range[2,1],
                                                          range[2,1],
                                                          range[1,1]),
                                                        c(range[1,2],
                                                          range[2,2],
                                                          range[2,2],
                                                          range[1,2],
                                                          range[1,2])))),
                                     "p")),
                       proj4string = CRS(projection))

# Load Block Data
if (file.exists("blocks_poly.Rdata")) {
  load("blocks_poly.Rdata")
} else {
  blocks.poly <- readOGR("../admin_areas/CensusBlockTIGER2010.shp",
                         "CensusBlockTIGER2010")
  save(blocks.poly, file="blocks_poly.Rdata")
}

# Subset Blocks
centroids <- coordinates(blocks.poly)
blocks.poly <- blocks.poly[centroids[,1] > range[1,1] &
                           centroids[,1] < range[2,1] &
                           centroids[,2] > range[1,2] &
                           centroids[,2] < range[2,2],]

centroids <- coordinates(blocks.poly)
colnames(centroids) <- c("x", "y")

# Shape Files of Barriers that will effect edge weights

if (file.exists("barriers.Rdata")) {
  load("barriers.Rdata")
} else {
  railroads <- readOGR("../barriers/Railroads.shp",
                       layer = "Railroads")
  railroads <- spTransform(railroads,
                           CRS(projection)
                           )
  railroads <- gIntersection(railroads, bbx)
  
  streets <- readOGR("../barriers/Major_Streets.shp",
                     layer="Major_Streets",
                     p4s="+proj=utm +zone=16 +datum=NAD83")
  
  highways <- streets[streets@data$STREET %in% c("LAKE SHORE",
                                                 "KENNEDY"),]
  highways <- gIntersection(highways, bbx)
  
  grid.streets <- streets[!streets@data$STREET %in% c("LAKE SHORE",
                                                      "KENNEDY"),]
  grid.streets <- gIntersection(grid.streets, bbx)
  
  water <- readOGR("../barriers/Kmlchicagowaterfeatures.kml",
                   layer = "WATER_FEATURES")
  water <- spTransform(water,
                       CRS(projection)
                       )
  water <- gIntersection(water, bbx)
  save(railroads, highways, grid.streets, water, file="barriers.Rdata")
}


if (file.exists("barrier_intersects.Rdata")) {
  load("barrier_intersects.Rdata")
} else {
  edgelist <-nb2edgelist(poly2nb(blocks.poly,
                                 foundInBox=gUnarySTRtreeQuery(blocks.poly)))

  railroad.intersects <- edgesIntersect(edgelist,
                                        centroids,
                                        railroads,
                                        projection)

  highway.intersects <- edgesIntersect(edgelist,
                                       centroids,
                                       highways,
                                       projection)
  
  grid.street.intersects <- edgesIntersect(edgelist,
                                           centroids,
                                           grid.streets,
                                           projection)

  water.intersects <- edgesIntersect(edgelist,
                                     centroids,
                                     water,
                                     projection)

  save(railroad.intersects,
       highway.intersects,
       grid.street.intersects,
       water.intersects,
       file="barrier_intersects.Rdata")
}



  
