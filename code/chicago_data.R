library(devtools)
library(spdep)
library(rgeos)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Lat/Lon of Bounding Box

# Load Block Data
if (file.exists("chicago/data/chicago_blocks_poly.Rdata")) {
  load("chicago/data/chicago_blocks_poly.Rdata")
} else {
  chicago.blocks.poly <- readOGR("../admin_areas/CensusBlockTIGER2010.shp",
                         "CensusBlockTIGER2010")
  save(chicago.blocks.poly, file="chicago/data/chicago_blocks_poly.Rdata")
}


# Shape Files of Barriers that will effect edge weights

if (file.exists("chicago/data/chicago_barriers.Rdata")) {
  load("chicago/data/chicago_barriers.Rdata")
} else {
  railroads <- readOGR("../barriers/Railroads.shp",
                       layer = "Railroads")
  chicago.railroads <- spTransform(railroads,
                                   CRS(projection)
                                   )
  
  streets <- readOGR("../barriers/Major_Streets.shp",
                     layer="Major_Streets",
                     p4s="+proj=utm +zone=16 +datum=NAD83")
  
  chicago.highways <- streets[(streets@data$STREET %in% c("LAKE SHORE",
                                                         "KENNEDY")
                               | streets@data$STREET_TYP == "EXPY"),]
  
  chicago.grid.streets <- streets[!(streets@data$STREET %in% c("LAKE SHORE",
                                                         "KENNEDY")
                                    | streets@data$STREET_TYP == "EXPY"),]
  
  water <- readOGR("../barriers/Kmlchicagowaterfeatures.kml",
                   layer = "WATER_FEATURES")
  chicago.water <- spTransform(water,
                               CRS(projection)
                               )
  save(chicago.railroads,
       chicago.highways,
       chicago.grid.streets,
       chicago.water,
       file="chicago/data/chicago_barriers.Rdata")
}



if (file.exists("chicago/data/chicago_all_edges.Rdata")) {
  load("chicago/data/chicago_all_edges.Rdata")
} else { 

    nodes = chicago.blocks.poly
    node_neighbors <-spdep::poly2nb(nodes,
                                    queen=FALSE,
                                    foundInBox=rgeos::gUnarySTRtreeQuery(nodes))
    node_edgelist <- common::nb2edgelist(node_neighbors)

    chicago.all_edges <- common::extractBorder(node_edgelist, nodes) 
    save(chicago.all_edges, file='chicago/data/chicago_all_edges.Rdata')
}

if (file.exists("chicago/data/chicago_edge_lines.Rdata")) {
  load("chicago/data/chicago_edge_lines.Rdata")
} else { 

  edgelist <- common::edgeList(chicago.blocks.poly, edges=chicago.all_edges)

  centroids <- coordinates(chicago.blocks.poly)

  chicago.edge.lines <- common::edgesToLines(edgelist, centroids, projection)

  save(chicago.edge.lines, file='chicago/data/chicago_edge_lines.Rdata')
}
