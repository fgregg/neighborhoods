library(RMySQL)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

labelNodes <- function(nodes) {

  # Import Neighborhood Label Point Data
  con <- dbConnect(MySQL(), dbname="neighborhood")

  listings <- dbGetQuery(con, "
  SELECT DISTINCT
  X(lat_long) AS y,
  Y(lat_long) AS x,
  label.label AS neighborhood
  FROM location, label
  WHERE location.location_id = label.location_id
  AND city = 'chicago'"
                         ) 
  dbDisconnect(con)


  listings$neighborhood <- common::cleanLabels(listings$neighborhood)

  listings <- sp::SpatialPointsDataFrame(coords = listings[, c("x", "y")],
                                         data = data.frame(listings$neighborhood),
                                         proj4string = CRS("+proj=longlat")
                                         )
  names(listings) <- c("neighborhood")
  listings <- sp::spTransform(listings, CRS(projection))

  neighborhoods <- c("lakeview","lincoln park", "roscoe village",
                     "buena park", "southport corridor", "wrigleyville",
                     "boystown", "uptown", "north center", "ravenswood",
                     "lincoln square", "avondale", "old town",
                     "logan square", "bucktown", "wicker park",
                     "humboldt park", "andersonville", "albany park",
                     "river north", "river west", "ukrainian village",
                     "east village", "noble square", "streeterville",
                     "west town", "magnificent mile", "gold coast")

  centroids <- sp::coordinates(nodes)
  # Estimate KDE
  classes = common::trainKDE(listings,
    neighborhoods,
    centroids,
    range,
    common::ks.pi,
    0.0000015)

  # Unary Potentials of Block Labels
  unary <- log(classes)
  minimum.val = min(unary[is.finite(unary)])
  unary[is.infinite(unary)] <- minimum.val
  return(unary)
}
  
if (!common::from_source()) {
  unary <- labelNodes(blocks.poly)

  write.csv(unary, file="unary.csv", row.names=FALSE)

  write.csv(unary, file="../interchange/unary.csv", row.names=FALSE)

  node_neighbors <-spdep::poly2nb(blocks.poly,
                                queen=FALSE,
                                foundInBox=rgeos::gUnarySTRtreeQuery(blocks.poly))
  node_edgelist <- common::nb2edgelist(node_neighbors)[all_edges$spatial_lines,]
  G <- igraph::graph.data.frame(node_edgelist)

  ks_labels <- apply(unary, 1, which.max)

  segments <- common::hoodsToSegments(ks_labels, G)
  write.csv(segments, file="../interchange/ks_label.csv", row.names=FALSE)
}
