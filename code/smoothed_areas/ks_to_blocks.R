library(RMySQL)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

labelNodes <- function(nodes) {
    con <- dbConnect(MySQL(), dbname="neighborhood")

    names <- dbGetQuery(con, "
      SELECT 
      COUNT(DISTINCT(location_id)) as num_locations,
      LOWER(label) AS neighborhood
      FROM claim, listing
      WHERE claim.listing_id = listing.listing_id
      AND listing.city = 'chicago'
      group by lower(label)")

    dbDisconnect(con)

    normalized_neighborhood <- common::normalizedNames(names, 'chicago',
                                                       'il')

    # remove compound neighborhood
    normalized_neighborhood <- normalized_neighborhood[!grepl("/",
                                                              normalized_neighborhood)]

    # Import Neighborhood Label Point Data
    con <- dbConnect(MySQL(), dbname="neighborhood")

    claims <- dbGetQuery(con, "
      SELECT
      DISTINCT
      X(lat_long) AS y,
      Y(lat_long) AS x,
      LOWER(label) AS neighborhood
      FROM claim, listing, geolocation
      WHERE claim.listing_id = listing.listing_id
      AND claim.location_id = geolocation.location_id
      AND listing.city = 'chicago'")

    dbDisconnect(con)

    claims$neighborhood <- as.factor(normalized_neighborhood[claims$neighborhood])
    freq_names <- table(claims$neighborhood)

    claims <- claims[(freq_names[claims$neighborhood] >= 10
                      & !is.na(claims$neighborhood)),]

    claims$neighborhood <- factor(claims$neighborhood)

    print(sort(table(claims$neighborhood)))

    claims <- sp::SpatialPointsDataFrame(coords = claims[, c("x", "y")],
                                         data = data.frame(claims$neighborhood),
                                         proj4string = CRS("+proj=longlat")
                                         )
    names(claims) <- c("neighborhood")
    claims <- sp::spTransform(claims, CRS(projection))

    neighborhoods <- unique(claims$neighbohood)
    
    centroids <- sp::coordinates(nodes)
    # Estimate KDE
    classes = common::trainKDE(claims,
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


    







