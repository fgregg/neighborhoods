library(maptools)
library(rgdal)
library(spdep)
library(rgeos)
library(igraph)
library(RMySQL)
library(ks)

source('utils.R')
source('common_data.R')

alpha.levels <- function(probs) {
  return(as.numeric(cut(probs, breaks=seq(0, 1.1, .1)/11)))
       }

probColors <- function(map.colors, classes) {
  #map.colors <- sample(map.colors, length(map.colors))
  max.class <- apply(classes, 1, which.max)
  max.probs <- apply(classes, 1, max)
  block.colors <- map.colors[max.class]
  return(rgb(t(col2rgb(block.colors))/255, alpha=max.probs))
}

normalize <- function(x) {
  return((x - median(x))/mad(x, constant=1))
  }
  
hampelCriticalValue <- function(n, alpha, num.samples=100) {
  samples = matrix(rnorm(n * num.samples, 0, 1), n, num.samples)
  samples = normalize(samples)/0.6745
  maxValues = apply(samples,
                    MARGIN=2,
                    FUN=function(x) {max(abs(x))}
                    )
  return(quantile(maxValues, 1-alpha))
}


hampelOutliers <- function(points, alpha) {
  threshold <- hampelCriticalValue(dim(points)[1], alpha)

  return(points[abs(normalize(points@coords[,1])) < threshold &
                abs(normalize(points@coords[,2])) < threshold, ])
}

trainKDE <- function(listings,
                     hoods,
                     centroids,
                     range,
                     kde,
                     no.hood.prior) {
  
  class.matrix <- array(, c(dim(centroids)[1],
                            length(hoods)+1
                            )
                        )
  total.listings = 0
  
  for (i in 1:length(hoods)) {
    hood.listings <- listings[listings@data$neighborhood == hoods[i],]
    hood.listings <- hampelOutliers(hood.listings, .90)

    num.listings <- dim(hood.listings)[1]
    
    p.density <- kde(hood.listings@coords, centroids)
    
    class.matrix[,i] <- p.density * num.listings
  }
  
  class.matrix[,length(hoods)+1] <- no.hood.prior

  # the kde returns an estimate of the probability of drawing a
  # neighborhood centroid from the distribution of places that belong
  # to a neighborhood. We are interested in the inverse problem, what
  # is the probability of location be assigned to a neighborhood.
  class.matrix <- class.matrix/rowSums(class.matrix)

  return(class.matrix)
}
@ 


ks.pi <- function(points, test.points) {
  H.est <- Hpi.diag(x=points, binned=TRUE)
  fhat <- kde(x=points, 
              H=H.est,
              eval.points = test.points)
  p.x.hood <- fhat$estimate
  
  return(fhat$estimate)
}


createEdgeToLine <- function(centroids, crs) {
  edgeToLine <- function(edge) {
    l <- rbind(centroids[edge[1],],
               centroids[edge[2],])
    l <- Line(l)
    l <- SpatialLines(list(Lines(l, ID="l")),
                      proj4string = CRS(crs))
  }
  return(edgeToLine)
}

# Shape Files For Context
com.areas <- readOGR("/home/fgregg/academic/neighborhoods/admin_areas/chicomm.shp",
                     layer="chicomm",
                     p4s="+proj=longlat")
com.areas <- spTransform(com.areas,
                         CRS(projection)
                         )
parks <- readOGR("../barriers/Kmlchicagoparks.kml",
                 layer = "Chicago Parks")
parks <- spTransform(parks,
                     CRS(projection)
                     )


# Import Neighborhood Label Point Data
con <- dbConnect(MySQL(), dbname="neighborhood")
listings <- dbGetQuery(con, "
SELECT DISTINCT
X(lat_long) AS y,
Y(lat_long) AS x,
label.label AS neighborhood
FROM location, label
WHERE location.location_id = label.location_id"
                       ) 
dbDisconnect(con)

source("../code/normalizeNeighborhoods.R")
listings$neighborhood <- cleanLabels(listings$neighborhood)

listings <- SpatialPointsDataFrame(coords = listings[, c("x", "y")],
                                   data = data.frame(listings$neighborhood),
                                   proj4string = CRS("+proj=longlat")
                                   )
names(listings) <- c("neighborhood")
listings <- spTransform(listings, CRS(projection))

neighborhoods <- c("lakeview","lincoln park", "roscoe village",
                   "buena park", "southport corridor", "wrigleyville",
                   "boystown", "uptown", "north center", "ravenswood",
                   "lincoln square", "avondale", "old town",
                   "logan square", "bucktown", "wicker park",
                   "humboldt park", "andersonville", "albany park",
                   "river north", "river west", "ukrainian village",
                   "east village", "noble square", "streeterville",
                   "west town", "magnificent mile", "gold coast")

# Estimate KDE
classes = trainKDE(listings,
                   neighborhoods,
                   centroids,
                   range, 
                   ks.pi,
                   0.0000015)

map.colors = rep(c('#B2DF8A', "#A6CEE3", "#1F78B4", "#33A02C",
                   "#FB9A99", "#E31A1C", "#FDBF6F"),
             length.out = length(neighborhoods))

# Plot KDE
plot(blocks.poly,
     col = probColors(map.colors, classes),
     border=rgb(0,0,0,0.04),
     lwd=0.01)

# Topology of Block Connectivity
blocks <-poly2nb(blocks.poly,
                 foundInBox=gUnarySTRtreeQuery(blocks.poly))
blocks <- nb2edgelist(blocks)
write.csv(blocks, file="edges.csv", row.names=FALSE)

# Unary Potentials of Block Labels
unary <- log(classes)
minimum.val = min(unary[is.finite(unary)])
unary[is.infinite(unary)] <- minimum.val
write.csv(unary, file="unary.csv", row.names=FALSE)

#Edge Weights 
railroad.intersects <- edgesIntersect(blocks,
                                      centroids,
                                      railroads,
                                      projection)
highway.intersects <- edgesIntersect(blocks,
                                     centroids,
                                     highways,
                                     projection)
grid.street.intersects <- edgesIntersect(blocks,
                                         centroids,
                                         grid.streets,
                                         projection)
water.intersects <- edgesIntersect(blocks,
                                   centroids,
                                   water,
                                   projection)

edge.weights <- rep(1, dim(blocks)[1])

rail.weights <- as.numeric(railroad.intersects)
write.csv(rail.weights, file="rail_intersects.csv", row.names=FALSE)
rail.weights <- (rail.weights - 1) * -1

highway.weights <- as.numeric(highway.intersects)
write.csv(highway.weights, file="highway_intersects.csv", row.names=FALSE)
highway.weights <- (highway.weights - 1) * -1

water.weights <- as.numeric(water.intersects)
write.csv(water.weights, file="water_intersects.csv", row.names=FALSE)
water.weights <- (water.weights - 1) * -1

grid.street.weights <- as.numeric(grid.street.intersects)
write.csv(grid.street.weights, file="grid_intersects.csv", row.names=FALSE)
grid.street.weights <- (grid.street.weights -1) * -1

edge.weights = grid.street.weights * rail.weights * highway.weights * water.weights

write.csv(edge.weights, file="edge_weights.csv", row.names=FALSE)

# Import output of GCO and plot
plabels <- read.csv("potts_labels.csv")
plot(blocks.poly,
     col = map.colors[plabels[,1]],
#     border=rgb(0,0,0,0.04),
#     border=0,
#     lwd=0.005)
     lty=0)

plot(com.areas, border="grey", add=TRUE, lwd=.1)

plot(water, col="#C0E7F3", border=0, add=TRUE)
lines(railroads, lty=2, col="tan")

plot(parks[sapply(slot(parks, "polygons"), slot, "area") > 100000,],
     col="#DBEADC", border=0, add=TRUE)



