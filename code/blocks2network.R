library(maptools)
library(rgdal)
library(spdep)
library(rgeos)
library(igraph)
library(RMySQL)
library(ks)

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


nb2edgelist <- function(nb) {
  el <- c()
  for (i in 1:length(nb)) {
    neighbors <- blocks[[i]]
    new.neighbors <- neighbors[neighbors > i]
    if (length(new.neighbors) > 0) {
      el <- rbind(el, cbind(i, new.neighbors))
    }
  }
  return(el)
}

# Projection and Bounding Box Range
projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Lat/Lon of Bounding Box
range = cbind(c(-87.7, -87.6), c(41.89,41.98))
range = project(range, projection)

# Load Block Data
blocks.poly <- readOGR("../admin_areas/CensusBlockTIGER2010.shp",
                       "CensusBlockTIGER2010")

# Subset Blocks
centroids <- coordinates(blocks.poly)
blocks.poly <- blocks.poly[centroids[,1] > range[1,1] &
                           centroids[,1] < range[2,1] &
                           centroids[,2] > range[1,2] &
                           centroids[,2] < range[2,2],]

centroids <- coordinates(blocks.poly)
colnames(centroids) <- c("x", "y")

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

map.colors = rep(c('#B2DF8A', "#A6CEE3", "#1F78B4", "#33A02C",
                   "#FB9A99", "#E31A1C", "#FDBF6F"),
             length.out = length(neighborhoods))

classes = trainKDE(listings,
                   neighborhoods,
                   centroids,
                   range, 
                   ks.pi,
                   0.0000015)

plot(blocks.poly,
     col = probColors(map.colors, classes),
     border=rgb(0,0,0,0.04),
     lwd=0.01)
                    
#blocks <-poly2nb(blocks.poly,
#                 foundInBox=gUnarySTRtreeQuery(blocks.poly))


#blocks <- nb2edgelist(blocks)
#blocks <- graph.edgelist(blocks, directed=FALSE)

