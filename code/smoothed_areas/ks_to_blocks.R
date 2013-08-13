library(RMySQL)

source('utils.R')
source('common_data.R')
source('kde_methods.R')



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

# Unary Potentials of Block Labels
unary <- log(classes)
minimum.val = min(unary[is.finite(unary)])
unary[is.infinite(unary)] <- minimum.val
write.csv(unary, file="training/unary.csv", row.names=FALSE)


#map.colors = rep(c('#B2DF8A', "#A6CEE3", "#1F78B4", "#33A02C",
#                   "#FB9A99", "#E31A1C", "#FDBF6F"),
#             length.out = length(neighborhoods))

# Plot KDE
#plot(blocks.poly,
#     col = probColors(map.colors, classes),
#     border=rgb(0,0,0,0.04),
#     lwd=0.01)



                   



