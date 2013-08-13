library(maptools)
library(rgdal)
library(gstat)
library(spgwr)
library(RMySQL)

standardize <- function(x) {
  mean.x <- mean(x[!is.infinite(x)])
  sd.x <- sd(x[!is.infinite(x)])
  return((x - mean.x)/sd.x)
}

truncate <- function(x, sd) {
  test.x <- standardize(x)
  ifelse(test.x < -1 * sd,
         NA,
         ifelse(test.x > sd,
                NA,
                x
                )
         )
}


greyscale <- function(x, sd) {
  test.x <- standardize(x)
  ifelse(test.x < -1 * sd,
         0,
         ifelse(test.x > sd,
                1,
                (test.x + sd)/(2 * sd)
                )
         )
}

price.query <- "
select distinct
X(lat_long) as x,
Y(lat_long) as y,
price,
bedrooms
from location,
((select
  location_id,
  price,
  bedrooms
  from price, label
  where price.listing_id = label.listing_id)
 as d1)
where location.location_id = d1.location_id"


con <- dbConnect(MySQL(), dbname="neighborhood")
prices <- dbGetQuery(con, price.query)
dbDisconnect(con)

prices <- SpatialPointsDataFrame(coords=prices[,c(2,1)],
                                 prices[,-c(1,2)],
                                 proj4string=CRS("+proj=latlong")
                                 )
prices <- spTransform(prices,
                      CRS("+proj=utm +zone=16 +datum=NAD83")
                      )

prices <- prices[prices@coords[,2] != 4636435.19273896,]

x.lim <- c(median(prices@coords[,1]) - mad(prices@coords[,1])*3,
           median(prices@coords[,1]) + mad(prices@coords[,1])*3)
y.lim <- c(median(prices@coords[,2]) - mad(prices@coords[,2])*5,
           median(prices@coords[,2]) + mad(prices@coords[,2])*3)
x.range <- as.integer(x.lim)
y.range <- as.integer(y.lim)

prices <- prices[prices@coords[,1] >= x.lim[1] &
                 prices@coords[,1] <= x.lim[2] &
                 prices@coords[,2] >= y.lim[1] &
                 prices@coords[,2] <= y.lim[2], ]

prices$normalized.price <- prices$price/prices$bedrooms
prices$log.prices.trunc <- truncate(log(prices$normalized.price), 3)
bounded.prices <- prices[!is.na(prices@data$log.prices.trunc),]

# Chicago Grid
city.limits <- readOGR("/home/fgregg/academic/boundaries/Kmlcityboundary.kml",
                     layer="CITYBOUNDARY",
                     p4s="+proj=longlat")
city.limits <- spTransform(city.limits,
                         CRS("+proj=utm +zone=16 +datum=NAD83")
                         )
city.grid <- expand.grid(x=seq(from = x.range[1],
                         to= x.range[2],
                         100),
                   y=seq(from=y.range[1],
                         to=y.range[2],
                         100))
coordinates(city.grid) <- ~x+y
gridded(city.grid) <- TRUE
proj4string(city.grid) <- proj4string(prices)

city.grid <- city.grid[!is.na(overlay(city.grid, city.limits)),]


smooth.prices <- ggwr(log.prices.trunc ~ 1,
                      data=bounded.prices,
                      adapt = 30/dim(bounded.prices)[1],
# according to ggwr.sel, 0.55 is optimal
#                      adapt=.055,
                      fit.points = city.grid)

writeAsciiGrid(smooth.prices$SDF,
               "/home/fgregg/academic/boundaries/rent.ag",
               "X.Intercept.")

pdf("~/academic/boundaries/rent.pdf", paper="letter", pointsize=5)
image(smooth.prices$SDF, "X.Intercept.", col=topo.colors(100))
points(prices, pch='.', cex=.3)
contour(smooth.prices$SDF, "X.Intercept.", levels=log(c(500, 600, 750, 1000, 1200, 1500)), labels=c('$500', '$600', '$750', '$1,000', '$1,200', '$1,500'), add=TRUE, col="brown")
text(450000, 4650000, paste(dim(prices)[1], "locations"))
dev.off()
