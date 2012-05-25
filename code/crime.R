library(rjson)
library(lubridate)
library(maptools)
library(rgdal)
library(sp)
library(spgwr)

crimeBlock <- function(crime) {
  blocks <- readOGR("/home/fgregg/academic/boundaries/Census Blocks.shp",
                    "Census Blocks")
  blocks <- spTransform(blocks, CRS("+proj=utm +zone=16 +datum=NAD83"))
  attr(blocks, "data") <- attr(blocks, "data")[,c(1:4,9:16)]
  names(blocks) <- c("tract", "block", "geo.id", "geo.id.1", "type",
                     "ward", "census.year", "perimeter",
                     "community.area", "zipcode", "area", "length")

  blocks$crime <- 0

  crime.counts <- as.data.frame(table(overlay(crime, blocks)))
  names(crime.counts) <- c("block.index", "count")
  crime.counts$block.index <- as.numeric(as.character(crime.counts$block.index))

  attr(blocks, "data")[crime.counts$block.index, "crime"] <- crime.counts$count
  return(blocks)
}

censusBlocks <- function(blocks) {
  population <- read.table("dc_dec_2000_sf1_u_data1.txt",
                           sep="|", head=TRUE, skip=1)
  names(population) <- c("geo.id", "geo.id.1", "geo.summary.level",
                         "geography", "total.population")

  population$total.population[is.na(population$total.population)] <- 0
  population$geo.id.1 <- as.factor(population$geo.id.1)

  population <- population[, c("geo.id.1", "total.population")]
  blocks@data <- merge(blocks@data, population, by="geo.id.1")

  blocks <- blocks[blocks@data$area > -1,]
  return(blocks)

      }


crime.url <- 'http://data.cityofchicago.org/api/views/x2n5-8w5q/rows.json?search=Homicide'
crime.data <- fromJSON(file = crime.url)
crime <- as.data.frame(do.call(rbind, crime.data$data))
crime <- crime[,-c(1:8,21,22,25,26)]
crime <- as.data.frame(apply(crime, 2, unlist))
names(crime) <- c("case.num", "date.of.occurence", "block", "iucr",
                  "primary.description", "secondary.description",
                  "location", "arrest", "domestic", "beat", "ward",
                  "fbi.code", "latitude", "longitude")
crime <- within(crime, {
  arrest <- ifelse(arrest=="Y", TRUE, FALSE)
  domestic <- ifelse(domestic=="Y", TRUE, FALSE)
  date.of.occurence <- mdy(date.of.occurence) 
  latitude <- as.numeric(as.character(latitude))
  longitude <- as.numeric(as.character(longitude))
  location <- as.character(location)
}

                )


older.murders <- read.csv("/home/fgregg/academic/boundaries/Red-Eye-Homicide-Data_geocoded.csv")
older.murders.reduced <- data.frame(cbind(NA,
                                          as.character(older.murders$Date),
                                          NA, NA, NA, NA,
                                          as.character(older.murders$Location),
                                          NA, NA, NA, NA, NA,
                                          older.murders$Latititude,
                                          older.murders$Longitude) )
names(older.murders.reduced) <- names(crime)
older.murders.reduced$date.of.occurence <- mdy(older.murders.reduced$date.of.occurence)
crime <- rbind(crime, older.murders.reduced[older.murders.reduced$date.of.occurence < min(crime$date.of.occurence),])

crime <- within(crime, {
  latitude <- as.numeric(as.character(latitude))
  longitude <- as.numeric(as.character(longitude))
}
                )

crime <- SpatialPointsDataFrame(crime[,c("longitude","latitude")],
                                crime[,-c(13,14)],
                                proj4string=CRS("+proj=longlat"))
crime <- spTransform(crime, CRS("+proj=utm +zone=16 +datum=NAD83"))




x.lim <- c(median(crime@coords[,1]) - mad(crime@coords[,1])*5,
           median(crime@coords[,1]) + mad(crime@coords[,1])*5)
y.lim <- c(median(crime@coords[,2]) - mad(crime@coords[,2])*5,
           median(crime@coords[,2]) + mad(crime@coords[,2])*3)

city.limits <- readOGR("/home/fgregg/academic/boundaries/Kmlcityboundary.kml",
                       layer="CITYBOUNDARY",
                       p4s="+proj=longlat")
city.limits <- spTransform(city.limits,
                         CRS("+proj=utm +zone=16 +datum=NAD83")
                         )
city.grid <- expand.grid(x=seq(from = x.lim[1],
                           to= x.lim[2],
                           200),
                         y=seq(from=y.lim[1],
                           to=y.lim[2],
                           200))
coordinates(city.grid) <- ~x+y
gridded(city.grid) <- TRUE
proj4string(city.grid) <- proj4string(crime)

city.grid <- city.grid[!is.na(overlay(city.grid, city.limits)),]




plot(crime)
#inhabited.blocks <- blocks[blocks@data$total.population > 0,]

crime.counts <- as.data.frame(table(overlay(city.grid,
                                            SpatialPoints(coordinates(crime)
                                                          )
                                            )
                                    )
                              )

crime.grid <- SpatialPixelsDataFrame(city.grid@coords,
                                     data=as.data.frame(rep(0,length(city.grid)))
                                     )

names(crime.grid) <- "murders"
crime.grid@data[as.numeric(as.character(crime.counts$Var1)), "murders"] <- crime.counts$Freq



#smooth.crime.bw <- ggwr.sel(murders ~ 1,
                            data=crime.grid,
                            family = "poisson")
#print(smooth.crime.bw)

smooth.crime <- ggwr(murders ~ 1,
                     data=crime.grid,
                     family = "poisson",
                     bandwidth = 441.7113) # As of August 29, 2011

gridded(smooth.crime$SDF) <- TRUE
smooth.crime$SDF$predict <- exp(smooth.crime$SDF$X.Intercept.)
image(smooth.crime$SDF, "predict")

writeAsciiGrid(smooth.crime$SDF,
               "/home/fgregg/academic/boundaries/crime.ag",
               "predict")
