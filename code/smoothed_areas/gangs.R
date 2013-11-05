library(devtools)
library(RCurl)
library(rgdal)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

range <- cbind(c(1156695, 1183642), c(1913142, 1936155))

gang_cards.url <- "https://opendata.socrata.com/resource/6gqi-4u4s.csv?$select=gang,latitude,longitude,faction&$where=gang IS NOT NULL"
gang_cards.csv <- RCurl::getURLContent(URLencode(gang_cards.url))
gang_cards <- read.csv(textConnection(gang_cards.csv))
gang_cards$Latitude <- jitter(gang_cards$Latitude, factor=300)
gang_cards$Longitude <- jitter(gang_cards$Longitude, factor=300)
gangs <- sp::SpatialPointsDataFrame(coords = gang_cards[, c("Longitude", "Latitude")],
                                    data = data.frame(gang_cards$GANG),
                                    proj4string = CRS("+proj=longlat")
                                    )
names(gangs) <- c("gang")
gangs <- sp::spTransform(gangs, CRS(projection))

sufficient_gangs <- names(table(gangs$gang)[table(gangs$gang) > 30])

centroids <- sp::coordinates(nodes)
                                      # Estimate KDE

C = common::trainKDE(gangs,
  sufficient_gangs,
  centroids,
  range,
  common::ks.pi,
  0.0000012, id="gang")


map.colors = rep(c('#B2DF8A', "#A6CEE3", "#1F78B4", "#33A02C",
                 "#FB9A99", "#E31A1C", "#FDBF6F"),
                 length.out = 10)

map.colors = c("red", "blue", "green", "yellow", "purple")


pdf("gangs.pdf")
common::basePlot(range)


plot(blocks.poly,
     col = common::probColors(map.colors, C),
     #border=rgb(0,0,0,0.04),
     border=0,
     lwd=0.01,
     add=TRUE)

water <- readOGR("../../barriers/Kmlchicagowaterfeatures.kml",
                 layer = "WATER_FEATURES")

water <- spTransform(water,
                     CRS(projection)
                     )
plot(water, col="#C0E7F3", border=0, add=TRUE)

parks <- readOGR("../../barriers/Kmlchicagoparks.kml",
                 layer = "Chicago Parks")

parks <- spTransform(parks,
                     CRS(projection)
                     )
plot(parks, col="forest green", border=0, add=TRUE)

com.areas <- readOGR("../../admin_areas/chicomm.shp",
                     layer="chicomm",
                     p4s="+proj=longlat")
com.areas <- spTransform(com.areas,
                         CRS(projection)
                         )
plot(com.areas, add=TRUE, border="grey", lwd=0.1)



subset_gangs <- gangs[gangs$gang %in% sufficient_gangs,]

hood.medoids <- aggregate(subset_gangs@coords, 
                          by=list(subset_gangs$gang), 
                          median)
dev.off()

#names(hood.medoids) <- c("gang", "x", "y")
#text(hood.medoids[, c('x', 'y')],
#     labels = hood.medoids$gang,
#     col= "#00000063",
#     cex=0.5)
