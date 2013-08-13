source('common_data.R')

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
