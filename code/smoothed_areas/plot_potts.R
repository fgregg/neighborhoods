library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


# Shape Files For Context
com.areas <- readOGR("../../admin_areas/chicomm.shp",
                     layer="chicomm",
                     p4s="+proj=longlat")
com.areas <- spTransform(com.areas,
                         CRS(projection)
                         )
parks <- readOGR("../../barriers/Kmlchicagoparks.kml",
                 layer = "Chicago Parks")
parks <- spTransform(parks,
                     CRS(projection)
                     )


# Import output of GCO and plot
plabels <- read.csv("potts_labels.csv")

map.colors = rep(c('#B2DF8A', "#A6CEE3", "#1F78B4", "#33A02C",
                   "#FB9A99", "#E31A1C", "#FDBF6F"),
             length.out = (dim(unique(plabels))[1]+5))

pdf("potts.pdf")

plot(blocks.poly,
     col = map.colors[plabels[,1]],
#     border=rgb(0,0,0,0.04),
#     border=0,
#     lwd=0.005)
     lty=0)

plot(com.areas, border="grey", add=TRUE, lwd=.1)

#plot(water, col="#C0E7F3", border=FALSE, add=TRUE)

lines(railroads, lty=2, col="tan")

#plot(parks[sapply(slot(parks, "polygons"), slot, "area") > 100000,],
#     col="#DBEADC", border=FALSE, add=TRUE)

dev.off()
