library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')
devtools::load_all(pkg)

community_area <- readOGR("../../admin_areas/Kmlcommunityareas.kml",
                     layer = "COMMUNITYAREA")
community_area <- spTransform(community_area,
                              CRS(projection)
                              )


parks <- readOGR("../../barriers/Kmlchicagoparks.kml",
                     layer = "Chicago Parks")

parks <- spTransform(parks,
                     CRS(projection)
                     )


expit <- function(x) {
    exp(x)/(exp(x) + 1)
}

node_neighbors <-spdep::poly2nb(chicago.blocks.poly,
                                queen=FALSE,
                                foundInBox=rgeos::gUnarySTRtreeQuery(chicago.blocks.poly))
node_edgelist <- common::nb2edgelist(node_neighbors)[chicago.all_edges$spatial_lines,]

G <- igraph::graph.data.frame(node_edgelist)

segments <- read.table("predicted_chicago.csv")$V1+1

hoods <- common::segmentsToHoods(segments, G)

hood_frequency <- table(hoods)

map_colors <- sample(colors())[order(hood_frequency) %% 657 + 1]

pdf("predicted_chicago_neighborhoods.pdf")
plot(chicago.blocks.poly,
     col=rgb(t(col2rgb(map_colors[hoods %% 657 + 1])/255),
       alpha=ifelse(hood_frequency[hoods] < 10, 0.05, 1)),
     border="transparent")
dev.off()

pdf("predicted_chicago_neighborhoods_ca.pdf")
plot(chicago.blocks.poly,
     col=rgb(t(col2rgb(map_colors[hoods %% 657 + 1])/255),
       alpha=ifelse(hood_frequency[hoods] < 10, 0.05, 1)),
     border="transparent")
plot(parks[sapply(slot(parks, "polygons"), slot, "area") > 0.00001,],
     col="#DBEADC",
     border="transparent",
     add=TRUE)
plot(community_area,
     add=TRUE)
dev.off()


