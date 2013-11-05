library(rgeos)
library(spdep)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

nodes = populated.blocks

nodes@data = data.frame(nodes@data,
                              bordering = rep(FALSE, dim(nodes@data)[1]))

predicted <- read.table("../interchange/border.csv", skip=1)

feature <- read.table("../interchange/water_intersects.csv", skip=1)

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(nodes,
                           foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)


uncovered_borders <- edgelist[predicted == TRUE & feature == FALSE,]
uncovered_border_lines <- common::extractBorder(borders, nodes)

uncovered_feature_lines <- common::extractBorder(edgelist[feature == TRUE & predicted == FALSE,], nodes)

covered_lines <- common::extractBorder(edgelist[feature == TRUE & predicted == TRUE,], nodes)

png("water.png")

par(bg="grey")
plot(nodes,
     col="black",
     border=rgb(0,0,0,0.04),
     lwd=0.01)

lines(uncovered_border_lines, col="green", lwd=2)
lines(uncovered_feature_lines, col="blue", lwd=2)
lines(covered_lines, col="cyan", lwd=2)

dev.off()                 



