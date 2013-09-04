library(rgeos)
library(spdep)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

nodes = block.groups.poly

nodes@data = data.frame(nodes@data,
                              bordering = rep(FALSE, dim(nodes@data)[1]))

predicted <- read.table("predicted_borders.csv")

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(nodes,
                           foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)

borders <- edgelist[predicted == TRUE,]


border_lines <- common::extractBorder(borders, nodes)
       
plot(nodes,
     col="black",
     border=rgb(0,0,0,0.04),
     lwd=0.01)

lines(border_lines, col="red", lwd=2)

                 



