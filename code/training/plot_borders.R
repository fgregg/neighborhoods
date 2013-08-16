library(rgeos)
library(spdep)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

blocks.poly@data = data.frame(blocks.poly@data,
                              bordering = rep(FALSE, dim(blocks.poly@data)[1]))

predicted <- read.table("predicted_borders.csv")

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(blocks.poly,
                           foundInBox=rgeos::gUnarySTRtreeQuery(blocks.poly))

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)

borders <- edgelist[predicted == TRUE,]

blocks.poly@data[borders[,1], "bordering"] = TRUE
blocks.poly@data[borders[,2], "bordering"] = TRUE
       
plot(blocks.poly,
     col=c("black", "red")[as.numeric(blocks.poly@data$bordering)+1],
     border=rgb(0,0,0,0.04),
     lwd=0.01)


