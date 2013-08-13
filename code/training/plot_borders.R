library(rgdal)
library(rgeos)
library(spdep)
library(igraph)

source('utils.R')

source('common_data.R')

blocks.poly@data = data.frame(blocks.poly@data,
                              bordering = rep(FALSE, dim(blocks.poly@data)[1]))

predicted <- read.table("predicted_borders.csv")

# Topology of Block Connectivity
neighbors <-poly2nb(blocks.poly,
                    foundInBox=gUnarySTRtreeQuery(blocks.poly))

# Calculate 'edge features' will be node features in training
edgelist <- nb2edgelist(neighbors)


ifelse(predicted == 1,
       blocks.poly@data[c(edgelist[,1],
                          edgelist[,2]), "bordering"] = TRUE,
       blocks.poly@data[c(edgelist[,1],
                          edgelist[,2]), "bordering"] = FALSE)
       
plot(blocks.poly, col=colors()[blocks.poly@data$bordering]) # check alignment of potts labels
