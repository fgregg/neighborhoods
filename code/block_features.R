
library(rgdal)
library(rgeos)
library(spdep)
library(igraph)

nb2edgelist <- function(nb) {
  el <- c()
  for (i in 1:length(nb)) {
    neighbors <- nb[[i]]
    new.neighbors <- neighbors[neighbors > i]
    if (length(new.neighbors) > 0) {
      el <- rbind(el, cbind(i, new.neighbors))
    }
  }
  return(el)
}


# Load Block Data
blocks.poly <- readOGR("../admin_areas/CensusBlockTIGER2010.shp",
                       "CensusBlockTIGER2010")

# Map Projection
projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Lat/Lon of Bounding Box
range = cbind(c(-87.7, -87.6), c(41.89,41.98))
range = project(range, projection)


# Subset Blocks
centroids <- coordinates(blocks.poly)
blocks.poly <- blocks.poly[centroids[,1] > range[1,1] &
                           centroids[,1] < range[2,1] &
                           centroids[,2] > range[1,2] &
                           centroids[,2] < range[2,2],]


chicago_blocks <- read.csv("census_data_blocks.csv",
                           colClasses=c("TRACT_BLOC"="factor",
                                        "state"="factor",
                                        "tract"="factor",
                                        "block"="factor",
                                        "county"="factor"))
race <-  c("P0040003", "P0050003", "P0050004",
           "P0050005", "P0050006", "P0050007",
           "P0050008", "P0050009")

chicago_blocks[, race] <- ((1 + chicago_blocks[, race])
                           /
                           (chicago_blocks[, "P0040001"] + 8))

alignment <- match(blocks.poly@data$TRACT_BLOC, 
                   chicago_blocks$TRACT_BLOC)


# Merge Census data with Geo Spatial Data Frame
blocks.poly@data = data.frame(blocks.poly@data, chicago_blocks[alignment,])

# Merge labels 
plabels <- read.csv("potts_labels.csv")
blocks.poly@data = data.frame(blocks.poly@data, plabels)


# Topology of Block Connectivity
neighbors <-poly2nb(blocks.poly,
                    foundInBox=gUnarySTRtreeQuery(blocks.poly))
# plot(blocks.poly, col=colors()[blocks.poly@data$label]) # check alignment of potts labels

edgelist <- nb2edgelist(neighbors)


p <- blocks.poly@data[edgelist[,1], race]
q <- blocks.poly@data[edgelist[,2], race]

m <- 0.5 * (p + q)

js <- (0.5 * rowSums(log(p/m) * p)
       +
       0.5 * rowSums(log(q/m)*q))

labels <- (blocks.poly@data[edgelist[,1], "label"]
           != 
           blocks.poly@data[edgelist[,2], "label"])
labels <- as.numeric(labels)
           
write.table(data.frame(edgelist, js, labels),
            file="edge_features.txt",
            row.names=FALSE)

primal_graph <- graph.data.frame(edgelist, directed=FALSE)
line_graph <- line.graph(primal_graph)

write.graph(line_graph,
            "line_graph_edges.txt",
            format="edgelist")
