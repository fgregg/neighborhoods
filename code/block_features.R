
library(rgdal)
library(rgeos)
library(spdep)
library(igraph)

base_url <- "http://api.census.gov/data/2010/sf1?key=ac94ba69718a7e1da4f89c6d218b8f6b5ae9ac49"


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


write(c(), file="edge_features.txt")

for (i in 1:length(neighbors)) {
  label_i <- blocks.poly@data[i, "label"]
  p <- blocks.poly@data[i, c("P0040003", "P0050003", "P0050004",
                             "P0050005", "P0050006", "P0050007",
                             "P0050008", "P0050009")] + 1
  p <- p/(blocks.poly@data[i, "P0040001"] + 8)
  for (j in neighbors[[i]]) {
    label_j <- blocks.poly@data[j, "label"]
    q <- blocks.poly@data[j, c("P0040003", "P0050003", "P0050004",
                             "P0050005", "P0050006", "P0050007",
                             "P0050008", "P0050009")] + 1
    q <- q/(blocks.poly@data[j, "P0040001"] + 8)

    # Jensen-Shannon divergence
    # http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
    m <- 0.5 * (p + q)
    js <- (0.5 * sum(unlist(ifelse(p==0, 0, log(p/m)*p)))
           + 0.5 * sum(unlist(ifelse(q==0, 0, log(q/m)*q))))
    border <- ifelse(label_i == label_j, 0, 1)
    write(c(i, j, js, border), file="edge_features.txt", append=TRUE)
  }
}

edgelist <- nb2edgelist(neighbors)

primal_graph <- graph.data.frame(edgelist, directed=FALSE)
line_graph <- line.graph(primal_graph)

write.graph(line_graph,
            "line_graph_edges.txt",
            format="edgelist")


