library(rgdal)
library(rgeos)
library(spdep)
library(igraph)

source('utils.R')

source('common_data.R')

##### Load Data
chicago_blocks <- read.csv("census_data_blocks.csv",
                           colClasses=c("TRACT_BLOC"="factor",
                                        "state"="factor",
                                        "tract"="factor",
                                        "block"="factor",
                                        "county"="factor"))
# P0040003 : Hispanic or Latino
# P0050003 : Not Hispanic or Latino : White alone
# P0050004 : Not Hispanic or Latino : Black Alone
# P0050005 : Not Hispanic or Latino : American Indian and Alaska Native alone
# P0050006 : Not Hispanic or Latino : Asian alone
# P0050007 : Not Hispanic or Latino : Native Hawaiian and Other Pacific Islander alone
# P0050008 : Not Hispanic or Latino : Some other race alone
# P0050009 : Not Hispanic or Latino : Two or more races
# P0040001 : Total population

race <-  c("P0040003", "P0050003", "P0050004",
           "P0050005", "P0050006", "P0050007",
           "P0050008", "P0050009")
chicago_blocks[, race] <- ((1 + chicago_blocks[, race])
                           /
                           (chicago_blocks[, "P0040001"] + 8))

# Merge Census data with Geo Spatial Data Frame
alignment <- match(blocks.poly@data$TRACT_BLOC, 
                   chicago_blocks$TRACT_BLOC)
blocks.poly@data = data.frame(blocks.poly@data, chicago_blocks[alignment,])

# Merge labels 
plabels <- read.csv("potts_labels.csv")
blocks.poly@data = data.frame(blocks.poly@data, plabels)

##### Calculate Features


# Topology of Block Connectivity
neighbors <-poly2nb(blocks.poly,
                    foundInBox=gUnarySTRtreeQuery(blocks.poly))
# plot(blocks.poly, col=colors()[blocks.poly@data$label]) # check alignment of potts labels

# Calculate 'edge features' will be node features in training
edgelist <- nb2edgelist(neighbors)

# Jensen Shannon Inequality for race distribution
p <- blocks.poly@data[edgelist[,1], race]
q <- blocks.poly@data[edgelist[,2], race]
m <- 0.5 * (p + q)
js <- (0.5 * rowSums(log(p/m) * p)
       +
       0.5 * rowSums(log(q/m)*q))

# Is the edge a border between two different 'hoods
border <- (blocks.poly@data[edgelist[,1], "label"]
           != 
           blocks.poly@data[edgelist[,2], "label"])
border <- as.numeric(border)
           
write.table(data.frame(edgelist, js, border),
            file="edge_features.txt",
            row.names=FALSE)

# Line graph, the 'edges' between the edges
primal_graph <- graph.data.frame(edgelist, directed=FALSE)
line_graph <- line.graph(primal_graph)

write.graph(line_graph,
            "line_graph_edges.txt",
            format="edgelist")
