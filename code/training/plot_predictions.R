library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

expit <- function(x) {
    exp(x)/(exp(x) + 1)
}

node_neighbors <-spdep::poly2nb(blocks.poly,
                                queen=FALSE,
                                foundInBox=rgeos::gUnarySTRtreeQuery(blocks.poly))
node_edgelist <- common::nb2edgelist(node_neighbors)[all_edges$spatial_lines,]

G <- igraph::graph.data.frame(node_edgelist)

segments <- read.table("predicted_borders.csv")$V1+1

hoods <- common::segmentsToHoods(segments, G)
    
hood_frequency <- table(hoods)

set.seed(as.double(Sys.time()))

map_colors = sample(sample(colors()))

plot(blocks.poly,
     col=rgb(t(col2rgb(map_colors[hoods %% 657 + 1])/255),
         alpha=ifelse(hood_frequency[hoods] < 10, 0.05, 1)),
     border="transparent")
