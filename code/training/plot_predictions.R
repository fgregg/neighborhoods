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

hoods <- rep(0, length(segments))
color = 1

for (hood_label in unique(segments)) {
    G1 <- delete.vertices(G, V(G)[segments != hood_label])
    GLIST <- decompose.graph(G1)

    for (i in 1:length(GLIST)) {
        hoods[as.numeric(V(GLIST[[i]])$name)] <- color
        color <- color + 1
    }
}        

hood_frequency <- table(hoods)

plot(blocks.poly, col=rgb(t(col2rgb(sample(colors())[hoods %% 657 + 1])/255), alpha=ifelse(hood_frequency[hoods] < 10, 0.05, 1)), border="transparent")
