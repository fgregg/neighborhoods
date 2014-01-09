library(rgeos)
library(phyclust)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')
devtools::load_all(pkg)

community_area <- readOGR("../../admin_areas/Kmlcommunityareas.kml",
                     layer = "COMMUNITYAREA")
community_area <- spTransform(community_area,
                              CRS(projection)
                              )

node_neighbors <-spdep::poly2nb(chicago.blocks.poly,
                                queen=FALSE,
                                foundInBox=rgeos::gUnarySTRtreeQuery(chicago.blocks.poly))

node_edgelist <- common::nb2edgelist(node_neighbors)[chicago.all_edges$spatial_lines,]

G <- igraph::graph.data.frame(node_edgelist)

segments <- read.table("../training/predicted_chicago.csv")$V1+1

hoods <- common::segmentsToHoods(segments, G)

blocks_ca <- apply(rgeos::gContains(community_area,
                                    chicago.blocks.poly,
                                    byid=TRUE),
                   1,
                   which)

blocks_ca <- as.numeric(blocks_ca)
missing <- is.na(x)

phyclust::RRand(hoods[!missing], x[!missing])
