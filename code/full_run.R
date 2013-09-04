library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

NODES=populated.blocks


source("smoothed_areas/ks_to_blocks.R")
unary <- labelNodes(NODES)
write.csv(unary, file="interchange/unary.csv", row.names=FALSE)

source("feature_engineering/edge_weights.R")
weights <- physicalBarriers(NODES)
write.csv(weights$edges, file="interchange/edges.csv", row.names=FALSE)
write.csv(weights$highway, file="interchange/highway_intersects.csv", row.names=FALSE)
write.csv(weights$water, file="interchange/water_intersects.csv", row.names=FALSE)
write.csv(weights$grid_street, file="interchange/grid_intersects.csv", row.names=FALSE)
write.csv(weights$rail, file="interchange/rail_intersects.csv", row.names=FALSE)
write.csv(weights$total, file="interchange/edge_weights.csv", row.names=FALSE)

system('python2.6 smoothed_areas/potts.py')

source('feature_engineering/zoning.R')
crosses <- zoningChanges(NODES)
write.csv(as.numeric(crosses > 1),
          file="interchange/zoning_crosses.csv", row.names=FALSE)


source('feature_engineering/elementary_schools.R')
crosses <- elementarySchools(NODES)
write.csv(as.numeric(crosses > 1),
          file="interchange/elementary_schools_crosses.csv", row.names=FALSE)

source('feature_engineering/high_schools.R')
crosses <- elementarySchools(NODES)
write.csv(as.numeric(crosses > 1),
          file="interchange/high_schools_crosses.csv", row.names=FALSE)

source('feature_engineering/census_differences.R')
differences <- censusDifferences(NODES)
write.csv(differences$race,
          file="interchange/js_race.csv", row.names=FALSE)
write.csv(differences$age,
          file="interchange/js_age.csv", row.names=FALSE)
write.csv(differences$family,
          file="interchange/js_family.csv", row.names=FALSE)
write.csv(differences$housing,
          file="interchange/js_housing.csv", row.names=FALSE)

source('feature_engineering/line_graph.R')
line_graph <- lineGraph(NODES)
igraph::write.graph(line_graph,
                    "interchange/line_graph_edges.txt",
                    format="edgelist")

source('feature_engineering/borders.R')
border <- borders(NODES)
write.csv(border,
          file="interchange/border.csv", row.names=FALSE)

system('python2.6 training/train_ssvm.py')
