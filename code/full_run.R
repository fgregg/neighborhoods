library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

NODES=blocks.poly


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
write.csv(weights$highway.distance, file="interchange/highway_distance.csv", row.names=FALSE)
write.csv(weights$water.distance, file="interchange/water_distance.csv", row.names=FALSE)
write.csv(weights$grid.distance, file="interchange/grid_distance.csv", row.names=FALSE)
write.csv(weights$rail.distance, file="interchange/rail_distance.csv", row.names=FALSE)


system('python2.6 smoothed_areas/potts.py')

source('feature_engineering/zoning.R')
crosses <- zoningChanges(NODES)
write.csv(as.numeric(crosses > 1),
          file="interchange/zoning_crosses.csv", row.names=FALSE)


source('feature_engineering/elementary_schools.R')
results <- elementarySchools(NODES)
write.csv(as.numeric(results$crosses > 1),
          file="interchange/elementary_schools_crosses.csv", row.names=FALSE)
write.csv(results$distances,
          file="interchange/elementary_schools_distances.csv", row.names=FALSE)

source('feature_engineering/high_schools.R')
results <- highSchools(blocks.poly)
write.csv(as.numeric(results$crosses > 1),
          file="interchange/high_schools_crosses.csv", row.names=FALSE)
write.csv(results$distances,
          file="interchange/high_schools_distances.csv", row.names=FALSE)

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
write.csv(differences$population,
          file="interchange/min_population.csv", row.names=FALSE)


if (identical(NODES, blocks.poly) || identical(NODES, populated.blocks)) {
  source('feature_engineering/block_shapes.R')
  angle_differences <- blockShapes(NODES)
  write.csv(angle_differences,
            file="interchange/block_angles.csv", row.names=FALSE)
}

source('feature_engineering/edge_length.R')
lengths <- edgeLengths(NODES)
write.csv(lengths, file="interchange/edge_lengths.csv", row.names=FALSE)



source('feature_engineering/line_graph.R')
line_graph <- lineGraph(NODES)
igraph::write.graph(line_graph,
                    "interchange/line_graph_edges.txt",
                    format="edgelist")

source('feature_engineering/borders.R')
results <- borders(NODES)
write.csv(results$border,
          file="interchange/border.csv", row.names=FALSE)



system('python2.6 training/train_ssvm.py')
