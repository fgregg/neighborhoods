library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')
devtools::load_all(pkg)

NODES=chicago.blocks.poly
EDGES = chicago.all_edges
FILE_PREFIX = "chicago"

source("feature_engineering/edge_weights.R")
weights <- physicalBarriers(NODES, EDGES,
                            chicago.railroads,
                            chicago.highways,
                            chicago.grid.streets,
                            chicago.water)

write.csv(weights$edges,
          file=paste("interchange/", FILE_PREFIX, "edges.csv", sep=""),
          row.names=FALSE)
write.csv(weights$highway,
          file=paste("interchange/", FILE_PREFIX, "highway_intersects.csv", sep=""),
          row.names=FALSE)
write.csv(weights$water,
          file=paste("interchange/", FILE_PREFIX, "water_intersects.csv", sep=""),
          row.names=FALSE)
write.csv(weights$grid_street,
          file=paste("interchange/", FILE_PREFIX, "grid_intersects.csv", sep=""), row.names=FALSE)
write.csv(weights$rail,
          file=paste("interchange/", FILE_PREFIX, "rail_intersects.csv", sep=""),
          row.names=FALSE)
write.csv(weights$total,
          file=paste("interchange/", FILE_PREFIX, "edge_weights.csv", sep=""),
          row.names=FALSE)
write.csv(weights$highway.distance,
          file=paste("interchange/", FILE_PREFIX, "highway_distance.csv", sep=""),
          row.names=FALSE)
write.csv(weights$water.distance,
          file=paste("interchange/", FILE_PREFIX, "water_distance.csv", sep=""),
          row.names=FALSE)
write.csv(weights$grid.distance,
          file=paste("interchange/", FILE_PREFIX, "grid_distance.csv", sep=""),
          row.names=FALSE)
write.csv(weights$rail.distance,
          file=paste("interchange/", FILE_PREFIX, "rail_distance.csv", sep=""), 
          row.names=FALSE)


source('feature_engineering/elementary_schools.R')
results <- elementarySchools(NODES, EDGES)
write.csv(as.numeric(results$crosses > 1),
          file=paste("interchange/", FILE_PREFIX, "elementary_schools_crosses.csv", sep=""), 
          row.names=FALSE)
write.csv(results$distances,
          file=paste("interchange/", FILE_PREFIX, "elementary_schools_distances.csv", sep=""), 
          row.names=FALSE)

source('feature_engineering/high_schools.R')
results <- highSchools(NODES, EDGES)
write.csv(as.numeric(results$crosses > 1),
          file=paste("interchange/", FILE_PREFIX, "high_schools_crosses.csv", sep=""), 
          row.names=FALSE)
write.csv(results$distances,
          file=paste("interchange/", FILE_PREFIX, "high_schools_distances.csv", sep=""), 
          row.names=FALSE)

source('feature_engineering/census_differences.R')

differences <- censusDifferences(NODES, cached_edges=EDGES)
write.csv(differences$race,
          file=paste("interchange/", FILE_PREFIX, "js_race.csv", sep=""), 
          row.names=FALSE)
write.csv(differences$age,
          file=paste("interchange/", FILE_PREFIX, "js_age.csv", sep=""), 
          row.names=FALSE)
write.csv(differences$family,
          file=paste("interchange/", FILE_PREFIX, "js_family.csv", sep=""), 
          row.names=FALSE)
write.csv(differences$housing,
          file=paste("interchange/", FILE_PREFIX, "js_housing.csv", sep=""), 
          row.names=FALSE)
write.csv(differences$population,
          file=paste("interchange/", FILE_PREFIX, "min_population.csv", sep=""),
          row.names=FALSE)

source('feature_engineering/block_shapes.R')
angle_differences <- blockShapes(NODES, EDGES)
write.csv(angle_differences,
          file=paste("interchange/", FILE_PREFIX, "block_angles.csv", sep=""), 
          row.names=FALSE)

