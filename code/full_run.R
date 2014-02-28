library(devtools)
library(igraph)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

ALL_CHICAGO <- TRUE

if (ALL_CHICAGO) {
    pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')
    devtools::load_all(pkg)
    NODES=chicago.blocks.poly
    EDGES = chicago.all_edges
    EDGE_LINES = chicago.edge.lines
    FILE_PREFIX = "chicago_"
} else {
    NODES=blocks.poly
    EDGES = all_edges
    EDGE_LINES = all_edges$lines
    FILE_PREFIX = ""
}    

EDGELIST <- common::edgeList(NODES, edges=EDGES)

    
csv_columns = c("TRACT_BLOC"="factor",
    "state"="factor",
    "tract"="factor",
    "block"="factor",
    "county"="factor")

chicago_blocks <- read.csv('interchange/census_data_blocks.csv',
                           colClasses= csv_columns)
alignment <- match(NODES@data$TRACT_BLOC, 
                   chicago_blocks$TRACT_BLOC)

NODES@data = data.frame(NODES@data, chicago_blocks[alignment,])

source('feature_engineering/census_differences.R')

# AGE
NODES$log_median_age <- log(NODES$P0130001)

age_diff <- absDistance("log_median_age")

write.csv(age_diff,
          file=paste("interchange/", FILE_PREFIX, "abs_age.csv", sep=""),
          row.names=FALSE)

# HOUSING
NODES$owner_occupied <- NODES$H0040002 + NODES$H0040003

NODES$rentals <- NODES$H0040004 + NODES$H0050002 + NODES$H0050003

NODES$homes <- NODES$owner_occupied + NODES$H0050004 + NODES$H0050005 + NODES$H0050006

NODES$other_vacant <- NODES$H0050007 + NODES$H0050008

market_makeup <- c("rentals",
                   "homes",
                   "other_vacant")

chi_housing <- chiDistance(market_makeup)

write.csv(chi_housing,
          file=paste("interchange/", FILE_PREFIX, "chi_housing.csv", sep=""),
          row.names=FALSE)

NODES$all_units <- NODES$H0040001 + NODES$H0050001

min_units <- minPair('all_units')

write.csv(min_units,
          file=paste("interchange/", FILE_PREFIX, "min_housing_unit.csv", sep=""),
          row.names=FALSE)

NODES$unit_density <- NODES$all_units/rgeos::gArea(NODES, byid=TRUE)

NODES$log_unit_density <- log(NODES$unit_density)

diff_units <- absDistance("log_unit_density")

write.csv(diff_units,
          file=paste("interchange/", FILE_PREFIX, "diff_housing_unit.csv", sep=""),
          row.names=FALSE)

# Families
NODES$families <- NODES$P0180005 + NODES$P0180006 + NODES$P0180003
NODES$non_families <- NODES$P0180008 + NODES$P0180009

family <- c("families",
            "non_families")

chi_family <- chiDistance(family)

write.csv(chi_family,
          file=paste("interchange/", FILE_PREFIX, "chi_family.csv", sep=""),
          row.names=FALSE)


write.csv(minPair("P0180001"),
          file=paste("interchange/", FILE_PREFIX, "min_household.csv", sep=""),
          row.names=FALSE) 

# Ethnicity
ethnicity <- c("P0040003",
               "P0050003",
               "P0050004",
               "P0050005",
               "P0050006",
               "P0050007",
               "P0050008",
               "P0050009")

chi_ethnicity <- chiDistance(ethnicity)
write.csv(chi_ethnicity,
          file=paste("interchange/", FILE_PREFIX, "chi_ethnicity.csv", sep=""),
          row.names=FALSE)



source("feature_engineering/edge_weights.R")
weights <- physicalBarriers(NODES, EDGES, EDGE_LINES,
                            chicago.railroads,
                            chicago.highways,
                            chicago.grid.streets,
                            chicago.water)

write.csv(weights$edges,
          file=paste("interchange/", FILE_PREFIX, "edges.csv", sep=""),
          row.names=FALSE)
write.csv(as.numeric(weights$highway > 0),
          file=paste("interchange/", FILE_PREFIX, "highway_intersects.csv", sep=""),
          row.names=FALSE)
write.csv(as.numeric(weights$water > 0),
          file=paste("interchange/", FILE_PREFIX, "water_intersects.csv", sep=""),
          row.names=FALSE)
write.csv(as.numeric(weights$grid_street > 0),
          file=paste("interchange/", FILE_PREFIX, "grid_intersects.csv", sep=""), row.names=FALSE)
write.csv(as.numeric(weights$rail > 0),
          file=paste("interchange/", FILE_PREFIX, "rail_intersects.csv", sep=""),
          row.names=FALSE)
write.csv(as.numeric(weights$total > 0),
          file=paste("interchange/", FILE_PREFIX, "edge_weights.csv", sep=""),
          row.names=FALSE)


source('feature_engineering/elementary_schools.R')
results <- elementarySchools(NODES, EDGES, EDGE_LINES)
write.csv(as.numeric(results$crosses > 1),
          file=paste("interchange/", FILE_PREFIX, "elementary_schools_crosses.csv", sep=""), 
          row.names=FALSE)

source('feature_engineering/high_schools.R')
results <- highSchools(NODES, EDGES, EDGE_LINES)
write.csv(as.numeric(results$crosses > 1),
          file=paste("interchange/", FILE_PREFIX, "high_schools_crosses.csv", sep=""), 
          row.names=FALSE)
write.csv(results$distances,
          file=paste("interchange/", FILE_PREFIX, "high_schools_distances.csv", sep=""), 
          row.names=FALSE)

source('feature_engineering/block_shapes.R')
angle_differences <- blockShapes(NODES, EDGES)
write.csv(angle_differences,
          file=paste("interchange/", FILE_PREFIX, "block_angles.csv", sep=""), 
          row.names=FALSE)

