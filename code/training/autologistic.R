library(glmmBUGS)
library(R2jags)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

source('../feature_engineering/line_graph.R')
source('getInits.R')

js_age <- read.csv('../interchange/js_age.csv')$x
js_family <- read.csv('../interchange/js_family.csv')$x
js_race <- read.csv('../interchange/js_race.csv')$x
js_housing <- read.csv('../interchange/js_housing.csv')$x

rail <- read.csv('../interchange/rail_intersects.csv')$x
#rail <- read.csv('../interchange/rail_distance.csv')$x^(1/2)
highway <- read.csv('../interchange/highway_intersects.csv')$x
#highway <- read.csv('../interchange/highway_distance.csv')$x^(1/2)
grid_street <- read.csv('../interchange/grid_intersects.csv')$x
#grid_street <- read.csv('../interchange/grid_distance.csv')$x^(1/2)
water <- read.csv('../interchange/water_intersects.csv')$x
#water <- read.csv('../interchange/water_distance.csv')$x^(1/2)

zoning <- read.csv('../interchange/zoning_crosses.csv')$x
elementary_school <- read.csv('../interchange/elementary_schools_crosses.csv')$x
#elementary_school <- read.csv('../interchange/elementary_schools_distances.csv')$x
#elementary_school <- elementary_school^(1/2)
high_school <- read.csv('../interchange/high_schools_crosses.csv')$x
#high_school <- read.csv('../interchange/high_schools_distances.csv')$x
#high_school <- high_school^(1/2)

population <- read.csv('../interchange/min_population.csv')$x
sufficent_pop <- as.numeric(population > 30)

node_labels <- read.csv('../interchange/border.csv')$x

# Spatial Lags
adjacent_edges <- adjacentEdges(blocks.poly)
attr(adjacent_edges, 'region.id') <- as.character(1:length(node_labels))

group <- as.character(1:length(node_labels))

features <- data.frame(sufficent_pop,
                       js_age,
                       js_family,
                       js_race,
                       js_housing,
                       rail,
                       highway,
                       water,
                       zoning,
                       elementary_school,
                       high_school,
                       node_labels,
                       group)


forBugs <- glmmBUGS::glmmBUGS(node_labels ~ rail,
                              data=features,
                              effects="group",
                              family="binomial",
                              spatial=adjacent_edges,
                              spatialEffect="group",
                              modelFile="model.bug")

startingValues = forBugs$startingValues


#results <- jags(forBugs$ragged,
                getInits,
                parameters.to.save = names(getInits()),
                model.file="model.bug",
                n.chain=3,
                n.iter=1000, 
                n.burnin=100,
                n.thin=10,
                working.directory=getwd()) 
