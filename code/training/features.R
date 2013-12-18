pkg <- devtools::as.package('~/academic/neighborhoods/code/common')


devtools::load_all(pkg)

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

block_angle <- read.csv('../interchange/block_angles.csv')$x

block_angle <- block_angle %% pi/2 / (pi/2)

population <- read.csv('../interchange/min_population.csv')$x
sufficient_pop <- as.numeric(population > 30)

node_labels <- read.csv('../interchange/border.csv')$x

features <- data.frame(sufficient_pop,
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
                       block_angle,
                       node_labels,
                       grid_street)

M <- model.matrix(node_labels ~ sufficient_pop:(
                                  js_age + 
                                  js_family +
                                  js_race +
                                  js_housing) +
                                sufficient_pop*( 
                                  rail +
                                  water +
                                  #zoning +
                                  elementary_school +
                                  high_school +
                                  block_angle),
                  data=features)

write.table(M, "model.matrix", row.names=FALSE)

