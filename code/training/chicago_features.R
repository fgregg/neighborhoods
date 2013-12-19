pkg <- devtools::as.package('~/academic/neighborhoods/code/common')

devtools::load_all(pkg)

js_age <- read.csv('../interchange/chicago_js_age.csv')$x
js_family <- read.csv('../interchange/chicago_js_family.csv')$x
js_race <- read.csv('../interchange/chicago_js_race.csv')$x
js_housing <- read.csv('../interchange/chicago_js_housing.csv')$x

rail <- read.csv('../interchange/chicago_rail_intersects.csv')$x
highway <- read.csv('../interchange/chicago_highway_intersects.csv')$x
grid_street <- read.csv('../interchange/chicago_grid_intersects.csv')$x
water <- read.csv('../interchange/chicago_water_intersects.csv')$x

elementary_school <- read.csv('../interchange/chicago_elementary_schools_crosses.csv')$x
high_school <- read.csv('../interchange/chicago_high_schools_crosses.csv')$x

block_angle <- read.csv('../interchange/chicago_block_angles.csv')$x

block_angle <- block_angle %% pi/2 / (pi/2)

population <- read.csv('../interchange/chicago_min_population.csv')$x
sufficient_pop <- as.numeric(population > 30)

features <- data.frame(sufficient_pop,
                       js_age,
                       js_family,
                       js_race,
                       js_housing,
                       rail,
                       highway,
                       water,
                       elementary_school,
                       high_school,
                       block_angle,
                       grid_street)

M <- model.matrix(~ (sufficient_pop:(js_age + 
                                     js_family +
                                     js_race +
                                     js_housing) +
                     sufficient_pop*(rail +
                                     water +
                                     highway +
                                     grid_street +
                                     elementary_school +
                                     high_school +
                                     block_angle)),
                  data=features)

write.table(M, "chicago.model.matrix", row.names=FALSE)

