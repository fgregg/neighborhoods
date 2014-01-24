pkg <- devtools::as.package('~/academic/neighborhoods/code/common')


devtools::load_all(pkg)

js_age <- read.csv('../interchange/js_age.csv')$x
js_family <- read.csv('../interchange/js_family.csv')$x
js_race <- read.csv('../interchange/js_race.csv')$x
js_housing <- read.csv('../interchange/js_housing.csv')$x

cosine_ethnicity <- read.csv('../interchange/cosine_ethnicity.csv')$x
cosine_family <- read.csv('../interchange/cosine_family.csv')$x
cosine_housing <- read.csv('../interchange/cosine_housing.csv')$x
cosine_age <- read.csv('../interchange/cosine_age.csv')$x

cosine_ethnicity[is.na(cosine_ethnicity)] <- 100
cosine_family[is.na(cosine_family)] <- 100
cosine_housing[is.na(cosine_housing)] <- 100
cosine_age[is.na(cosine_age)] <- 100

chi_ethnicity <- read.csv('../interchange/chi_ethnicity.csv')$x
chi_family <- read.csv('../interchange/chi_family.csv')$x
chi_housing <- read.csv('../interchange/chi_housing.csv')$x
chi_age <- read.csv('../interchange/chi_age.csv')$x

chi_ethnicity[is.na(chi_ethnicity)] <- 100
chi_family[is.na(chi_family)] <- 100
chi_housing[is.na(chi_housing)] <- 100
chi_age[is.na(chi_age)] <- 100




rail <- read.csv('../interchange/rail_intersects.csv')$x
highway <- read.csv('../interchange/highway_intersects.csv')$x
grid_street <- read.csv('../interchange/grid_intersects.csv')$x
water <- read.csv('../interchange/water_intersects.csv')$x

elementary_school <- read.csv('../interchange/elementary_schools_crosses.csv')$x
high_school <- read.csv('../interchange/high_schools_crosses.csv')$x
block_angle <- read.csv('../interchange/block_angles.csv')$x

block_angle <- block_angle %% pi/2 / (pi/2)

population <- read.csv('../interchange/min_population.csv')$x
households <- read.csv('../interchange/min_household.csv')$x
housing_units <- read.csv('../interchange/min_housing_unit.csv')$x
sufficient_pop <- as.numeric(population > 30)
                             #& households > 1
                             #& housing_units > 1)


features <- data.frame(sufficient_pop,
                       js_age,
                       js_family,
                       js_race,
                       js_housing,
                       cosine_ethnicity,
                       cosine_housing,
                       cosine_family,
                       cosine_age,
                       chi_ethnicity,
                       chi_housing,
                       chi_family,
                       chi_age,
                       rail,
                       highway,
                       water,
                       elementary_school,
                       high_school,
                       block_angle,
                       grid_street)

M <- model.matrix(~ (sufficient_pop:(cosine_age + 
                                     #cosine_family +
                                     cosine_ethnicity) +
                                     #cosine_housing) +
                     sufficient_pop*(rail +
                                     water +
                                     highway +
                                     grid_street +
                                     elementary_school +
                                     high_school +
                                     block_angle)),
                  data=features)



write.table(M, "model.matrix", row.names=FALSE)

