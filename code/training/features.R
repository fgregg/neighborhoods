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

## chi_ethnicity <- read.csv('../interchange/chi_ethnicity.csv')$x
## chi_family <- read.csv('../interchange/chi_family.csv')$x
## chi_housing <- read.csv('../interchange/chi_housing.csv')$x
## chi_age <- read.csv('../interchange/chi_age.csv')$x

## chi_ethnicity[is.na(chi_ethnicity)] <- 100
## chi_family[is.na(chi_family)] <- 100
## chi_housing[is.na(chi_housing)] <- 100
## chi_age[is.na(chi_age)] <- 100




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

diff_housing_units <- read.csv('../interchange/diff_housing_unit.csv')$x
diff_housing_units[is.na(diff_housing_units)] <- 100

sufficient_pop <- as.numeric(population > 30)
sufficient_households <- as.numeric(households > 5)
sufficient_units <- as.numeric(housing_units > 5)

just_households <- as.numeric(sufficient_households == 1
                              & sufficient_pop == 0)

just_units <- as.numeric(sufficient_units == 1
                         & sufficient_households == 0
                         & sufficient_pop == 0)

features <- data.frame(sufficient_pop,
                       just_households,
                       just_units,
                       js_age,
                       js_family,
                       js_race,
                       js_housing,
                       cosine_ethnicity,
                       cosine_housing,
                       cosine_family,
                       cosine_age,
                       diff_housing_units,
                       rail,
                       highway,
                       water,
                       elementary_school,
                       high_school,
                       block_angle,
                       grid_street)

M <- model.matrix(~ (sufficient_pop:(cosine_age + 
                                     cosine_ethnicity) +
                     sufficient_pop*(rail +
                                     water +
                                     grid_street +
                                     elementary_school +
                                     high_school +
                                     block_angle) +
                     sufficient_households:cosine_family +
                     just_households +
                     just_households:(rail +
                                      water +
                                      grid_street +
                                      elementary_school +
                                      high_school +
                                      block_angle) + 
                     sufficient_units:(cosine_housing +
                                       diff_housing_units) +
                     just_units +
                     just_units:(grid_street +
                                 elementary_school +
                                 block_angle)),
                  data=features)



write.table(M, "model.matrix", row.names=FALSE)

