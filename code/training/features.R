pkg <- devtools::as.package('~/academic/neighborhoods/code/common')


devtools::load_all(pkg)

js_age <- read.csv('../interchange/js_age.csv')$x
js_family <- read.csv('../interchange/js_family.csv')$x
js_race <- read.csv('../interchange/js_race.csv')$x
js_housing <- read.csv('../interchange/js_housing.csv')$x

cosine_ethnicity <- read.csv('../interchange/cosine_ethnicity.csv')$x
cosine_family <- read.csv('../interchange/cosine_family.csv')$x
cosine_age <- read.csv('../interchange/cosine_age.csv')$x
cosine_housing <- read.csv('../interchange/cosine_housing.csv')$x

cosine_ethnicity[is.na(cosine_ethnicity)] <- 100
cosine_family[is.na(cosine_family)] <- 100
cosine_age[is.na(cosine_age)] <- 100
cosine_housing[is.na(cosine_housing)] <- 100


chi_ethnicity <- read.csv('../interchange/chi_ethnicity.csv')$x
chi_family <- read.csv('../interchange/chi_family.csv')$x
chi_housing <- read.csv('../interchange/chi_housing.csv')$x
chi_age <- read.csv('../interchange/chi_age.csv')$x

chi_ethnicity[is.na(chi_ethnicity)] <- 100
chi_family[is.na(chi_family)] <- 100
chi_housing[is.na(chi_housing)] <- 100
chi_age[is.na(chi_age)] <- 100

abs_age <- read.csv('../interchange/abs_age.csv')$x
abs_age[is.na(abs_age)] <- 100

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

all_sufficient <- as.numeric(sufficient_pop == 1
                             & sufficient_households == 1
                             & sufficient_units == 1)

pop_households <- as.numeric(sufficient_units == 0
                             & sufficient_households == 1
                             & sufficient_pop == 1)

pop_units <- sum(sufficient_units == 1
                 & sufficient_households == 0
                 & sufficient_pop == 1)

household_units <- as.numeric(sufficient_units == 1
                              & sufficient_households == 1
                              & sufficient_pop == 0)

just_pop <- as.numeric(sufficient_units == 0
                       & sufficient_households == 0
                       & sufficient_pop == 1)

# no blocks with just households
# just_households  <- sum(sufficient_units == 0
#                        & sufficient_households == 1
#                        & sufficient_pop == 0)

just_units  <- as.numeric(sufficient_units == 1
                          & sufficient_households == 0
                          & sufficient_pop == 0)


features <- data.frame(all_sufficient,
                       pop_households,
                       household_units,
                       just_pop,
                       just_units,
                       chi_ethnicity,
                       chi_housing,
                       chi_family,
                       chi_age,
                       diff_housing_units,
                       rail,
                       highway,
                       water,
                       elementary_school,
                       high_school,
                       block_angle,
                       grid_street)

M <- model.matrix(~ (highway + 
                     all_sufficient:(abs_age + 
                                     chi_ethnicity +
                                     chi_family +
                                     chi_housing +
                                     diff_housing_units) +
                     all_sufficient*(rail +
                                     water +
                                     grid_street +
                                     #highway +
                                     elementary_school +
                                     high_school +
                                     block_angle) +
                     household_units +
                     household_units:(rail +
                                      water +
                                      grid_street +
                                      #highway + 
                                      elementary_school +
                                      high_school +
                                      block_angle +
                                      chi_family +
                                      chi_housing +
                                      diff_housing_units) +
                     just_pop +
                     just_pop:(#rail +
                               #water +
                               grid_street +
                               #highway +
                               elementary_school +
                               # high_school +
                               block_angle +
                               chi_ethnicity +
                               abs_age) +
                     just_units +
                     just_units:(rail +
                                 # water +
                                 grid_street +
                                 #highway +
                                 elementary_school +
                                 high_school +
                                 block_angle +
                                 chi_housing)
                     ),
                  data=features)



write.table(M, "model.matrix", row.names=FALSE)
 
