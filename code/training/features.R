pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

#PREFIX = "chicago_"
PREFIX = ""

chi_ethnicity <- read.csv(paste('../interchange/', PREFIX, 'chi_ethnicity.csv', sep=""))$x
chi_family <- read.csv(paste('../interchange/', PREFIX, 'chi_family.csv', sep=""))$x
chi_housing <- read.csv(paste('../interchange/', PREFIX, 'chi_housing.csv', sep=""))$x

chi_ethnicity[is.na(chi_ethnicity)] <- 100
chi_family[is.na(chi_family)] <- 100
chi_housing[is.na(chi_housing)] <- 100

abs_age <- read.csv(paste('../interchange/', PREFIX, 'abs_age.csv', sep=""))$x
abs_age[is.na(abs_age)] <- 100
abs_age[is.infinite(abs_age)] <- 100

rail <- read.csv(paste('../interchange/', PREFIX, 'rail_intersects.csv', sep=""))$x
highway <- read.csv(paste('../interchange/', PREFIX, 'highway_intersects.csv', sep=""))$x
grid_street <- read.csv(paste('../interchange/', PREFIX, 'grid_intersects.csv', sep=""))$x
water <- read.csv(paste('../interchange/', PREFIX, 'water_intersects.csv', sep=""))$x

elementary_school <- read.csv(paste('../interchange/', PREFIX, 'elementary_schools_crosses.csv', sep=""))$x
high_school <- read.csv(paste('../interchange/', PREFIX, 'high_schools_crosses.csv', sep=""))$x
block_angle <- read.csv(paste('../interchange/', PREFIX, 'block_angles.csv', sep=""))$x

block_angle <- block_angle %% pi/2 / (pi/2)

population <- read.csv(paste('../interchange/', PREFIX, 'min_population.csv', sep=""))$x
households <- read.csv(paste('../interchange/', PREFIX, 'min_household.csv', sep=""))$x
housing_units <- read.csv(paste('../interchange/', PREFIX, 'min_housing_unit.csv', sep=""))$x

diff_housing_units <- read.csv(paste('../interchange/', PREFIX, 'diff_housing_unit.csv', sep=""))$x
diff_housing_units[is.na(diff_housing_units)] <- 100
diff_housing_units[is.infinite(diff_housing_units)] <- 100

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
                       abs_age,
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



write.table(M, paste(PREFIX, "model.matrix", sep=""), row.names=FALSE)
 
