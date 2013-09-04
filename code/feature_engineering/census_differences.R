library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'

censusDifferences <- function(nodes) {
  print(substitute(nodes))
  if (identical(nodes, blocks.poly) 
      || identical(nodes, populated.blocks)) {
    census_data = paste(PATH, 'census_data_blocks.csv', sep='')
    csv_columns = c("TRACT_BLOC"="factor",
                    "state"="factor",
                    "tract"="factor",
                    "block"="factor",
                    "county"="factor")
    chicago_blocks <- read.csv(census_data,
                               colClasses= csv_columns)
    alignment <- match(nodes@data$TRACT_BLOC, 
                       chicago_blocks$TRACT_BLOC)


  } else if (identical(nodes, block.groups.poly)) {
    census_data = paste(PATH, 'census_data_block_groups.csv', sep='')
    csv_columns = c("TRACT_BLOC"="factor",
                    "state"="factor",
                    "tract"="factor",
                    "block.group"="factor",
                    "county"="factor")
    chicago_blocks <- read.csv(census_data,
                               colClasses= csv_columns)
    alignment <- match(nodes@data$TRACT_BLKGRP, 
                       chicago_blocks$TRACT_BLOC)

  }

  nodes@data = data.frame(nodes@data, chicago_blocks[alignment,])

  # Topology of Block Connectivity
  neighbors <- spdep::poly2nb(nodes,
                              foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

  # Calculate 'edge features' will be node features in training
  edgelist <- common::nb2edgelist(neighbors)
    

  race <-  c("P0040003", # Hispanic or Latino
             "P0050003", # Not Hispanic or Latino : White alone
             "P0050004", # Not Hispanic or Latino : Black Alone
             "P0050005", # Not Hispanic or Latino : American Indian and Alaska Native alone
             "P0050006", # Not Hispanic or Latino : Asian alone
             "P0050007", # Not Hispanic or Latino : Native Hawaiian and Other Pacific Islander alone
             "P0050008", # Not Hispanic or Latino : Some other race alone
             "P0050009") # Not Hispanic or Latino : Two or more races
  nodes@data[, race] <- ((1 + nodes@data[, race])
                         /
                         (nodes@data[, "P0040001"] + 8))   # Total population

  # Jensen Shannon Inequality for race distribution
  p <- nodes@data[edgelist[,1], race]
  q <- nodes@data[edgelist[,2], race]
  m <- 0.5 * (p + q)
  js_race <- (0.5 * rowSums(log(p/m) * p)
              +
              0.5 * rowSums(log(q/m)*q))


  housing_type <- c("H0040002", # Owned with mortgage
                    "H0040003", # Owned free and clear
                    "H0040004", # Renter Occupied
                    "H0050002", # For rent
                    "H0050003", # Rented, not occupied
                    "H0050004", # For sale only
                    "H0050005", # Sold not occupied
                    "H0050006", # For seasonal, recreational, or occasional use
                    "H0050007", # For migrant workers
                    "H0050008" # Other vacant
                    )

  nodes@data[, housing_type] <- ((1 + nodes@data[, housing_type])
                                 /
                                 (nodes@data[, "H0040001"]
                                  + nodes@data[, "H0050001"]
                                  + length(housing_type)))

  # Jensen Shannon Inequality for household distribution
  p <- nodes@data[edgelist[,1], housing_type]
  q <- nodes@data[edgelist[,2], housing_type]
  m <- 0.5 * (p + q)
  js_housing <- (0.5 * rowSums(log(p/m) * p)
                 +
                 0.5 * rowSums(log(q/m)*q))


  age <- c("P0120003", # Under 5, male
           "P0120004", # Under 5-9, male
           "P0120005", # Under 10-14, male
           "P0120006", # Under 15-17, male
           "P0120007", # Under 18, 19, male
           "P0120008", # Under 20, male
           "P0120009", # Under 21, male
           "P0120010", # Under 22-24, male
           "P0120011", # Under 25-29, male
           "P0120012", # Under 30-34, male
           "P0120013", # Under 35-39, male
           "P0120014", # Under 40-44, male
           "P0120015", # Under 45-49, male
           "P0120016", # Under 50-54, male
           "P0120017", # Under 55-59, male
           "P0120018", # Under 60,61, male
           "P0120019", # Under 62-64, male
           "P0120020", # Under 65,66, male
           "P0120021", # Under 67-69, male
           "P0120022", # Under 70-74, male
           "P0120023", # Under 75-79, male
           "P0120024", # Under 80-84, male
           "P0120025", # Under 85+, male
           "P0120027", # Under 5, female
           "P0120028", # Under 5-9, female
           "P0120029", # Under 10-14, female
           "P0120030", # Under 15-17, female
           "P0120031", # Under 18, 19, female
           "P0120032", # Under 20, female
           "P0120033", # Under 21, female
           "P0120034", # Under 22-24, female
           "P0120035", # Under 25-29, female
           "P0120036", # Under 30-34, female
           "P0120037", # Under 35-39, female
           "P0120038", # Under 40-44, female
           "P0120039", # Under 45-49, female
           "P0120040", # Under 50-54, female
           "P0120041", # Under 55-59, female
           "P0120042", # Under 60,61, female
           "P0120043", # Under 62-64, female
           "P0120044", # Under 65,66, female
           "P0120045", # Under 67-69, female
           "P0120046", # Under 70-74, female
           "P0120047", # Under 75-79, female
           "P0120048", # Under 80-84, female
           "P0120049" # Under 85+, female
           )

  nodes@data[, age] <- ((1 + nodes@data[, age])
                        /
                        (nodes@data[, "P0040001"] + length(age)))

  # Jensen Shannon Inequality for age distribution
  p <- nodes@data[edgelist[,1], age]
  q <- nodes@data[edgelist[,2], age]
  m <- 0.5 * (p + q)
  js_age <- (0.5 * rowSums(log(p/m) * p)
             +
             0.5 * rowSums(log(q/m)*q))


  family_type <- c("P0180003", # Husband-wife family
                   "P0180005", # Male householder, no wife present
                   "P0180006", # Female householder, no husband present
                   "P0180008", # Male householder, living alone
                   "P0180009" # Female householder, living alone
                   )

  nodes@data[, family_type] <- ((1 + nodes@data[, family_type])
                                /
                                (nodes@data[, "P0180001"] + length(family_type)))




  # Jensen Shannon Inequality for family type distribution
  p <- nodes@data[edgelist[,1], family_type]
  q <- nodes@data[edgelist[,2], family_type]
  m <- 0.5 * (p + q)
  js_family <- (0.5 * rowSums(log(p/m) * p)
                +
                0.5 * rowSums(log(q/m)*q))

  differences <- list(race=js_race,
                      age=js_age,
                      housing=js_housing,
                      family=js_family)

  return(differences)

}


if (!common::from_source()) {
  differences <- censusDifferences(block.groups.poly)
  write.csv(differences$race,
            file="../interchange/js_race.csv", row.names=FALSE)
  write.csv(differences$age,
            file="../interchange/js_age.csv", row.names=FALSE)
  write.csv(differences$family,
            file="../interchange/js_family.csv", row.names=FALSE)
  write.csv(differences$housing,
            file="../interchange/js_housing.csv", row.names=FALSE)
}
