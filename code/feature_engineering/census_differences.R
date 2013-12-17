library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'

if (interactive() & !common::from_source()) {
  neighbors <- spdep::poly2nb(nodes,
                              queen=FALSE,
                              foundInBox=rgeos::gUnarySTRtreeQuery(nodes))
  edgelist <- common::nb2edgelist(neighbors)
  border_lines <- common::plotBorders(edgelist)
}

featurePlot <- function(feature, file_name) {
  classes <- classInt::classIntervals(feature, 8, style="kmeans")
  colcode <- classInt::findColours(classes, c("wheat", "red"))

  png(file_name)
  plot(nodes, col=colcode, border='transparent')
  lines(border_lines)
  dev.off()
}

censusDifferences <- function(nodes, make_plots=FALSE, cached_edges) {

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

  # Calculate 'edge features' will be node features in training
  edgelist <- common::edgeList(nodes, edges=cached_edges)

  pop_1 <- nodes@data[edgelist[,1], "P0040001"]
  pop_2 <- nodes@data[edgelist[,2], "P0040001"]
  min_pop <- apply(cbind(pop_1, pop_2), 1, min)

  if (make_plots) {
    featurePlot(nodes$P0040001, "population.png")
  }

  
  race <-  c("P0040003", # Hispanic or Latino
             "P0050003", # Not Hispanic or Latino : White alone
             "P0050004", # Not Hispanic or Latino : Black Alone
             "P0050006") # Not Hispanic or Latino : Asian alone

  overall_proportion <- colSums(nodes@data[,race])/sum(nodes@data[,race])

  nodes@data[, race] <- ((nodes@data[, race] + overall_proportion)
                         /
                         (rowSums(nodes@data[, race]) + 1))   # Total population

  # Jensen Shannon Inequality for race distribution
  p <- nodes@data[edgelist[,1], race]
  q <- nodes@data[edgelist[,2], race]
  m <- 0.5 * (p + q)
  js_race <- (0.5 * rowSums(log(p/m) * p)
              +
              0.5 * rowSums(log(q/m)*q))

  if (make_plots) {
    featurePlot(nodes$P0040003, "hispanic.png")
    featurePlot(nodes$P0050003, "white.png")
    featurePlot(nodes$P0050004, "black.png")
    featurePlot(nodes$P0050006, "asian.png")
  }



  ### HOUSING
  
  all_units <- c("H0040001", # Occupied Units
                 "H0050001") # Vacant Units

  rentals <- c("H0040004", # Renter Occupied
               "H0050002", # For rent
               "H0050003") # Rented, not occupied

  overall_proportion <- sum(nodes@data[,rentals])/sum(nodes@data[,all_units])

  nodes$rental_units <- ((rowSums(nodes@data[, rentals])
                          + overall_proportion)
                         /
                         (rowSums(nodes@data[, all_units]) + 1))

  overall_proportion <- sum(nodes$H0040001)/sum(nodes@data[,all_units])
  
  nodes$occupied_units <- ((nodes$H0040001 + overall_proportion)
                           /(rowSums(nodes@data[, all_units]) + 1))


                                  
  # Jensen Shannon Inequality for household distribution
  p <- nodes@data[edgelist[,1], "rental_units"]
  q <- nodes@data[edgelist[,2], "rental_units"]
  m <- 0.5 * (p + q)
  js_housing <- (0.5 * log(p/m) * p
                 +
                 0.5 * log(q/m)*q)

  if (make_plots) {
    featurePlot(nodes$rental_units, "rentals.png")
  }

  nodes$preschool <- rowSums(nodes@data[, c("P0120003", # Under 5, male
                                            "P0120027")] # Under 5, female
                             )
  
  nodes$school <- rowSums(nodes@data[, c("P0120004", # Under 5-9, male
                                         "P0120005", # Under 10-14, male
                                         "P0120006", # Under 15-17, male
                                         "P0120028", # Under 5-9, female
                                         "P0120029", # Under 10-14, female
                                         "P0120030")] # Under 15-17, female
                          )

  nodes$college <- rowSums(nodes@data[, c("P0120007", # Under 18, 19, male
                                          "P0120008", # Under 20, male
                                          "P0120009", # Under 21, male
                                          "P0120031", # Under 18, 19, female
                                          "P0120032", # Under 20, female
                                          "P0120033")] # Under 21, female
                           )

  nodes$young_adult <- rowSums(nodes@data[, c("P0120010", # Under 22-24, male
                                              "P0120011", # Under 25-29, male
                                              "P0120034", # Under 22-24, female
                                              "P0120035")] # Under 25-29, female
                               )

  nodes$middle_age <- rowSums(nodes@data[, c("P0120012", # Under 30-34, male
                                             "P0120013", # Under 35-39, male
                                             "P0120014", # Under 40-44, male
                                             "P0120015", # Under 45-49, male
                                             "P0120016", # Under 50-54, male
                                             "P0120017", # Under 55-59, male
                                             "P0120018", # Under 60,61, male
                                             "P0120019", # Under 62-64, male
                                             "P0120036", # Under 30-34, female
                                             "P0120037", # Under 35-39, female
                                             "P0120038", # Under 40-44, female
                                             "P0120039", # Under 45-49, female
                                             "P0120040", # Under 50-54, female
                                             "P0120041", # Under 55-59, female
                                             "P0120042", # Under 60,61, female
                                             "P0120043")] # Under 62-64, female
                              )
  

  nodes$retired <- rowSums(nodes@data[, c("P0120020", # Under 65,66, male
                                          "P0120021", # Under 67-69, male
                                          "P0120022", # Under 70-74, male
                                          "P0120023", # Under 75-79, male
                                          "P0120024", # Under 80-84, male
                                          "P0120025", # Under 85+, male
                                          "P0120044", # Under 65,66, female
                                          "P0120045", # Under 67-69, female
                                          "P0120046", # Under 70-74, female
                                          "P0120047", # Under 75-79, female
                                          "P0120048", # Under 80-84, female
                                          "P0120049")] # Under 85+, female
                           )              

  ages <- c("preschool", "school", "college",
            "young_adult", "middle_age", "retired")

  overall_proportion <- colSums(nodes@data[, ages])/sum(nodes$P0040001)
  
  nodes@data[, ages] <- ((nodes@data[, ages] + overall_proportion)
                         / 
                        (nodes$P0040001 + 1))

  # Jensen Shannon Inequality for age distribution
  p <- nodes@data[edgelist[,1], ages]
  q <- nodes@data[edgelist[,2], ages]
  m <- 0.5 * (p + q)
  js_age <- (0.5 * rowSums(log(p/m) * p)
             +
             0.5 * rowSums(log(q/m)*q))

  if (make_plots) {

    featurePlot(nodes$preschool, "preschool.png")
    featurePlot(nodes$school, "school.png")
    featurePlot(nodes$college, "college.png")
    featurePlot(nodes$young_adult, "young_adult.png")
    featurePlot(nodes$middle_age, "middle_age.png")
    featurePlot(nodes$retired, "retired.png")

  }




  family_type <- c("P0180003", # Husband-wife family
                   "P0180005", # Male householder, no wife present
                   "P0180006", # Female householder, no husband present
                   "P0180008", # Householder, living alone
                   "P0180009" # Householder, not living alone
                   )

  overall_proportion <- colSums(nodes@data[, family_type])/sum(nodes$P0180001)

  nodes@data[, family_type] <- ((nodes@data[, family_type] + overall_proportion)
                                /
                                (nodes$P0180001 + 1))




  # Jensen Shannon Inequality for family type distribution
  p <- nodes@data[edgelist[,1], family_type]
  q <- nodes@data[edgelist[,2], family_type]
  m <- 0.5 * (p + q)
  js_family <- (0.5 * rowSums(log(p/m) * p)
                +
                0.5 * rowSums(log(q/m)*q))

  if (make_plots) {
    
    featurePlot(nodes$P0180003, "husband_wife.png")
    featurePlot(nodes$P0180005, "single_dad.png")
    featurePlot(nodes$P0180006, "single_mom.png")
    featurePlot(nodes$P0180008, "living_along.png")
    featurePlot(nodes$P0180009, "roommates.png")

  }


  differences <- list(population=min_pop,
                      race=js_race,
                      age=js_age,
                      housing=js_housing,
                      family=js_family)



  return(differences)

}


if (!common::from_source()) {
  differences <- censusDifferences(blocks.poly)
  write.csv(differences$race,
            file="../interchange/js_race.csv", row.names=FALSE)
  write.csv(differences$age,
            file="../interchange/js_age.csv", row.names=FALSE)
  write.csv(differences$family,
            file="../interchange/js_family.csv", row.names=FALSE)
  write.csv(differences$housing,
            file="../interchange/js_housing.csv", row.names=FALSE)
}
