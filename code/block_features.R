library(RJSONIO)
library(rgdal)
library(rgeos)
library(spdep)
library(igraph)

base_url <- "http://api.census.gov/data/2010/sf1?key=ac94ba69718a7e1da4f89c6d218b8f6b5ae9ac49"


nb2edgelist <- function(nb) {
  el <- c()
  for (i in 1:length(nb)) {
    neighbors <- nb[[i]]
    new.neighbors <- neighbors[neighbors > i]
    if (length(new.neighbors) > 0) {
      el <- rbind(el, cbind(i, new.neighbors))
    }
  }
  return(el)
}


censusData <- function(json_file, variables=NULL) {
  json_data <- fromJSON(json_file)

  data <- do.call(rbind.data.frame, json_data[2:length(json_data)])
  names(data) <- json_data[[1]]

  data[, variables] <- apply(data[, variables],
                             2,
                             FUN=function(x) {as.numeric(as.character(x))})

  return(data)
}

blockData <- function(state, county, data) {
  state_county <- paste("&in=state:",
                        state,
                        "+county:",
                        county,
                        sep = "")

  data <- paste(data, collapse=",")
                        
  tract_name_url <- paste(base_url,
                          "&get=NAME&for=tract:*",
                          state_county,
                          sep="")


  tract_names <- censusData(tract_name_url)$tract

  block_urls <- paste(base_url,
                      "&get=NAME,",
                      data,
                      "&for=block:*",
                      state_county,
                      "+tract:",
                      tract_names,
                      sep="")

  print(block_urls)
  
  return(do.call(rbind.data.frame,
                 lapply(block_urls,
                        FUN=function(x) {
                          censusData(x, variables)
                          })))
}

variables <- c("P0040001", # total population,
               "P0040003", # Hispanic,
               "P0050003", # Non Hispanic White
               "P0050004", # Non Hispanic Black,
               "P0050005", # American Indian,
               "P0050006", # Asian,
               "P0050007", # Pacific Islander,
               "P0050008", # Some Other Race alone
               "P0050009" # Two or more races
               )

chicago_blocks <- blockData('17', '031', variables)
chicago_blocks$TRACT_BLOC = paste(chicago_blocks$tract,
                                  chicago_blocks$block,
                                  sep="")
               
# Load Block Data
blocks.poly <- readOGR("../admin_areas/CensusBlockTIGER2010.shp",
                       "CensusBlockTIGER2010")

blocks.poly@data = data.frame(blocks.poly@data,
                              chicago_blocks[match(blocks.poly@data[,c("TRACT_BLOC")],
                                                   chicago_blocks[, c("TRACT_BLOC")]),])


# Topology of Block Connectivity
neighbors <-poly2nb(blocks.poly,
                    foundInBox=gUnarySTRtreeQuery(blocks.poly))

edgelist <- nb2edgelist(neighbors)


for (i in 1:length(neighbors)) {
  p <- blocks.poly@data[i, c("P0040003", "P0050003", "P0050004",
                             "P0050005", "P0050006", "P0050007",
                             "P0050008", "P0050009")] + 1
  p <- p/(blocks.poly@data[i, "P0040001"] + 8)
  for (j in neighbors[[i]]) {
    q <- blocks.poly@data[j, c("P0040003", "P0050003", "P0050004",
                             "P0050005", "P0050006", "P0050007",
                             "P0050008", "P0050009")] + 1
    q <- q/(blocks.poly@data[j, "P0040001"] + 8)
    m <- 0.5 * (p + q)
    kl <- (0.5 * sum(unlist(ifelse(p==0, 0, log(p/m)*p)))
           + 0.5 * sum(unlist(ifelse(q==0, 0, log(q/m)*q))))
    print(kl)
  }
}

