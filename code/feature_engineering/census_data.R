library(RJSONIO)

base_url ='http://api.census.gov/data/2010/sf1?'
key = 'ac94ba69718a7e1da4f89c6d218b8f6b5ae9ac49'

base_url = paste(base_url, 'key=', key, sep='')

censusData <- function(json_file, variables=NULL) {
  json_data <- fromJSON(json_file)

  data <- do.call(rbind.data.frame, json_data[2:length(json_data)])
  names(data) <- json_data[[1]]

  data[, variables] <- apply(data[, variables],
                             2,
                             FUN=function(x) {as.numeric(as.character(x))})

  return(data)
}

blockData <- function(state, county, variables) {
  state_county <- paste("&in=state:",
                        state,
                        "+county:",
                        county,
                        sep = "")

  var_chunks = split(variables, ceiling(seq_along(variables)/49))

  results <- list()

  for (i in 1:length(var_chunks)) {
    var_chunk <- var_chunks[[i]]

    data <- paste(var_chunk, collapse=",")

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

  
    results[[i]] <- do.call(rbind.data.frame,
                            lapply(block_urls,
                                   FUN=function(x) {
                                     censusData(x, var_chunk)
                                   }))

  }
  return(do.call("data.frame", results))
  
  
}

variables <- c("P0040001", # total population,
               "P0040003", # Hispanic,
               "P0050003", # Non Hispanic White
               "P0050004", # Non Hispanic Black,
               "P0050005", # American Indian,
               "P0050006", # Asian,
               "P0050007", # Pacific Islander,
               "P0050008", # Some Other Race alone
               "P0050009", # Two or more races
               "H0040001", # Occupied housing units
               "H0040002", # Owned with mortgage
               "H0040003", # Owned free and clear
               "H0040004", # Renter Occupied
               "H0050001", # Vacant housing units
               "H0050002", # For rent
               "H0050003", # Rented, not occupied
               "H0050004", # For sale only
               "H0050005", # Sold not occupied
               "H0050006", # For seasonal, recreational, or occasional use
               "H0050007", # For migrant workers
               "H0050008", # Other vacant
               "P0120003", # Under 5, male
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
               "P0120049", # Under 85+, female
               "P0180001", # Households
               "P0180003", # Husband-wife family
               "P0180005", # Male householder, no wife present
               "P0180006", # Female householder, no husband present
               "P0180008", # Male householder, living alone
               "P0180009" # Female householder, living alone
               )

chicago_blocks <- blockData('17', '031', variables)

chicago_blocks$TRACT_BLOC = paste(chicago_blocks$tract,
                                  chicago_blocks$block,
                                  sep="")

write.csv(chicago_blocks,
          "census_data_blocks.csv",
          row.names=FALSE)
