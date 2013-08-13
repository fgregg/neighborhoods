library(RJSONIO)

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

write.csv(chicago_blocks,
          "census_data_blocks.csv",
          row.names=FALSE)
