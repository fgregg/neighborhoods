library(spdep)
library(devtools)


pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')
devtools::load_all(pkg)


pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)


blockPop <- function(nodes) {

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



    nodes@data = data.frame(nodes@data, chicago_blocks[alignment,])

    pop <- nodes@data[, "P0040001"]
    return(pop)
    
}

pop <- blockPop(chicago.blocks.poly)
  
write.csv(as.numeric(pop <= 1), '../interchange/chicago_block_pop.csv', row.names=FALSE)
