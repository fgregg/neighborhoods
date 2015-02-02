library(RMySQL)
library(stringdist)
library(XML)

# Convenience function to convert html codes
html2txt <- function(charvec) {
      sapply(charvec, FUN = function(x) {
          if (x == '') {
              return(x)
          }
          else {
              return(XML::xpathApply(XML::htmlParse(x, asText=TRUE),
                                     "//body//text()", 
                                     XML::xmlValue)[[1]])
          }
      })
}


cleanChars <- function(label) {
    label <- html2txt(label)
    label <- gsub(pattern = "^[^a-zA-Z0-9]*|[^a-zA-Z0-9]*$", replacement="", label) # trim everything that's not an ASCII letter or number from front and back
    label <- unname(label)
    label <- gsub(pattern = "(\\d\\W*\\s*){9,11}", replacement="", label)   # phone
    label <- gsub(pattern = "\\b(\\+|&)\\b", replacement=" and ", label)
    label <- gsub(pattern = "(\\+|&)", replacement="and", label)
    label <- gsub(pattern = "[-!|@:;,()*<>{}_~!#[]+", replacement="/", label)
    label <- gsub(pattern = "\\s*/\\s*", replacement="/", label)
    label <- gsub(pattern = "\\s+", replacement=" ", label)
    label <- gsub(pattern = "^\\s*|\\s*$", replacement="", label)
    return(label)
}

removeCityState <- function(label, city, state, fullstate) {
    label <- gsub(pattern = paste("[,/] *", city, "\\b", sep=""),
                  replacement = "",
                  label)
    label <- gsub(pattern = paste("[,/] *(", state, '|', fullstate, ")\\b", sep=""),
                  replacement = "",
                  label)
    label <- gsub(pattern = paste("[,/] *", city, "[,/] *", state, "\\b", sep=""),
                  replacement = "",
                  label)
    return(label)
}

normalizedNames <- function(names, city, state) {
    common_names <- names$neighborhood[names$num_locations > 1]
    common_names <- removeCityState(common_names, city, state)
    common_names <- cleanChars(common_names)
    
    D <- as.dist(stringdist::stringdistmatrix(common_names,
                                              common_names,
                                              method='jw'))

    tree <- hclust(D)

    clusters <- cutree(tree, h=0.075)

    lookup_name <- rep('', length(common_names))

    for (i in 1:max(clusters)) {
        lookup_name[clusters == i] <- common_names[clusters == i][1]
    }
    
    names(lookup_name) <- names$neighborhood[names$num_locations > 1]

    return(lookup_name)
}

