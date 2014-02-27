library(earthmovdist)

euclideanDistance <- function(columns) {
    normalizer <- NODES@data[, "P0040001"]
    normalizer <- ifelse(normalizer == 0, NA, 1)

    block_A <- NODES@data[EDGELIST[,1], columns]/normalizer[EDGELIST[,1]]
    block_B <- NODES@data[EDGELIST[,2], columns]/normalizer[EDGELIST[,2]]


    distance <- sqrt(rowSums((block_A - block_B)^2))

    classIntervals(distance)

}

absDistance <- function(column, zeroes=FALSE) {
    #normalizer <- sapply(slot(nodes, "polygons"), slot, "area")
    
    block_A <- NODES@data[EDGELIST[,1], column] 
    block_B <- NODES@data[EDGELIST[,2], column]

    if (!zeroes) { 
        block_A[block_A == 0] <- NA
        block_B[block_B == 0] <- NA
    }

    distance <- abs(block_A - block_B)

    return(distance)
}

absRatio <- function(column) {
    block_A <- NODES@data[EDGELIST[,1], column] #/normalizer[EDGELIST[,1]]
    block_B <- NODES@data[EDGELIST[,2], column] #/normalizer[EDGELIST[,2]]

    distance <- ((block_A - block_B)^2)/(block_A + block_B)

    distance <- sqrt(distance)

    return(distance)
}

vectorDistance <- function(columns) {


    block_A <- NODES@data[EDGELIST[,1], columns]
    block_B <- NODES@data[EDGELIST[,2], columns]

    block_A <- block_A/rowSums(block_A)
    block_B <- block_B/rowSums(block_B)

    distance <- rowSums(block_A * block_B)

    return(1-distance)
}

mahalanobisDistance <- function(columns) {

    block_A <- NODES@data[EDGELIST[,1], columns]
    block_B <- NODES@data[EDGELIST[,2], columns]

    diff_blocks <- as.matrix(block_A - block_B)

    C = solve(cov(na.omit(diff_blocks)))

    distances <- apply(diff_blocks, 1, function(x) t(x) %*% C %*% x)

    distances <- sqrt(distances)

    return(distances)
}

jsDistance <-function(columns) {
    normalizer <- colSums(NODES@data[, columns])/sum(NODES@data[, columns])
    smoother <- 0.1

    block_A <- ((NODES@data[EDGELIST[,1], columns] + normalizer * smoother)
                /(NODES@data[EDGELIST[,1], "P0040001"] + smoother))
    block_B <- ((NODES@data[EDGELIST[,2], columns] + normalizer * smoother)
                /(NODES@data[EDGELIST[,2], "P0040001"] + smoother))

    m <- 0.5 * (block_A + block_B)

    js <- (0.5 * rowSums((log(block_A) - log(m)) * block_A)
           + 0.5 * rowSums((log(block_B) - log(m)) * block_B))

    return(js)
}

emdDistance <-function(columns) {
    distances <- rep(0, dim(EDGELIST)[1])
    
    block_A <- NODES@data[EDGELIST[,1], columns]
    block_B <- NODES@data[EDGELIST[,2], columns]

    block_A <- block_A/rowSums(block_A)
    block_B <- block_B/rowSums(block_B)


    for (i in 1:length(distances)) {
        distances[i] <- earthmovdist::emdL1(as.numeric(block_A[i,]),
                                            as.numeric(block_B[i,]))
    }

    return(distances)

}

cramerDistance <-  function(columns) {
    block_A <- NODES@data[EDGELIST[,1], columns]
    block_B <- NODES@data[EDGELIST[,2], columns]

    block_A <- block_A/rowSums(block_A)
    block_B <- block_B/rowSums(block_B)

    block_A <- t(apply(block_A, 1, cumsum))
    block_B <- t(apply(block_B, 1, cumsum))

    missing <- rowSums(block_A + block_B, na.rm=TRUE) == 0
    

    distance <- rowSums((block_A - block_B)^2, na.rm=TRUE)

    distance[missing] <- NA

    return(sqrt(distance)/2)
}


chiDistance <- function(columns, smoothing=10) {
    smoother <- colSums(NODES@data[, columns])
    smoother <- smoother / sum(smoother)
    
    block_A <- NODES@data[EDGELIST[,1], columns]
    block_B <- NODES@data[EDGELIST[,2], columns]

    normalizer_A <- rowSums(block_A)
    normalizer_B <- rowSums(block_B)

    block_A <- (block_A + smoother * smoothing)/(normalizer_A + smoothing)
    block_B <- (block_B + smoother * smoothing)/(normalizer_B + smoothing)

    missing <- (normalizer_A + normalizer_B) == 0
    
    distance <- rowSums(((block_A - block_B)^2)/(block_A + block_B), na.rm=TRUE)

    distance[missing] <- NA

    return(sqrt(0.5*distance))
}

minPair <- function(column) {
    block_A <- NODES@data[EDGELIST[,1], column]
    block_B <- NODES@data[EDGELIST[,2], column]

    return(apply(cbind(block_A, block_B), 1, min))
}

                                             





