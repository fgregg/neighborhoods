library(ks)

normalize <- function(x) {
  return((x - median(x))/mad(x, constant=1))
  }
  
hampelCriticalValue <- function(n, alpha, num.samples=100) {
  samples = matrix(rnorm(n * num.samples, 0, 1), n, num.samples)
  samples = normalize(samples)/0.6745
  maxValues = apply(samples,
                    MARGIN=2,
                    FUN=function(x) {max(abs(x))}
                    )
  return(quantile(maxValues, 1-alpha))
}


hampelOutliers <- function(points, alpha) {
  threshold <- hampelCriticalValue(dim(points)[1], alpha)

  return(points[abs(normalize(points@coords[,1])) < threshold &
                abs(normalize(points@coords[,2])) < threshold, ])
}

trainKDE <- function(listings,
                     hoods,
                     centroids,
                     range,
                     kde,
                     no.hood.prior,
                     id="neighborhood") {
  
  class.matrix <- array(, c(dim(centroids)[1],
                            length(hoods)+1
                            )
                        )
  total.listings = 0
  
  for (i in 1:length(hoods)) {
    hood.listings <- listings[listings@data[, id] == hoods[i],]
    hood.listings <- hampelOutliers(hood.listings, .90)

    num.listings <- dim(hood.listings)[1]

    p.density <- kde(hood.listings@coords, centroids)
    
    class.matrix[,i] <- p.density * num.listings
  }
  
  class.matrix[,length(hoods)+1] <- no.hood.prior

  # the kde returns an estimate of the probability of drawing a
  # neighborhood centroid from the distribution of places that belong
  # to a neighborhood. We are interested in the inverse problem, what
  # is the probability of location be assigned to a neighborhood.
  class.matrix <- class.matrix/rowSums(class.matrix)

  return(class.matrix)
}



rasterTrainKDE <- function(listings,
                           hoods,
                           range,
                           n.points,
                           no.hood.prior,
                           id="neighborhood") {
  
  class.matrices <- array(, c(n.points, 
                              n.points, 
                              length(hoods)+1
                              )
                          )
  resolution = 10
  freq.hood <- matrix(0, resolution*resolution, length(hoods))
  
  total.listings = 0
  
  for (i in 1:length(hoods)) {
    hood.listings <- listings[listings@data[, id] == hoods[i],]
    hood.listings <- hampelOutliers(hood.listings, .10)
#    hood.listings$x <- jitter(hood.listings$x, amount=.001)
#    hood.listings$y <- jitter(hood.listings$y, amount=.001)

    num.listings <- dim(hood.listings)[1]
    
    p.density <- ks.pi.raster(hood.listings@coords, n.points, range)
    class.matrices[,,i] <- p.density * num.listings
    total.listings = total.listings + num.listings
  }
  
  class.matrices <- class.matrices/total.listings
  class.matrices[,,length(hoods)+1] <- matrix(no.hood.prior, 
                                              n.points, 
                                              n.points)
  class.matrices
}


ks.pi.raster <- function(points, n.points, range) {
  H <- Hpi.diag(x=points)
  fhat <- kde(x=points, 
              H=H, 
              gridsize=c(n.points, n.points),
              xmin = range[1,],
              xmax = range[2,]
              )
  fhat$estimate/(sum(colSums(fhat$estimate)))
}


ks.pi <- function(points, test.points) {
  H.est <- Hpi.diag(x=points, binned=TRUE)
  fhat <- kde(x=points, 
              H=H.est,
              eval.points = test.points)
  p.x.hood <- fhat$estimate
  
  return(fhat$estimate)
}

