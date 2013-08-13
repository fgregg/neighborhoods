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
                     no.hood.prior) {
  
  class.matrix <- array(, c(dim(centroids)[1],
                            length(hoods)+1
                            )
                        )
  total.listings = 0
  
  for (i in 1:length(hoods)) {
    hood.listings <- listings[listings@data$neighborhood == hoods[i],]
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


ks.pi <- function(points, test.points) {
  H.est <- Hpi.diag(x=points, binned=TRUE)
  fhat <- kde(x=points, 
              H=H.est,
              eval.points = test.points)
  p.x.hood <- fhat$estimate
  
  return(fhat$estimate)
}


