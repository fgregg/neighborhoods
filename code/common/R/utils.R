library(rgdal)

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

# Could certainly be sped up by creating all the lines once
edgesIntersect <- function(edges, centroids, barrier, crs) {
  edgeToLine <- createEdgeToLine(centroids, crs)
  apply(edges,
        1,
        FUN=function(x) {
          gIntersects(edgeToLine(x), barrier)
        }
        )
}

crossesPolygons <- function(edges, centroids, barrier, crs) {
  edgeToLine <- createEdgeToLine(centroids, crs)
  apply(edges,
        1,
        FUN=function(x) {
          num_intersect <- sum(gIntersects(edgeToLine(x), barrier, byid=TRUE))
        }
        )
}

createEdgeToLine <- function(centroids, crs) {
  edgeToLine <- function(edge) {
    l <- rbind(centroids[edge[1],],
               centroids[edge[2],])
    l <- Line(l)
    l <- SpatialLines(list(Lines(l, ID="l")),
                      proj4string = CRS(crs))
  }
  return(edgeToLine)
}

alpha.levels <- function(probs) {
  return(as.numeric(cut(probs, breaks=seq(0, 1.1, .1)/11)))
       }

probColors <- function(map.colors, classes) {
  #map.colors <- sample(map.colors, length(map.colors))
  max.class <- apply(classes, 1, which.max)
  max.probs <- apply(classes, 1, max)
  block.colors <- map.colors[max.class]
  return(rgb(t(col2rgb(block.colors))/255, alpha=max.probs))
}

plotPredictions <- function(class.matrices,colors,k,range,boundaries=TRUE) {
  # Takes a X*Y*Number Array of Classes
  #
  n.points <- dim(class.matrices)[1]
  x = seq(range[1,1], range[2,1], length.out=n.points)
  y = seq(range[1,2], range[2,2], length.out=n.points)
  
  max.probs <- apply(class.matrices, 
                     MARGIN=c(1,2),
                     FUN=function(x) {max(x, na.rm=TRUE)})

  imageDensity <- function(x, y, z, color) {
    image(x,
          y,
          z, 
          col=rgb(t(col2rgb(color))/255,
                  alpha=seq(0,1,.1)),
          add=TRUE,
          breaks=c(seq(0,1,.1)/k, 1.1),
          useRaster=TRUE
          )

  }
  decisionBoundary <- function(x,y, z, max.probs) {
    contour(x,
            y,
            z - max.probs, 
            levels=c(0), 
            drawlabels=FALSE,
            add=TRUE,
            lty=1,
            lwd=.3,
            col="slategray")
  }

  for (i in 1:(dim(class.matrices)[3]-1)) {
    class.matrix <- class.matrices[,,i]
    imageDensity(x,y,class.matrix, colors[i])
    if (boundaries) {
      decisionBoundary(x,y,class.matrix, max.probs)
    }
    
  }
}

basePlot <- function(range) {
  plot(range[,1], range[,2],
       type="n",
       asp=1,
       axes=FALSE,
       yaxs="i",
       xaxs="i",
       xlab="",
       ylab="")
  #plot(com.areas, border="grey", add=TRUE, lwd=.1)
  plot(water, col="#C0E7F3", border=0, add=TRUE)
  lines(railroads, lty=2, col="tan")
}


extractBorder <- function(border_edgelist, polys) {
  border_lines <- apply(border_edgelist,
                        1,
                        FUN=function(x) {
                          inter <- rgeos::gIntersection(polys[x[1],],                                                                       polys[x[2],])
                        })
  border_lines <- border_lines[unlist(lapply(border_lines, class)) == "SpatialLines"]

  border_lines <- lapply(border_lines,
                         FUN=function(x) {
                           sp::spChFIDs(x,
                                        as.character(sample(10000000,
                                                            length(x))))
                         })

  border_lines <- do.call(rbind, border_lines)

  return(border_lines)
}

dissolve <- function(SP, by, types, new_type) {
  SP <- SP[SP@data[, by] %in% types,]
  neighbors <- spdep::poly2nb(SP, foundInBox=rgeos::gUnarySTRtreeQuery(SP))
  subgraphs <- spdep::n.comp.nb(neighbors)
  dissolved_SP <- maptools::unionSpatialPolygons(SP, subgraphs$comp.id)
  dissolved_SPDF <- sp::SpatialPolygonsDataFrame(dissolved_SP,
                                                 data.frame(type=rep(new_type,
                                                              length(dissolved_SP))))
  return(dissolved_SPDF)
}



projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# Lat/Lon of Bounding Box
range = cbind(c(-87.7, -87.6), c(41.89,41.98))
range = rgdal::project(range, projection)

bbx <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(range[1,1],
                                                          range[1,1],
                                                          range[2,1],
                                                          range[2,1],
                                                          range[1,1]),
                                                        c(range[1,2],
                                                          range[2,2],
                                                          range[2,2],
                                                          range[1,2],
                                                          range[1,2])))),
                                     "l")),
                       proj4string = CRS(projection))

from_source <- function() {
  return(length(sys.frames()) >= 4 && sys.call(1)[[1]] == quote(source))
}
