library(rgdal)

  
edgeList <- function(nodes, all_blocks=TRUE, edges=NULL) {
  node_neighbors <-spdep::poly2nb(nodes,
                                  queen=FALSE,
                                  foundInBox=rgeos::gUnarySTRtreeQuery(nodes))
  node_edgelist <- common::nb2edgelist(node_neighbors)

  if (!is.null(edges) & all_blocks) {
    results <- edges
  }
  else {
    results <- common::extractBorder(node_edgelist, nodes)
  }

  return(node_edgelist[results$spatial_lines,])
}


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

edgesToLines <- function(edges, centroids, projection) {
    edges <- lapply(seq_len(nrow(edges)), function(i) edges[i,])  
    SpatialLines(
        mapply(function(edge, id) {
            l <- rbind(centroids[edge[1],],
                       centroids[edge[2],])
            l <- Lines(Line(l), ID=id)
            return(l)
        },
               edges,
               1:length(edges)),
        proj4string=CRS(projection))
}
          
              


createEdgeToLine <- function(centroids, crs) {
  edgeToLine <- function(edge) {
    l <- rbind(centroids[edge[1],],
               centroids[edge[2],])
    l <- Line(l)
    l <- SpatialLines(list(Lines(l, ID="l")),
                      proj4string = CRS(crs))
    return(l)
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
  #plot(water, col="#C0E7F3", border=0, add=TRUE)
  lines(railroads, lty=2, col="tan")
}



extractBorder <- function(border_edgelist, polys) {
  borders <- rep(list(NA), dim(border_edgelist)[1])
  id = 1
  for (i in 1:length(borders)) {
    edge <- border_edgelist[i,]
    inter <- rgeos::gIntersection(polys[edge[1],],
                                  polys[edge[2],])
    if (class(inter) == "SpatialPoints") {
      borders[i] <- inter
    }
    else if (class(inter) == "SpatialCollections") {
      inter <- attr(inter, 'lineobj')
      inter <- inter[which.max(SpatialLinesLengths(inter))]
      borders[i] <- sp::spChFIDs(inter, as.character(id))
      id <- id + 1
    }
    else if (class(inter) == "SpatialPolygons") {
      inter <- as(inter, 'SpatialLines')
      borders[i] <- sp::spChFIDs(inter, as.character(id))
      id <- id + 1
    }
    else {
      borders[i] <- sp::spChFIDs(inter, as.character(id))
      id <- id + 1
    }
  }

  spatial_lines <- unlist(lapply(borders, class)) != "SpatialPoints"

  border_lines <- borders[spatial_lines]

  border_lines <- do.call(rbind, border_lines)

  return(list(lines=border_lines,
              spatial_lines=spatial_lines))
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

plotBorders <- function(edgelist) {
    predicted <- read.table("/home/fgregg/academic/neighborhoods/code/interchange/border.csv", skip=1)
    borders <- edgelist[predicted == TRUE,]
    border_lines <- common::extractBorder(borders, nodes)
    return(border_lines)
}

# http://richardburcher.com/2012/05/01/minumum-bounding-rectangle-as-proxy-for-internal-metrics-of-drumlins-or-other-geometries/

minimum_bounding_box <- function(points) {
  points <- points[chull(points),]

  n_points <- nrow(points)
  
  # Complete the ring
  points <- rbind(points, points[1,])

  minimum_box_area <- Inf
  minimum_rotated_points <- NA

  for (i in 1:n_points) {
    theta <- pi - atan2(points[i+1,2]-points[i,2],
                        points[i+1,1]-points[i,1])
    rotated_points <- rotate(points, theta)
    box_area <- (diff(range(rotated_points[,1]))
                 * diff(range(rotated_points[,2]))
                 )
    if (box_area < minimum_box_area) {
      minimum_box <- cbind(
        c(min(rotated_points[,1]),max(rotated_points[,1]),
          max(rotated_points[,1]),min(rotated_points[,1])),
        c(min(rotated_points[,2]),min(rotated_points[,2]),
          max(rotated_points[,2]),max(rotated_points[,2]))
        )
      minimum_box <- rbind(minimum_box,
                           minimum_box[1,])
      minimum_box <- rotate(minimum_box, -theta)

      minimum_rotated_points <- rotated_points
      minimum_box_area <- box_area
    }
  }

  dist_1 <- ((minimum_box[2,2]-minimum_box[1,2])^2
             +
             (minimum_box[2,1]-minimum_box[1,1])^2)

  theta_1 <- pi - atan2(minimum_box[2,2]-minimum_box[1,2],
                        minimum_box[2,1]-minimum_box[1,1])
 
  dist_2 <- ((minimum_box[3,2]-minimum_box[2,2])^2
             +
             (minimum_box[3,1]-minimum_box[2,1])^2)
 
  if (dist_1 > dist_2) {
    minimum_box_theta = theta_1
  } else {
    minimum_box_theta <- pi - atan2(minimum_box[3,2]-minimum_box[2,2],
                                    minimum_box[3,1]-minimum_box[2,1])
  }
  
  
  return(list(minbba=minimum_box_area,
              theta=minimum_box_theta,
              pts=minimum_rotated_points,
              box=minimum_box))
}


rotate = function(pts,angle){
  co = cos(angle)
  si = sin(angle)
  cbind(co * pts[,1] - si * pts[,2], si * pts[,1] + co * pts[,2])
}

