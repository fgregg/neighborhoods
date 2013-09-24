library(rgeos)
library(spdep)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

nodes = populated.blocks

nodes@data = data.frame(nodes@data,
                              bordering = rep(FALSE, dim(nodes@data)[1]))

predicted <- read.table("predicted_borders.csv")

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(nodes,
                           foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)



borders <- edgelist[predicted == TRUE,]

extractBorder <- function(border_edgelist, polys) {
  border_lines <- apply(border_edgelist,
                        1,
                        FUN=function(x) {
                          inter <- rgeos::gIntersection(polys[x[1],],                                                                       polys[x[2],])
                        })
  border_lines <- border_lines[unlist(lapply(border_lines, class)) == "SpatialLines"]

  print(length(border_lines))

  border_lines <- mapply(sp::spChFIDs,
                         border_lines,
                         as.character(1:length(border_lines)))

  border_lines <- do.call(rbind, border_lines)

  return(border_lines)
}


border_lines <- extractBorder(borders, nodes)
       
plot(nodes,
     col="black",
     border=rgb(0,0,0,0.04),
     lwd=0.01)

lines(border_lines, col="red", lwd=2)

                 



