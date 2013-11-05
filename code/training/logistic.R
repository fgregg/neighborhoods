library(rgeos)
library(spdep)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

source('../feature_engineering/line_graph.R')

js_age <- read.csv('../interchange/js_age.csv')$x
js_family <- read.csv('../interchange/js_family.csv')$x
js_race <- read.csv('../interchange/js_race.csv')$x
js_housing <- read.csv('../interchange/js_housing.csv')$x

rail <- read.csv('../interchange/rail_intersects.csv')$x
#rail <- read.csv('../interchange/rail_distance.csv')$x^(1/2)
highway <- read.csv('../interchange/highway_intersects.csv')$x
#highway <- read.csv('../interchange/highway_distance.csv')$x^(1/2)
grid_street <- read.csv('../interchange/grid_intersects.csv')$x
#grid_street <- read.csv('../interchange/grid_distance.csv')$x^(1/2)
water <- read.csv('../interchange/water_intersects.csv')$x
#water <- read.csv('../interchange/water_distance.csv')$x^(1/2)

zoning <- read.csv('../interchange/zoning_crosses.csv')$x
elementary_school <- read.csv('../interchange/elementary_schools_crosses.csv')$x
#elementary_school <- read.csv('../interchange/elementary_schools_distances.csv')$x
#elementary_school <- elementary_school^(1/2)
high_school <- read.csv('../interchange/high_schools_crosses.csv')$x
#high_school <- read.csv('../interchange/high_schools_distances.csv')$x
#high_school <- high_school^(1/2)

block_angle <- read.csv('../interchange/block_angles.csv')$x

block_angle <- 1 - (block_angle %% pi/2) / (pi/2)

population <- read.csv('../interchange/min_population.csv')$x
sufficent_pop <- as.numeric(population > 30)

node_labels <- read.csv('../interchange/border.csv')$x

# Spatial Lags
adjacent_edges <- adjacentEdges(blocks.poly)

A <- matrix(0, nrow=length(node_labels), ncol=length(node_labels))

for (i in 1:length(node_labels)) {
  neighbors <- adjacent_edges[[i]]
  for (j in neighbors) {
    A[i,j] = A[j,i] = 1
  }
}



angles <- common::edgeStatistics(all_edges$lines)$angles

angles <- angles %% pi/2

diff_angles <- lapply(1:length(angles),
                      function(i) {
                        (0.5 - abs(0.5 - (abs(angles[i] - angles[adjacent_edges[[i]]]) / (pi/2))))/0.5
                      })

lags <- sapply(1:length(adjacent_edges),
               function(i) {
                 mean(diff_angles[[i]] * node_labels[adjacent_edges[[i]]])
               })

lags[is.nan(lags)] <- 0

group <- rep("1", length(node_labels))

features <- data.frame(sufficent_pop,
                       js_age,
                       js_family,
                       js_race,
                       js_housing,
                       rail,
                       highway,
                       water,
                       zoning,
                       elementary_school,
                       high_school,
                       block_angle,
                       lags,
                       node_labels,
                       group)

best_F = 0
m1 = NA
for (w in seq(0.1, 1, .1)) {
  m <- glm(node_labels ~ (sufficent_pop:(
                             js_age + 
                             js_family +
                             js_race +
                             js_housing) +
                           sufficent_pop*( 
                             rail +
                             highway +
                             water +
                             #zoning +
                             elementary_school +
                             high_school +
                             block_angle +
                             lags
                             )),
           data = features,
           weights = ifelse(node_labels, 1.0, w),
           family=binomial)
  precision <- sum(m$fitted > 0.5 & node_labels)/sum(m$fitted > 0.5)
  recall <- sum(m$fitted > 0.5 & node_labels)/sum(node_labels > 0.5)
  F = precision*recall/(0.5^2*precision + recall)
  print(F)
  if (F > best_F) {
    best_precision = precision
    best_recall = recall
    m1 <- m
    best_F <- F
  }
}

write.csv(m1$fitted.values, "costs.csv", row.names=FALSE)

nodes = blocks.poly

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(nodes,
                           foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)

png("logistic.png")

plot(nodes,
     col="black",
     border="transparent",
     lwd=0.01)


border_lines <- all_edges$lines[m1$fitted.values > 0.3,]

lines(border_lines, col="dark red", lwd=2)

borders <- edgelist[m1$fitted.values > 0.4,]
border_lines <- common::extractBorder(borders, nodes)$lines

lines(border_lines, col="red", lwd=2)

dev.off()

system('python2.6 /home/fgregg/public/watershed-cuts/nabes.py')

segments <- read.table("segments.txt")$V1 + 2

segments <- read.table("merged_segments.csv")$V1

segment_borders <- segments[edgelist[,1]] != segments[edgelist[,2]]
segment_lines <- common::extractBorder(edgelist[segment_borders,],
                                       nodes)$lines

png("segments.png")

plot(blocks.poly, col=sample(colors())[segments], border="transparent")

lines(segment_lines)

dev.off()




