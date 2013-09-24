library(rgeos)
library(spdep)
library(devtools)

library(rgeos)
library(spdep)

library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

js_age <- read.csv('../interchange/js_age.csv')$x
js_family <- read.csv('../interchange/js_family.csv')$x
js_race <- read.csv('../interchange/js_race.csv')$x
js_housing <- read.csv('../interchange/js_housing.csv')$x

#rail <- read.csv('../interchange/rail_intersects.csv')$x
rail <- read.csv('../interchange/rail_distance.csv')$x^(1/2)
#highway <- read.csv('../interchange/highway_intersects.csv')$x
highway <- read.csv('../interchange/highway_distance.csv')$x^(1/2)
#grid_street <- read.csv('../interchange/grid_intersects.csv')$x
grid_street <- read.csv('../interchange/grid_distance.csv')$x^(1/2)
#water <- read.csv('../interchange/water_intersects.csv')$x
water <- read.csv('../interchange/water_distance.csv')$x^(1/2)

zoning <- read.csv('../interchange/zoning_crosses.csv')$x
#elementary_school <- read.csv('../interchange/elementary_schools_crosses.csv')$x
elementary_school <- read.csv('../interchange/elementary_schools_distances.csv')$x
elementary_school <- elementary_school^(1/2)
#high_school <- read.csv('../interchange/high_schools_crosses.csv')$x
high_school <- read.csv('../interchange/high_schools_distances.csv')$x
high_school <- high_school^(1/2)

block_angle <- read.csv('../interchange/block_angles.csv')$x

population <- read.csv('../interchange/min_population.csv')$x
sufficent_pop <- as.numeric(population > 30)

features <- data.frame()

node_labels <- read.csv('../interchange/border.csv')$border

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
                             block_angle)),
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

nodes = blocks.poly

# Topology of Block Connectivity
neighbors <-spdep::poly2nb(nodes,
                           foundInBox=rgeos::gUnarySTRtreeQuery(nodes))

# Calculate 'edge features' will be node features in training
edgelist <- common::nb2edgelist(neighbors)

png("logistic.png")

plot(nodes,
     col="black",
     border=rgb(0,0,0,0.04),
     lwd=0.01)


borders <- edgelist[m1$fitted.values > 0.3,]
border_lines <- common::extractBorder(borders, nodes)$lines

lines(border_lines, col="dark red", lwd=2)

borders <- edgelist[m1$fitted.values > 0.4,]
border_lines <- common::extractBorder(borders, nodes)$lines

lines(border_lines, col="red", lwd=2)

dev.off()
