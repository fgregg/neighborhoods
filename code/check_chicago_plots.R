pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')

devtools::load_all(pkg)

setwd('training')
source('chicago_features.R')
setwd('..')

plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$rail + 1])
 
plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$highway + 1])
 
plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$grid_street + 1])

plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$water + 1])
 
plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$elementary_school + 1])
 
plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$high_school + 1])
 
plot(chicago.all_edges$lines, col=c("#00000022", "red")[features$sufficient_pop + 1])
 
plot(chicago.all_edges$lines, col=rgb(colorRamp(c("lightgrey", "red"))(features$block_angle)/255))
 
plot(chicago.all_edges$lines[features$sufficient_pop==1], col=rgb(colorRamp(c("lightgrey", "red"))(features$js_race[features$sufficient_pop==1])/255))

plot(chicago.all_edges$lines[features$sufficient_pop==1], col=rgb(colorRamp(c("lightgrey", "red"))(features$js_age[features$sufficient_pop==1])/255))
 
plot(chicago.all_edges$lines[features$sufficient_pop==1], col=rgb(colorRamp(c("lightgrey", "red"))(features$js_family[features$sufficient_pop==1])/255))
 
plot(chicago.all_edges$lines[features$sufficient_pop==1], col=rgb(colorRamp(c("lightgrey", "red"))(features$js_housing[features$sufficient_pop==1])/255))
 


