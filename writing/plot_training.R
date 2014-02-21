library(devtools)

pkg <- devtools::as.package('../code/common')
devtools::load_all(pkg)

map_colors = sample(sample(colors()))

neighborhoods <- read.csv("../code/interchange/ks_label_semantic.csv")$x 

plot(blocks.poly,
     col=map_colors[neighborhoods],
     border="transparent")
