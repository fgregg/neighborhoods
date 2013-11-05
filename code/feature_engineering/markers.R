library(rgeos)
library(spdep)
library(devtools)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)
 
unary <- read.csv('../interchange/unary.csv')
potts_labels <- read.csv('../interchage/potts_labels.csv')

max_labels = apply(unary, 2, which.max)

max_labels = max_labels[apply(unary, 2, max) > -1]

labels = potts_labels$label[max_labels]

write.csv(data.frame(block_id=max_labels, label=labels),
          '../interchange/markers.csv',
          row.names=FALSE)

