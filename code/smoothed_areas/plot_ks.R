source('ks_to_blocks.R')
 

map.colors = rep(c('#B2DF8A', "#A6CEE3", "#1F78B4", "#33A02C",
                   "#FB9A99", "#E31A1C", "#FDBF6F"),
             length.out = length(neighborhoods))

pdf("blocks.pdf")
#Plot KDE
plot(blocks.poly,
     col = common::probColors(map.colors, classes),
     border=rgb(0,0,0,0.04),
     lwd=0.01)
dev.off()

n.points <- 2^9
C = rasterTrainKDE(listings, neighborhoods, range, n.points, 0.0000012)

pdf("smooth.pdf")
common::basePlot(range)
common::plotPredictions(C, map.colors, 100000, range)

neighborhood_listings <- listings[listings$neighborhood %in% neighborhoods,]

hood.medoids <- aggregate(neighborhood_listings@coords, 
                          by=list(neighborhood_listings$neighborhood), 
                          median)
names(hood.medoids) <- c("neighborhood", "x", "y")

hood.n <- aggregate(neighborhood_listings$neighborhood ,
                    by=list(neighborhood_listings$neighborhood), 
                    length)
names(hood.n) <- c("neighborhood", "n")

hood.medoids <- merge(hood.medoids, hood.n)

neighborhoods <- neighborhoods[neighborhoods != "southport corridor"]
neighborhood.medoids <- hood.medoids[match(neighborhoods, 
                                           hood.medoids$neighborhood),]


text(neighborhood.medoids[, c('x', 'y')],
     labels = paste(neighborhood.medoids$neighborhood,
                    neighborhood.medoids$n, sep=", "),
     col= "#00000063",
     cex=.3)

text(hood.medoids[hood.medoids$neighborhood == "southport corridor", 
                  c('x', 'y')],
     labels = paste("southport corridor",
       hood.medoids[hood.medoids$neighborhood == "southport corridor", "n"], sep=", "),
     col= "#00000063",
     cex=.3,
     srt=90)

dev.off()
