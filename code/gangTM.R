library(tm)
library(RMySQL)

gang.query <- "
select tags, title, description, sets_clean
from tag where Intersects(lat_long, GeomFromText('POLYGON((-88 41, -87 40, -87 42, -88 42, -88 41))'))"

tags.query <- "select tags, title, description, sets_clean from tag where tags is not null"


con <- dbConnect(MySQL(), dbname="chicago_gangs")
gangs <- dbGetQuery(con, gang.query)
dbDisconnect(con)

gangs.text <- do.call("paste", gangs[, c(1,2,3)])
gangs.text <- data.frame(gangs.text)
gangs.labels <- gangs$sets_clean

gangs.text <- Corpus(DataframeSource(gangs.text))

gangsTDM <- TermDocumentMatrix(gangs.text,
                               control = list(weighting = weightTfIdf)
                               )

#gangsTDM <- removeSparseTerms(gangsTDM, .99)

#gangsTDM.clust <- hclust(dissimilarity(gangsTDM, method="cosine"), method="ward")

#plot(gangsTDM.clust)

