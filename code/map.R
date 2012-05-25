library(RColorBrewer)
library(RMySQL)
library(maptools)
library(rgdal)

setwd('/home/fgregg/academic/boundaries')


genLogit <- function(x, A=0, K=1, B=1, v=1, Q=1, M=0) {
  return(A + (K-A)/(1 + Q*exp(-B*(x-M)))^(1/v))
}
  

factorColor <- function(x, alpha.level=.2) {
  rgb(t(col2rgb(factor(x))/255), alpha=alpha.level)
}

standardize <- function(x) {
  mean.x <- mean(x[!is.infinite(x)])
  sd.x <- sd(x[!is.infinite(x)])
  return((x - mean.x)/sd.x)
}

truncate <- function(x, sd) {
  test.x <- standardize(x)
  ifelse(test.x < -1 * sd,
         NA,
         ifelse(test.x > sd,
                NA,
                test.x
                )
         )
}

greyscale <- function(x, sd) {
  test.x <- standardize(x)
  ifelse(test.x < -1 * sd,
         0,
         ifelse(test.x > sd,
                1,
                (test.x + sd)/(2 * sd)
                )
         )
}
  

partialOrder <- function(listings) {
  library(igraph)
  library(Rgraphviz)
  
  wilsonScore <- function(x, n, z) {
    p.hat <- x/n
    z = qnorm(z)
    interval = z*sqrt((p.hat*(1-p.hat))/n + z^2/(4*n^2))
    return((p.hat + z^2/(2*n) - interval)/(1 + z^2/n))
  }

  bayesScore <- function(x, n, x.bar, n.bar) {
    (n.bar * x.bar + x)/(n.bar + n)
  }
  
  plural.listings <- listings[regexpr("/",
                                      as.character(listings$neighborhood)) > 1,]

  plural.counts <- as.data.frame(table(plural.listings$neighborhood))
  splits <- strsplit(as.character(plural.counts$Var1), "/")
  
  splits.d = c()
  for (i in 1:length(splits)) {
    if (length(splits[[i]]) == 2) {
      splits.d = rbind(splits.d, c(splits[[i]], plural.counts[i, "Freq"]))
    }
  }
  splits.d <- as.data.frame(splits.d)
  names(splits.d) <-  c('A', 'B', 'count.ab')
  splits.d$count.ab <- as.numeric(splits.d$count.ab)
  splits.d$count.ba <- splits.d[match(paste(splits.d$B, splits.d$A),
                                      paste(splits.d$A, splits.d$B)),
                              "count.ab"]
  splits.d$count.ba[is.na(splits.d$count.ba)] <- 0
  
  splits.d$count.sum <- splits.d$count.ab + splits.d$count.ba
  
  splits.d$wilson.score <- wilsonScore(splits.d$count.ab,
                                       splits.d$count.sum,
                                       .95)
  
  splits.d$bayes.score <- bayesScore(splits.d$count.ab,
                                     splits.d$count.sum,
                                     .5,
                                     mean(splits.d$count.sum)
                                     )
  neighborhood.order <- graph.data.frame(splits.d[splits.d$bayes.score > .51
                                                  & splits.d$count.ba > 2,
                                                  c("B", "A")]
                                         )
  neighborhood.order <- igraph.to.graphNEL(neighborhood.order)
  nNodes <- length(nodes(neighborhood.order))
  nA <- list()
  nA$fixedsize = rep(FALSE, nNodes)
  nA$shape = rep("box", nNodes)
  nA <- lapply(nA, function(x) {
    names(x) <- nodes(neighborhood.order)
    x
  }
               )
  plot(neighborhood.order, nodeAttrs = nA)
}


# Import Base Layers
com.areas <- readOGR("/home/fgregg/academic/boundaries/chicomm.shp",
                     layer="chicomm",
                     p4s="+proj=longlat")
com.areas <- spTransform(com.areas,
                         CRS("+proj=utm +zone=16 +datum=NAD83")
                         )

tracts <- readOGR("/home/fgregg/academic/boundaries/Census_Tracts.shp",
                     layer="Census_Tracts")
tracts <- spTransform(tracts,
                      CRS("+proj=utm +zone=16 +datum=NAD83")
                      )

gang.areas.ns <- readOGR("/home/fgregg/academic/boundaries/North Side .kml",
                         layer="North Side ")
gang.areas.ss <- readOGR("/home/fgregg/academic/boundaries/southside.kml",
                         layer="South_Chicago")
gang.areas.ns <- spTransform(gang.areas.ns,
                             CRS("+proj=utm +zone=16 +datum=NAD83")
                             )
gang.areas.ss <- spTransform(gang.areas.ss,
                             CRS("+proj=utm +zone=16 +datum=NAD83")
                             )


#streets <- readOGR("Major_Streets1.shp",
#                   layer="Major_Streets1",
#                   p4s="+proj=utm +zone=16 +datum=NAD83")

railroads <- readOGR("Railroads.shp", layer = "Railroads")
railroads <- spTransform(railroads,
                     CRS("+proj=utm +zone=16 +datum=NAD83")
                     )
                     
water <- readOGR("Kmlchicagowaterfeatures.kml", layer = "WATER_FEATURES")
water <- spTransform(water,
                     CRS("+proj=utm +zone=16 +datum=NAD83")
                     )

parks <- readOGR("Kmlchicagoparks.kml", layer = "Chicago Parks")
parks <- spTransform(parks,
                     CRS("+proj=utm +zone=16 +datum=NAD83")
                     )

rent <- readAsciiGrid("/home/fgregg/academic/boundaries/rent.ag")
proj4string(rent) <- CRS("+proj=utm +zone=16 +datum=NAD83")
crime <- readAsciiGrid("/home/fgregg/academic/boundaries/crime.ag")
proj4string(crime) <- CRS("+proj=utm +zone=16 +datum=NAD83")

# Import Craiglists Locations

removeStreets <- function(label, streets) {
  streets <- paste(streets, collapse="|")
  streets <- paste("(^|/)(north |south |east |west )?(", streets, ") *(st|street|ave|avenue|rd|road|blvd|dr|drive|rt|route)*(/|$)", sep='')
  label <- gsub(pattern = streets, replacement="\\1\\5", label)
  label <- gsub(pattern = "(north|south|east|west)? *\\w* (st|street|ave|avenue|rd|road|blvd|dr|drive|rt|route)\\b",
                replacement="", label)
  return(label)
}

cleanCharacters <- function(label) {
  label <- gsub(pattern = "(/) *(.*)", replacement="\\1\\2", label)
  label <- gsub(pattern = "(.*) *(/)", replacement="\\1\\2", label)
  label <- gsub(pattern = "^ *(.*)", replacement="\\1", label)
  label <- gsub(pattern = "(.*) *$", replacement="\\1", label)
  return(label)
}

cleanLabels <- function(label, streets) {
  label <- tolower(label)
  label <- gsub(pattern = "\\b(&amp;|\\+|&)\\b", replacement=" and ", label)
  label <- gsub(pattern = "(&amp;|\\+|&)", replacement="and", label)
  label <- gsub(pattern = "[[#!._{}]+", replacement=" ", label)
  label <- gsub(pattern = "'", replacement="", label)
  label <- gsub(pattern = "^ *(.*)", replacement="\\1", label)
  label <- gsub(pattern = "(.*) *$", replacement="\\1", label)
  label <- gsub(pattern = "  +", replacement=" ", label)
  label <- gsub(pattern = "[-!|@:;,()*]+", replacement="/", label)
  label <- gsub(pattern = " */ *", replacement="/", label)

  ## Addresses

  label <- gsub(pattern = "\\d(\\w| )*($|/)?(.*)", replacement="\\3", label)
  ## Intersections
  label <- gsub(pattern = "(\\w| )* and (\\w| )*/?(.*)", replacement="\\3", label)
  
  ## Zipcode & Phone
  label <- gsub(pattern = "\\W(\\d|/)*$", replacement="", label)

  larger.area <- c("il", "ill", "illinois",
                   "in", "ind", "indiana",
                   "wi",
                   "us", "united states", "north chicago"
                   )
  larger.area <- paste(larger.area, collapse="|")
  larger.area <- paste("( in |\\W+|^)(", larger.area, ")(/|$)", sep='')
  label <- gsub(pattern = larger.area, replacement="", label)
  label <- gsub(pattern = "( in |/|^)chicago(/|$)", replacement="", label)
  label <- gsub(pattern = "chicago\\B", replacement="", label)



  ##Prepositions
  label <- gsub(pattern = "( |/|^)(just )*(near|at|on|by|close to|on|off|next|west of|east of|south of|north of|minutes from|minutes to|steps to|steps from|across from|right by|good part) (\\w| )*(/|$)",
                replacement="\\5", label)
  label <- gsub(pattern = " (location|area|campus)( |/|$)", replacement="\\2", label)
  label <- gsub(pattern = "(/|^| )(prime|downtown|dt|down town|downtwon|downtnown|dowtown|dwntwn|dowtnown|center|center of|heart of|downtown|nice part) ", replacement="\\1", label)



  label <- gsub(pattern = "\\be\\b", replacement="east", label)
  label <- gsub(pattern = "\\bw\\b|\\bwests\\b", replacement="west", label)
  label <- gsub(pattern = "\\bn\\b", replacement="north", label)
  label <- gsub(pattern = "\\bs\\b", replacement="south", label)
  label <- gsub(pattern = "\\bne\\b", replacement="northeast", label)
  label <- gsub(pattern = "\\bnw\\b", replacement="northwest", label)
  label <- gsub(pattern = "\\bse\\b", replacement="southeast", label)
  label <- gsub(pattern = "\\bsw\\b", replacement="southwest", label)

  label <- gsub(pattern = "(northeast|northwest|southeast|southwest) ",
                replacement="", label)

  


  ## Spelling 
  label <- gsub(pattern = "(sq|squre|suqare|square|sqaure)(/|$)",
                replacement="square\\2", label)
  label <- gsub(pattern = "( sq)([^u])", replacement="square\\2", label)
  label <- gsub(pattern = "pr*k\\b",
                replacement="park", label)
  label <- gsub(pattern = "cooridor", replacement="corridor", label)
  label <- gsub(pattern = "\\bhts\\b|heigths", replacement="heights", label)
  label <- gsub(pattern = "\\bctr\\b", replacement="center", label)
  label <- gsub(pattern = "\\blp\\b", replacement="loop", label)
  label <- gsub(pattern = "\\bpl\\b", replacement="place", label)
  label <- gsub(pattern = "\\bchgo\\b", replacement="chicago", label)
  label <- gsub(pattern = "esates", replacement="estates", label)
  label <- gsub(pattern = "(red|green|brown|pink|purple|orange|blue) *lines*( stop| train)*", replacement="", label)
  label <- gsub(pattern = "(dist|distr|district|distrit|distric|disctrict)(/|$)",
                replacement="district\\2", label)
  
  ## Canonical
  label <- gsub(pattern = "(^|.)saint ", replacement="\\1st ", label)
  label <- gsub(pattern = "(^|/)mt ", replacement="\\1mount ", label)
  label <- gsub(pattern = "the ", replacement="", label)
  label <- gsub(pattern = "\\bburbs*\\b", replacement="suburbs", label)
  

  label <- gsub(pattern = "belmont/cragin", replacement="belmont cragin", label)
  label <- gsub(pattern = "craigin|crain", replacement="cragin", label)
  label <- gsub(pattern = "brideport", replacement="bridgeport", label)
  label <- gsub(pattern = "berwin", replacement="berwyn", label)
  label <- gsub(pattern = "boys town", replacement="boystown", label)
  label <- gsub(pattern = "buck town|bucktowm", replacement="bucktown", label)
  label <- gsub(pattern = "bronzville|bronzevillw", replacement="bronzeville", label)
  label <- gsub(pattern = "(buck)(/|$)", replacement="bucktown\\2", label)
  label <- gsub(pattern = "(buena)(/|$)", replacement="buena park\\2", label)
  label <- gsub(pattern = "budolong", replacement="budlong", label)
  label <- gsub(pattern = "bwrn", replacement="bryn", label)
  label <- gsub(pattern = "brwn", replacement="bryn", label)
  label <- gsub(pattern = "desplaines", replacement="des plaines", label)
  label <- gsub(pattern = "de paul|depaul university", replacement="depaul", label)
  label <- gsub(pattern = "edge water", replacement="edgewater", label)
  label <- gsub(pattern = "evenston", replacement="evanston", label)
  label <- gsub(pattern = "goldcoast|gold goast|goald coast|gold coat", replacement="gold coast", label)
  label <- gsub(pattern = "chicago gold coast", replacement="gold coast", label)
  label <- gsub(pattern = "greek town", replacement="greektown", label)
  label <- gsub(pattern = "harvery", replacement="harvey", label)
  label <- gsub(pattern = "(humbolt|humboldtd|humboltd)( |$|/)", replacement="humboldt\\2", label)
  label <- gsub(pattern = "(humboldt)(/|$)", replacement="humboldt park\\2", label)
  label <- gsub(pattern = "hydepark", replacement="hyde park", label)
  label <- gsub(pattern = "egewater", replacement="edgewater", label)
  label <- gsub(pattern = "edgwater", replacement="edgewater", label)
  label <- gsub(pattern = "chicago hyde park", replacement="hyde park", label)
  label <- gsub(pattern = "illinois institute of technology", replacement="iit", label)
  label <- gsub(pattern = "\\bitt\\b", replacement="iit", label)
  label <- gsub(pattern = "illinois *medical *district", replacement="medical district", label)
  label <- gsub(pattern = "\\bimd\\b", replacement="medical district", label)
  label <- gsub(pattern = "jeff park", replacement="jefferson park", label)
  label <- gsub(pattern = "kelvin park", replacement="kelvyn park", label)
  label <- gsub(pattern = "lagrange", replacement="la grange", label)
  label <- gsub(pattern = "larmie", replacement="laramie", label)
  label <- gsub(pattern = "lake view|lakview|lakeivew|lekaview", replacement="lakeview", label)
  label <- gsub(pattern = "lake shore east", replacement="lakeshore east", label)
  label <- gsub(pattern = "lincolnpk|lincon park|linc park|linocln park|lincolnpark", replacement="lincoln park", label)
  
  label <- gsub(pattern = "little itally|littleitaly|little itay|lil italy", replacement="little italy", label)
  label <- gsub(pattern = "logansquare", replacement="logan square", label)
  label <- gsub(pattern = "loyola university|loyloa", replacement="loyola", label)
  label <- gsub(pattern = "(logan)(/|$)", replacement="logan square\\2", label)
  label <- gsub(pattern = "mag *mile", replacement="magnificent mile", label)
  label <- gsub(pattern = "six corner$", replacement="six corners", label)
  label <- gsub(pattern = "med district", replacement="medical district", label)
  label <- gsub(pattern = "millennium|millenniun", replacement="millenium", label)
  label <- gsub(pattern = "mt greenwood", replacement="mount greenwood", label)
  label <- gsub(pattern = "new east side", replacement="new eastside", label)
  label <- gsub(pattern = "napervville", replacement="naperville", label)
  label <- gsub(pattern = "nobel", replacement="noble", label)
  label <- gsub(pattern = "(noble)(/|$)", replacement="noble square\\2", label)
  label <- gsub(pattern = "northcenter", replacement="north center", label)
  label <- gsub(pattern = "northwestchicago", replacement="northwest chicago", label)
  label <- gsub(pattern = "oldtown", replacement="old town", label)
  label <- gsub(pattern = "old town triangle", replacement="old town", label)
  label <- gsub(pattern = "(old irving)(/|$)", replacement="old irving park\\2", label)
  label <- gsub(pattern = "pullam", replacement="pullman", label)
  label <- gsub(pattern = "plainfiedl", replacement="plainfield", label)
  label <- gsub(pattern = "riverwest", replacement="river west", label)
  label <- gsub(pattern = "rivernorth|riv north", replacement="river north", label)
  label <- gsub(pattern = "(rodgers|roders)", replacement="rogers", label)
  label <- gsub(pattern = "roger park", replacement="rogers park", label)
  label <- gsub(pattern = "west rogers(/|$)", replacement="west rogers park\\1", label)
  label <- gsub(pattern = "rollingmeadows", replacement="rolling meadows", label)
  label <- gsub(pattern = "roscoe *vil\\w*",
                replacement="roscoe village", label)
    label <- gsub(pattern = "roscoe(/|$)",
                  replacement="roscoe village\\1", label)
  label <- gsub(pattern = "regents park buildings*", replacement="regents park", label)
  label <- gsub(pattern = "roosevelt collections*", replacement="roosevelt connection", label)
  label <- gsub(pattern = "southloop", replacement="south loop", label)
  label <- gsub(pattern = "soutport", replacement="southport", label)
  label <- gsub(pattern = "southshore", replacement="south shore", label)
  label <- gsub(pattern = "stbens", replacement="st bens", label)
  label <- gsub(pattern = "(streerterville|sreeterville|streeter ville|streetersville|streetervile|streetville)",
                replacement="streeterville", label)
  label <- gsub(pattern = "tri/taylor", replacement="tri taylor", label)
  label <- gsub(pattern = "uk\\w* vil\\w*\\b", replacement="ukrainian village", label)

  label <- gsub(pattern = "ukv\\b", replacement="ukrainian village", label)
  label <- gsub(pattern = "uni\\w* vil\\w*\\b", replacement="university village", label)
  label <- gsub(pattern = "westridge", replacement="west ridge", label)
  label <- gsub(pattern = "wickerpark", replacement="wicker park", label)
  label <- gsub(pattern = "(wicker)(/|$)", replacement="wicker park\\2", label)
  label <- gsub(pattern = "(wriglyville|wrigglyville|wriggleyville|wrigleville|wrigelyville|wrigleyvill|wrigleyvile|wrigleyviile|wirgleyville)(/|$)",
                replacement="wrigleyville\\2", label)

  label <- gsub(pattern = "(^|/)south side", replacement="\\1southside", label)
  label <- gsub(pattern = "(^|/)west side(/|$)", replacement="\\1westside\\2", label)
  label <- gsub(pattern = "(^|/)north side(/|$)", replacement="\\1northside\\2", label)
  label <- gsub(pattern = "(^|/)south suburb(/|$)", replacement="\\1south suburbs\\2", label)
  label <- gsub(pattern = " site(/|$)", replacement=" side\\1", label)
  label <- gsub(pattern = "near west(/|$)", replacement="near west side\\1", label)
  label <- gsub(pattern = "fulton riv district", replacement="fulton river district", label)
  label <- gsub(pattern = "prairieshores", replacement="prairie shores", label)
  label <- gsub(pattern = "hazel crest", replacement="hazelcrest", label)
  label <- gsub(pattern = "chatam", replacement="chatham", label)
  label <- gsub(pattern = "budlong wds", replacement="budlong woods", label)
  label <- gsub(pattern = "brynmawr", replacement="bryn mawr", label)
  label <- gsub(pattern = "cresthill", replacement="crest hill", label)
  label <- gsub(pattern = "dicersey", replacement="diversey", label)
  label <- gsub(pattern = "westtown", replacement="west town", label)
  label <- gsub(pattern = "eastvillage", replacement="east village", label)
  label <- gsub(pattern = "westloop", replacement="west loop", label)
  label <- gsub(pattern = "oakbrook", replacement="oak brook", label)
  label <- gsub(pattern = "northpark", replacement="north park", label)
  label <- gsub(pattern = "albanay", replacement="albany", label)
  label <- gsub(pattern = "ravenswoods", replacement="ravenswood", label)
  label <- gsub(pattern = "andersonville*", replacement="andersonville", label)
  label <- gsub(pattern = "midloth\\b", replacement="midlothian", label)
  label <- gsub(pattern = "lkshore east", replacement="lakeshore east", label)
  label <- gsub(pattern = "fulton district|fulton river distr\\b|fulton river($|/)", replacement="fulton river district", label)
  label <- gsub(pattern = "med center|med ctr", replacement="medical center", label)
  label <- gsub(pattern = "bloomindale|bloomingale", replacement="bloomingdale", label)

  label <- gsub(pattern = "(northwestern|nu|nwu|northwestern u\\b|northwestern edu)(/|$)",
                replacement="northwestern university\\2", label)
  
  label <- gsub(pattern = "univ(/|$)", replacement="university\\1", label)
  label <- gsub(pattern = "univercity", replacement="university", label)
  label <- gsub(pattern = "(university of chicago|uofc|u of c|u of chicago|univ of chicago)",
                replacement="uchicago", label)

  label <- gsub(pattern = "hyde park/u(\\w| )$",
                replacement="hyde park/uchicago", label)
  label <- gsub("^ukranian$", "ukrainian village", label)
  label <- gsub("ukrainian$", "ukrainian village", label)


  places <- c("andersonville", "arlington heights", "austin",
              "aurora", "avondale", "belmont cragin",
              "belmont central", "berwyn", "boystown", "bridgeport",
              "bucktown", "buena park", "budlong woods", "bryn mawr",
              "chinatown", "east village", "edgewater",
              "edgewater beach", "edison park", "evanston",
              "fulton river district", "gage park", "gold coast",
              "greektown", "harvey", "humboldt park", "hyde park",
              "iit", "kelvyn park", "kensington", "la grange",
              "lakeview", "lakeview east", "lakeshore east",
              "lawndale", "libertyville", "lincoln park",
              "little italy", "logan square", "loop", "loyola",
              "margate park", "mayfair", "magnificent mile",
              "medical district", "millenium park", "mount greenwood",
              "mundelein", "naperville", "new eastside", "new city",
              "noble square", "north center", "northlake",
              "northbrook", "northwestern university", "old town",
              "old irving park", "ohare", "pilsen", "palos heights",
              "pullman", "rogers park", "rogers park beach",
              "river north", "river west", "roscoe village",
              "roseland", "south shore", "southside", "south suburbs",
              "southport corridor", "st bens", "streeterville",
              "tri taylor", "uchicago", "uptown", "uic",
              "ukrainian village", "union station",
              "university village", "villa park", "washington park",
              "west loop", "west pullman", "west town", "west ridge",
              "wheeling", "wicker park", "wrigleyville", "woodlawn",
              "wheeling", "englewood", "tinley park")

  places <- paste(places, collapse="|")

  stop.words <- c("east",
                  "west",
                  "north",
                  "south",
                  "beach",
                  "manor",
                  "park"
                  )
  stop.words <- paste(stop.words, collapse="|")

  places.forward <- paste("(", places, ")(\\B|( ))(?!(", stop.words, ")\\b)", sep='')
  places.backward <- paste("(?<!(east|west|outh|orth))(\\B|( ))(east |west |south |north )?(",
                           places,
                           ")",
                           sep = '')

  print(places.backward)
  label <- gsub(pattern = places.forward, replacement = "\\1/\\5", label, perl=TRUE)
  label <- gsub(pattern = places.backward, replacement = "\\1/\\4\\5", label, perl=TRUE)

  ##  Combine Places
  label <- gsub("west lincoln park", "lincoln park", label)
  label <- gsub("east lincoln park", "lincoln park", label)
  label <- gsub("north lakeview", "lakeview", label)
  label <- gsub("west lakeview", "lakeview", label)
  label <- gsub("east lakeview", "lakeview", label)
  label <- gsub("lakeview east", "lakeview", label)
  label <- gsub("west wicker park", "wicker park", label)
  label <- gsub("west ukrainian village", "ukrainian village", label)
  label <- gsub("east ukrainian village", "ukrainian village", label)
  label <- gsub("west bucktown", "bucktown", label)
  label <- gsub("east pilsen", "pilsen", label)
  label <- gsub("east humboldt park", "humboldt park", label)
  label <- gsub("north lawndale", "lawndale", label)
  label <- gsub("east rogers park", "rogers park", label)
  label <- gsub("west logan square", "logan square", label)
  label <- gsub("uic east", "uic", label)
  label <- gsub("south evanston", "evanston", label)
  label <- gsub("west pilsen", "pilsen", label) 

  # remove Streets
  label <- removeStreets(label, streets)

  label <- cleanCharacters(label)

  forced.excluded <- c("deerfield", "downers grove", "valparaiso",
                       "north michigan ave", "cta", "northwest",
                       "michiana shores", "lake shore", "1", "uc",
                       "antioch", "addison", "highland", "lakeshore",
                       "presidential towers", "davis el",
                       "alta at k station west loop", "dolton",
                       ".* can help", "highland", "rt", "blue line",
                       "red line", "downtown", "mcclurg", "griffith",
                       "ish", "chicago downtown", "chicago", "niu",
                       "west", "wells", "north chicago",
                       "west chicago", "clark", "north chicagoland",
                       " *border", "paulina", "sheffield", "belmont",
                       "harvard", "diversey", "downtown", "damen",
                       "chic", "alta", "northwest chicago",
                       "lakefront", "gary", "near","ridge", "red",
                       "brown line", "broadway", "south", "westmont",
                       "batavia", "north", "elm", "peterson",
                       "stroger", "shops", "pubs", "pt", "park",
                       "lemont", "ida", "i", "area", "route",
                       "northwest side", "northside", "side", "north land", "albany", "wrigley", "suburbs", "in", "el", "beach", "bellwood", "irving", "hwy", "side of chicago", "right", "city of chicago", "metra", "chicago northwest", "of chicago", 'andgt', 'and gt', 'andlt', 'k station', 'l', 'lsd', 'neighborhood', 'northside chicago', 'river', 'text', "steger", "east", "oak", "chicago north", "university", "walk to downtown", "green")


  forced.excluded <- paste(forced.excluded, collapse="|")
  forced.excluded <- paste("(^|/)(", forced.excluded, ")(/|$)", sep='')
  label <- gsub(pattern = forced.excluded, replacement="\\3", label)
  label <- gsub(pattern = forced.excluded, replacement="\\3", label)

  
  label <- gsub(pattern = "^/*", replacement="", label)
  label <- gsub(pattern = "/*$", replacement="", label)
  label <- gsub(pattern = "//+", replacement="/", label)
  label <- gsub(pattern = "^ *", replacement="", label)
  label <- gsub(pattern = " *$", replacement="", label)

  label <- gsub(pattern = places.forward, replacement = "\\1/\\5", label, perl=TRUE)
}

splitNeighborhoods <- function(listings, streets) {
  split.listings <- listings[0,]
  plural.listings <- listings[regexpr("/",
                                      as.character(listings$neighborhood)) > 1,]
  labels <- strsplit(plural.listings[,"neighborhood"], "/")
  for (i in 1:length(labels)) {
    split.listing <- cbind(plural.listings[i, 'y'],
                           plural.listings[i, 'x'],
                           labels[[i]]
                           )
    split.listings <- rbind(split.listings,
                            split.listing)
  }
  names(split.listings) <- names(listings)
  listings <- listings[regexpr("/", as.character(listings$neighborhood)) < 1,]
  listings <- rbind(listings, split.listings)
  listings$x <- as.numeric(listings$x)
  listings$y <- as.numeric(listings$y)
  
  listings$neighborhood <- removeStreets(listings$neighborhood, streets)

  return(listings)
}

  
spdfCrop <- function(spdf, x.min, x.max) {
   return(spdf[spdf@coords[,1] > x.min &
               spdf@coords[,1] < x.max,])
 }

calculateCentroids <- function(spdf, grouping) {
  # Calculate
  return(aggregate(spdf@coords,  
                   by=grouping,
                   FUN = function(x) {
                     quantile(x, type=3)[3]
                   }
                   )
         )
}

aggregateLabels <- function(spdf, grouping.column){
  spdf <- spdf[!is.na(spdf@data[,grouping.column]),]
  spdf <- spdf[spdf@data[,grouping.column] != "",]
  
  spdf@data[, grouping.column] <- factor(spdf@data[, grouping.column])
  x <- cbind(calculateCentroids(spdf,
                                list(spdf@data[, grouping.column])),
             as.numeric(table(spdf@data[, grouping.column]))
             )
  names(x) <- c(grouping.column, "x", "y", "count")
  x <- SpatialPointsDataFrame(coords = x[, c("x", "y")],
                              data = x[, c(grouping.column, "count")],
                              proj4string = CRS(proj4string(spdf))
                              )
                              
                                  
  return(x)
}
                              
labelPlot <- function(spdf, label.col, cex.size=1, color="#00000063") {
# Label all places with four or more listings without a '/'
  text(spdf@coords,
       labels = spdf@data[, label.col],
       cex=cex.size,
       offset=0,
       col=color)
}

basePlot <- function(...) {
#  plot(streets[as.numeric(as.character(streets$CLASS)) > 2,],
#       col="lightgrey",
#       ...)
  plot(com.areas, border="grey", add=TRUE, lwd=.1)
  plot(water, col="#C0E7F3", border=0, add=TRUE)
  plot(parks[sapply(slot(parks, "polygons"), slot, "area") > 100000,],
       col="#DBEADC", border=0, add=TRUE)
  lines(railroads, lty=2, col="tan")
  
}
  
simplePlot <- function(x.lim = c(429300.8, 451830.8),
                       y.lim = c(4620821, 4655485)) {
  plot(x.lim, y.lim,
       type="n",
       asp=1,
       axes=FALSE,
       xlab="",
       ylab="")
}

chicagoSPDF <- function(df, coord.col, data.col) {
spdf <- SpatialPointsDataFrame(coords=df[, coord.col],
                                data=data.frame(df[, data.col]),
                                proj4string = CRS("+proj=longlat")
                                           )
names(spdf) <- data.col
spdf <- spdfCrop(spdf, -90, -84)
spdf <- spTransform(spdf,
                         CRS("+proj=utm +zone=16 +datum=NAD83")
                         )
return(spdf)
}



streets <- c("western", "damen", "division", "california", "delaware",
             "maple", "miller", "jefferson", "clarendon", "clybourn",
             "higgins", "berteau", "milwaukee", "diversey", "kenmore",
             "addison", "lockport", "sheridan", "broadway",
             "wellington", "waveland", "southport", "lakeshore drive",
             "lake shore drive", "foster", "lasalle", "sheffield",
             "halsted", "belmont", "ogden", "armitage", "cornelia",
             "dearborn", "lincoln", "montrose", "paulina", "taylor",
             "fullerton", "ashland", "racine", "wilmette", "wells",
             "wellington", "rush", "sherman", "lawrence", "church",
             "paulina", "pulaski", "melrose", "clark", "barry",
             "harlem", "lake", "laramie", "diversey", "madison",
             "michigan", "randolph", "roosevelt", "schiller",
             "wabash", "cornelia", "deming", "chestt", "grace",
             "state", "grand", "cermak", "kedzie", "kinzie",
             "webster", "chestnut", "wilson", "whipple", "wilton",
             "dickens","huron","wolcott",
             "wrightwood","belden","oakdale","superior","greenview","polk",
             "kimball", "erie", "devon", "oakley", "cumberland",
             "ontario", "fulton", "ohio", "noyes", "dempster",
             "pratt", "aldine", "morse", "wacker drive", "halsted",
             "harison", "harrison", "la salle", "lake shore",
             "euclid", "augusta", "franklin", "washington", "clinton", "marshfield", "granville", "sunnyside", "rockwell", "beecher", "pine grove", "grace", "leland", "wacker", "columbus")



listings.query <- "
select distinct
X(lat_long) as y,
Y(lat_long) as x,
label.label as neighborhood
from location, label
where location.location_id = label.location_id"

price.query <- "
select distinct
X(lat_long) as x,
Y(lat_long) as y,
price,
bedrooms
from location,
((select
  location_id,
  price,
  bedrooms
  from price, label
  where price.listing_id = label.listing_id)
 as d1)
where location.location_id = d1.location_id"


con <- dbConnect(MySQL(), dbname="neighborhood")
listings <- dbGetQuery(con, listings.query)
dbDisconnect(con)

gang.query <- "
select distinct
X(lat_long) as x,
Y(lat_long) as y,
sets_clean as gang,
description as description
from tag"

con <- dbConnect(MySQL(), dbname="chicago_gangs")
gangs <- dbGetQuery(con, gang.query)
dbDisconnect(con)



gangs <- chicagoSPDF(gangs, c("x", "y"), c("gang", "description"))


#listings <- utmConvert(listings)
#listings <- listings[listings$y > 4400000 & listings$x > 300000, ]

#listings$neighborhood <- tolower(listings$neighborhood)
listings$neighborhood <- cleanLabels(listings$neighborhood, streets)
listings <- splitNeighborhoods(listings, streets)
listings <- unique(listings)
listings <- chicagoSPDF(listings, c("x", "y"), "neighborhood")

centroids <- aggregateLabels(listings, "neighborhood")



x.lim <- c(median(listings@coords[,1]) - mad(listings@coords[,1])*3,
           median(listings@coords[,1]) + mad(listings@coords[,1])*3)
y.lim <- c(median(listings@coords[,2]) - mad(listings@coords[,2])*5,
           median(listings@coords[,2]) + mad(listings@coords[,2])*3)


palette(c(brewer.pal(12, "Set3"), "#777777"))


listings.narrow <- listings[listings@data$neighborhood %in%
                            centroids@data[regexpr("/",
                            as.character(centroids@data$neighborhood)) < 1
                            & centroids@data$count >= 3,
                            "neighborhood"],]
listings.narrow$neighborhood <- factor(listings.narrow$neighborhood)

pdf("~/academic/boundaries/test.pdf", paper="letter", pointsize=5)
simplePlot(x.lim, y.lim)
#basePlot(add=TRUE)
points(listings.narrow,
       col=factorColor(listings.narrow$neighborhood),
       pch=16,
       cex=.8)
#labelPlot()
dev.off()

pdf("~/academic/boundaries/test1.pdf", paper="letter", pointsize=5)
simplePlot(x.lim, y.lim)
basePlot(add=TRUE)
points(listings.narrow,
       col=factorColor(listings.narrow$neighborhood),
       pch=16,
       cex=.8)
centroids.narrow <- centroids[regexpr("/", as.character(centroids@data$neighborhood)) < 1 & centroids@data$count >= 3,]
labelPlot(centroids.narrow,
          "neighborhood", .5,
          color=rgb(0, 0, 0, genLogit(centroids.narrow$count,
            A=0,
            K=.6,
            M=20,
            B=.8,
            Q=10,
            v=10)))
text(451000, 4650000, paste(dim(unique(listings.narrow@coords))[1], "locations,",
                            length(levels(listings.narrow$neighborhood)),
                            "places"))
dev.off()

pdf("~/academic/boundaries/test2.pdf", paper="letter", pointsize=5)
simplePlot(x.lim, y.lim)
basePlot(add=TRUE)
points(listings, pch=46, cex=.3)
text(450000, 4650000, paste(dim(unique(listings@coords))[1], "locations"))
dev.off()

pdf("~/academic/boundaries/allTogether.pdf", paper="letter", pointsize=5)
simplePlot(x.lim, y.lim)
basePlot(add=TRUE)
points(listings.narrow,
       col=factorColor(listings.narrow$neighborhood),
       pch=16)
contour(rent, "rent.ag",
        levels=log(c(500, 600, 750, 1000, 1200, 1500)),
        labels=c('500', '600', '750', '1000', '1200', '1500'),
        add=TRUE, col="brown")
dev.off()

pdf("~/academic/boundaries/crime.pdf", paper="letter", pointsize=5)
simplePlot(x.lim, y.lim)
basePlot(add=TRUE)
points(listings.narrow,
       col=factorColor(listings.narrow$neighborhood),
       pch=16)
contour(crime, "crime.ag",
        levels=c(.2, .4, .6),
        add=TRUE, col="brown")
dev.off()

gangs <- gangs[!gangs$gang %in% c("", "Gang Graffiti", "Albany Park",
                                  "Warlords (Wicker Park)-Now Extinct",
                                  "Stone Kents (Extinct)",
                                  "Outlaw Bloods (Extinct?)",
                                  "Insane Hoods (Retired/extinct)",
                                  "Maniac Latin Disciples/YLO Disciples",
                                  "AurorA") ,]
gangs$gang <- gsub("^Imperial Gangsters",
                   "Almighty Imperial Gangsters",
                   gangs$gang)
gangs$gang <- gsub("^Latin Kings",
                   "Almighty Latin Kings",
                   gangs$gang)
gangs$gang <- gsub("^Latin Disciples",
                   "Maniac Latin Disciples",
                   gangs$gang)
gangs$gang <- gsub("^Latin Brothers",
                   "Insane Latin Brothers",
                   gangs$gang)
gangs$gang <- gsub("^Insane Orquestra Albany.*",
                   "Insane Orquestra Albany",
                   gangs$gang)
gangs$gang <- gsub("^Throop Boys.*",
                   "Throob Boys",
                   gangs$gang)
gangs$gang <- gsub("^Cermak Boys.*",
                   "Cermak Boys",
                   gangs$gang)
gang.centroids <- aggregateLabels(gangs, "gang")
gangs$gang <- factor(gangs$gang)
gangs$gang <- factor(gangs$gang, levels(gangs$gang)[order(-gang.centroids$count)])
gangs <- gangs[gangs@data$gang %in%
                      gang.centroids@data[regexpr("/",
                      as.character(gang.centroids@data$gang)) < 1 &
                      gang.centroids@data$count >= 5, "gang"],]
pdf("~/academic/boundaries/gangs.pdf", paper="letter", pointsize=5)
#png("~/academic/boundaries/gangs.png", width=1000, height=3000)
simplePlot(x.lim, y.lim)
basePlot(add=TRUE)
plot(gang.areas.ns, border="grey", add=TRUE, lwd=.1)
plot(gang.areas.ss, border="grey", add=TRUE, lwd=.1)
points(gangs, col=factorColor(gangs$gang),
       pch = unclass(factor(gangs$gang)) %% 4 + 15,
       cex=.6)
text(451000, 4650000, paste(dim(gangs)[1], "locations,",
                            length(levels(as.factor(gangs$gang)))-1,
                            "gangs"))
contour(crime, "crime.ag",
        levels=c(.1, .2, .4, .6),
        add=TRUE, col="brown")
legend(451000, 4648000,
       legend = levels(gangs$gang),
       pch = 1:nlevels(gangs$gang) %% 4 + 15,
       col = factorColor(1:nlevels(gangs$gang), 1),
       bty = "n",
       ncol = 2,
       cex = .8
       )
labelPlot(gang.centroids, "gang", .2)
dev.off()


