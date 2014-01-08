pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

pkg <- devtools::as.package('~/academic/neighborhoods/code/chicago')
devtools::load_all(pkg)

community_area <- readOGR("../../admin_areas/Kmlcommunityareas.kml",
                     layer = "COMMUNITYAREA")
community_area <- spTransform(community_area,
                              CRS(projection)
                              )


