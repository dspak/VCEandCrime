# Plot shape files

# load/install required packages
list.of.packages <- c("maptools", "ggmap", "rgdal", "spatialEco", 
                      "RColorBrewer", "plyr", "rgeos", "ggplot2", 
                      "raster", "sp", "optparse")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)





# read in polygon shape files
#**********

neighborhoods <- readOGR(dsn=path.expand("/Users/danielspakowicz/Box Sync/projects/scfbw/shape_files/Weaver/nh_neighborhoods/"),
                         layer="nh_neighborhoods")

blockgroups <- readOGR(dsn=path.expand("/Users/danielspakowicz/Box Sync/projects/scfbw/shape_files/blockgroupct_37800_0000_2010_s100_census_1_shp/wgs84/"),
                       layer="blockgroupct_37800_0000_2010_s100_census_1_shp_wgs84")


plot(neighborhoods)

plot(blockgroups) 

x <- 0.03
y <- 0.1

plot(blockgroups, 
     xlim=c(-72.92816-x, -72.92816+x),
     ylim = c(41.30815-y, 41.30815+x))

plot(blockgroups, 
     xlim=c(-72.85, -72.95),
     ylim = c(41.10, 41.5))


Neighborhoods <- spTransform(neighborhoods, CRS("+proj=longlat +datum=WGS84"))

Neighborhoods <- fortify(Neighborhoods)

map<-get_map(location='new haven, ct', zoom=12, maptype = "terrain",
             source='google',color='color')
ggmap(map)


NewHavenNeighborhoods <- ggmap(map) + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='red', data=Neighborhoods, alpha=0)
NewHavenNeighborhoods


bg <- spTransform(blockgroups, CRS("+proj=longlat +datum=WGS84"))

BG <- fortify(bg)

map<-get_map(location='new haven, ct', zoom=12, maptype = "terrain",
             source='google',color='color')
ggmap(map)


NewHavenBG <- ggmap(map) + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='red', data=BG, alpha=0)
NewHavenBG

