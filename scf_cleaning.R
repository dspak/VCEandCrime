#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  scf_cleaning.R 
# Version : 1.0
#
# Purpose : Input the SCF data files and process them to be model-ready
#  
# Version Notes : 
#
# Created.date  : 27 Apr 2016
# Created.by    : Dan Spakowicz
# Updated.date  : 05 May 2016 
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load required packages
#install.packages("maptools")
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("spatialEco")
library(maptools)
library(RColorBrewer)
library(ggmap)
library(ggplot2)
library(rgdal)
library(plyr)
library(rgeos)
library(spatialEco)

# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"

# load issues file
scf_issues <- "raw_data/scf_data/nh_scf_issues.csv"
df <- read.csv(scf_issues)

# make a column of just year
df$created_at <- as.POSIXct(df$created_at)
df$year <- strftime(df$created_at, format = "%Y")

# Figure of posts by year  
ggplot(data = df)+
  geom_point(aes(x = lng, y = lat, color = year), size=0.3)+
  #   geom_line(data=area.points, 
  #                aes(x=long, y = lat, fill = NULL))+
  ylim(41.25, 41.35)+
  xlim(-73, -72.85)+ 
  facet_wrap(~year)
ggsave("scf_posts_by_year.pdf", path = figout)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apply shape files to scf data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# view shape file
shape.dir <- "shape_files/Weaver/nh_neighborhoods/"
shape.file <- "nh_neighborhoods.shp"
area <- readShapePoly(paste(shape.dir, shape.file, sep = ""))
plot(area)

# load full set of shape files
neighborhoods <- readOGR(dsn=path.expand("shape_files/Weaver/nh_neighborhoods/"),
                 layer="nh_neighborhoods")

# create dataframe from shape file
neighborhoods@data$id = rownames(neighborhoods@data)
neighborhoods.points = fortify(neighborhoods, region="id")
neighborhoods.df = join(neighborhoods.points, neighborhoods@data, by="id")

# convert df into SpatialPoints DataFrame
coordinates(df) = c("lng", "lat")

# this step fails, check http://r-sig-geo.2731867.n2.nabble.com/point-in-polygon-or-over-help-td7583635.html
scf.in.neighborhoods <- point.in.poly(df, neighborhoods)




# convert shape file into dataframe
area.points <- fortify(area)

# reduce to our area of interest
area.point.nh <- area.points[which(area.points$long >= (-72.95) & area.points$long <= (-72.85)),]
area.point.nh <- area.points[which(area.point.nh$lat >= (41.25) & area.point.nh$lat <= (41.35)),]


# get google map of the area
mapImage <- get_map(location = c(lon = -72.9, lat = 41.3),
                    color = "color",
                    source = "osm",
                    # maptype = "terrain",
                    zoom = 10)

map2 <- get_googlemap("newhaven")

# map
colors <- brewer.pal(9, "BuGn")

ggmap(map2)

ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

ggmap(mapImage) +
  geom_polygon(aes(x = lng,
                   y = lat,
                  # group = group),
               data = df,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")



# Figure of posts by year  
ggplot(data = df)+
  geom_point(aes(x = lng, y = lat, color = year), size=0) +
  geom_polygon(data=area.points, 
               aes(x=long, y = lat),
               color = "red")+
  ylim(41.25, 41.35)+
  xlim(-73, -72.85)
  
# create a column for number of votes per issue


# create a column for number of watchers per issue


# Trying with another set of packages
# http://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile


data(meuse)
xy = meuse[c("x", "y")] # retrieve coordinates as data.frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c("x", "y") # specify column names
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c(1, 2) # specify column names
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = ~x+y # formula
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = xy   # as data frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = as.matrix(xy)   # as matrix
meuse$log.zn = log(meuse$zinc)
class(meuse)
dim(meuse)

