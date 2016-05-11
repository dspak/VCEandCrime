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
library(maptools)
library(RColorBrewer)
library(ggmap)
library(ggplot2)

# input dir

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"

# load issues file
scf_issues <- "~/Box Sync/projects/scfbw/scf_data/nh_scf_issues.csv"
df <- read.csv(scf_issues)
df$created_at <- as.POSIXct(df$created_at)
df$year <- strftime(df$created_at, format = "%Y")




# check for extraneous values




#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apply shape files to scf data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load in shape file
shape.file <- "~/Box Sync/projects/scfbw/shape_files/blockgroupct_37800_0000_2010_s100_census_1_shp/wgs84/blockgroupct_37800_0000_2010_s100_census_1_shp_wgs84.shp"
area <- readShapePoly(shape.file)
plot(area)

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
  geom_point(aes(x = lng, y = lat, color = year), size=0.3)+
#   geom_line(data=area.points, 
#                aes(x=long, y = lat, fill = NULL))+
  ylim(41.25, 41.35)+
  xlim(-73, -72.85)+ 
  facet_wrap(~year)+
  ggsave("scf_posts_by_year.pdf", path = figout)

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
