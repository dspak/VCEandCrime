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
# Updated.date  :
# Mon May 16 13:25:25 2016 ------------------------------
# Mon May 23 23:09:42 2016 ------------------------------
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load/install required packages
list.of.packages <- c("maptools", "ggmap", "rgdal", "spatialEco", 
                      "RColorBrewer", "plyr", "rgeos", "ggplot2", 
                      "raster")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


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

# of new haven map
colors <- brewer.pal(9, "BuGn")

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



  
# create a column for number of votes per issue


# create a column for number of watchers per issue


# Trying with another set of packages
# http://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check merged crime + neighborhoods + block group dataframe

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load full set of shape files
crime_n_bg <- readOGR(dsn = "/Users/danielspakowicz/Desktop/shape_files",
                      layer = "crime_neigh_bg")

scf_n_bg <- readOGR(dsn = "/Users/danielspakowicz/Desktop/shape_files",
                      layer = "scf_issues_neighborhood_bg")


# create dataframe from shape file
crime_df <- as.data.frame(crime_n_bg)
scf_df <- as.data.frame(scf_n_bg)

# write to file
outloc <- "~/Desktop/shape_files/"
write.csv(x = crime_df, file = paste(outloc, "nhcrime_neighborhoods_bg.csv", sep = ""))
write.csv(x = scf_n_bg, file = paste(outloc, "scf_issues_neighborhoods_bg.csv", sep = ""))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate scf posts by neighborhood and year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

scf_df <- read.csv(file = "raw_data/scf_data/scf_issues_neighborhoods_bg.csv")
scf_df$created_at <- as.POSIXct(scf_df$created_at)
scf_df$year <- strftime(scf_df$created_at, format = "%Y")

scf_ag <- aggregate(scf_df, by=list(scf_df$name, scf_df$year), FUN=length)
colnames(scf_ag)[1:3] <- c("Neighborhood", "Year", "Num.Posts")

ggplot(scf_ag, aes(x = Year, y = Num.Posts))+
  geom_line(aes(group=Neighborhood, color = Neighborhood))+
  labs(x = "Year",
       y = "Number of Posts",
       title = "SeeClickFix Posts in each Neighborhood by Year")+
  ggsave("scf_neigh_year_line.pdf", path = figout)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate crime reports by neighborhood and year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime_df <- read.csv(file = "raw_data/crime_data/nhcrime_neighborhoods_bg.csv")
#crime_df$rpt_date <- as.POSIXct(crime_df$rpt_date, format="%YYYY)
crime_df$year <- gsub(pattern = "^(\\d{4}).*$", "\\1", crime_df$rpt_date)

crime_ag <- aggregate(crime_df, by=list(crime_df$name, crime_df$year), FUN=length)
colnames(crime_ag)[1:3] <- c("Neighborhood", "Year", "Num.Crimes")

ggplot(crime_ag, aes(x = Year, y = Num.Crimes))+
  geom_line(aes(group=Neighborhood, color = Neighborhood))+
  labs(x = "Year",
       y = "Number of Recorded Crimes",
       title = "Crimes in each Neighborhood by Year")+
  ggsave("crime_neigh_year_line.pdf", path = figout)
