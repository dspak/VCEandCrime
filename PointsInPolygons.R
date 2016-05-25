#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  PointsInPolygons.R 
# Version : 1.0
#
# Purpose : Takes and input csv containing lat long returns a csv with the points assigned to polygons (from shapefiles with hardcoded paths). Polygons are in WGS1984
#
# Created.date  : 24 May 2016
# Created.by    : Dan Spakowicz
# Updated.date  : 
# Updated.by    : 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Usage:      Rscript PointsInPolygons.R -i <points.csv> -o <output.csv>

# Future Args should include the shapefiles and coordinate systems


# load/install required packages
list.of.packages <- c("maptools", "ggmap", "rgdal", "spatialEco", 
                      "RColorBrewer", "plyr", "rgeos", "ggplot2", 
                      "raster", "sp", "optparse")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)

# set arguments
option_list = list(
  make_option(c("-i", "--input"), type="character", default=NULL, 
              help="", metavar="character"),
  make_option(c("-o", "--output"), type="character", 
              default="output.csv", 
              help="output file name [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


# function for reading in the shapefile and writing to csv
PointsInPolygons <- function(infile, outfile) {
  # read in csv file
  df <- read.csv(infile)
  
  # remove rows without spatial data
  df <- df[!is.na(df$point_x),]
  
  # set spatial data as a new var for conversion to SpatialDataFrame object
  xy <- df[,33:34]
  
  # convert to SpatialDataFrame
  sp.df <- SpatialPointsDataFrame(coords = xy, data = df[,1:32], 
                                  proj4string = CRS("+proj=lcc +lat_1=41.86666666666667 +lat_2=41.2 +lat_0=40.83333333333334 +lon_0=-72.75 +x_0=304800.6096012192 +y_0=152400.3048006096 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
  
  # change coordinate system to match polygon shapefiles
  sp.df2 <- spTransform(sp.df, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # read in polygon shape files
  neighborhoods <- readOGR(dsn=path.expand("/Users/danielspakowicz/Box Sync/projects/scfbw/shape_files/Weaver/nh_neighborhoods/"),
                           layer="nh_neighborhoods")
  
  blockgroups <- readOGR(dsn=path.expand("/Users/danielspakowicz/Box Sync/projects/scfbw/shape_files/blockgroupct_37800_0000_2010_s100_census_1_shp/wgs84/"),
                         layer="blockgroupct_37800_0000_2010_s100_census_1_shp_wgs84")
 
  # assign points to polygons 
  pnp1 <- point.in.poly(pts = sp.df2, polys = neighborhoods)
  pnp2 <- point.in.poly(pts = pnp1, polys = blockgroups)
  
  # convert to normal dataframe
  out <- as.data.frame(pnp2)
  
  # write out csv containing polygons
  write.csv(x = out, outfile)
  
}

# call function
PointsInPolygons(opt$input, opt$output)