#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  ShapeToCSV.R 
# Version : 1.0
#
# Purpose : A simple script to convert a shapefile into a csv. Reduces some file size issues with large
#           shapefiles
#  
# Version Notes : fixed bug that removed one col from the analysis
#
# Created.date  : 24 May 2016
# Created.by    : Dan Spakowicz
# Updated.date  : 05 May 2016 
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Usage:      Rscript ShapeToCSV.R <base file name>
# Example:    Rscript ShapeToCSV.R nh_crime


args <- commandArgs()

# load/install required packages
list.of.packages <- c("rgdal")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# function for reading in the shapefile and writing to csv
ShapeToCSV <- function(baseName) {
	# read in SpatialPointsDataFrame object
	shape <- readOGR(dsn=path.expand(getwd()), layer=quote(baseName))
	
	# convert to standard dataframe
	df = as.data.frame(shape)
	
	# write to csv
	write.csv(df, paste(baseName,".csv", sep=""))
}

# call function
ShapeToCSV(args[1])