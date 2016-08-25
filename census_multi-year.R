## Read in and output census data files

# get a list of files
listoffiles <- list.files(path = "~/Downloads", pattern = "ACS_5yr*")

# read into a 
census <- sapply(X = paste("~/Downloads/", listoffiles, sep = ""),FUN = read.csv)

census.r <- lapply(census, function(x) { x[-1,]})

census.r2 <- lapply(census, function(x) { x[,!is.na(x)]})

is.na(census[[1]][,2])
