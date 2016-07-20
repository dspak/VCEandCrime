# Reformat aggregation file
# Fri Jul 15 14:26:30 2016 ------------------------------

# load/install required packages
list.of.packages <- c()

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"


# read in data
df <- read.csv(file = "data/processed/combined/crime_scf_census_by_mneigh_ym.csv")

# remove rows without crime data
has.crime <- complete.cases(df$Num.Crimes)
df.crime <- df[has.crime,]

# set NA to 0 in Num.SeeClickFix col
df.crime$Num.SeeClickFix.Issues[is.na(df.crime$Num.SeeClickFix.Issues)] <- 0

# output this datafile for model building
write.csv(df.crime, file = "data/processed/combined/crime_scf_census_by_mneigh_ym_ITS.csv")

