#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  MergeCensusData.R 
# Version : 1.0
#
# Purpose : Join the census data files onto the crime data
#  
# Version Notes : 
#
# Created.date  : 7 Jul 2016
# Created.by    : Dan Spakowicz
# Updated.date  :
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load/install required packages
list.of.packages <- c()

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in and format crime data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime_df <- read.csv(file = "data/processed/crime_data/nh_crime_neigh_bg_gun.csv")
#crime_df$rpt_date <- as.POSIXct(crime_df$rpt_date, format="%YYYY)
crime_df$gunviolence <- as.factor(crime_df$gunviolence)
crime_df$year <- as.factor(crime_df$year)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in and format census data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

census <- read.csv(file = "data/raw/census/R11207373_SL140_ManualNeighborhoods.csv")
census <- census[-1,]
census[,57:ncol(census)] <- apply(X = census[,57:ncol(census)], 2, as.character)
census[,57:ncol(census)] <- apply(X = census[,57:ncol(census)], 2, as.numeric)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in and format scf data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

scf <- read.csv(file = "data/processed/scf_data/scf_issues_neighborhoods_bg.csv")
scf$created_at <- as.POSIXct(scf$created_at)
scf$acknowledg <- as.POSIXct(scf$acknowledg)
scf$closed_at <- as.POSIXct(scf$closed_at)
scf$reopened_a <- as.POSIXct(scf$reopened_a)
scf$year <- strftime(scf$created_at, format = "%Y")
scf$YearMonth <- strftime(scf$created_at, format = "%Y%m")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge crime and census to apply the manual neighborhood labels to the crime data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

merged <- merge(crime_df, census, by.x = "TRACTCE10", by.y = "Census.Tract")

# sanity check -- are the tracts uniquely assigned to neighborhoods?
neighborhood_tracts <- data.frame(merged$TRACTCE10, merged$name, merged$Neighborhood)
unique <- unique(neighborhood_tracts)

# this file is ~900MB, so just load it at the start of each analysis rather than keeping it as an exported version.
#write.csv(merged, file = "data/processed/crime_data/nh_crime_neigh_bg_gun_manNeigh.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge scf and census to apply the manual neighborhood labels to the scf dat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

merged.scf <- merge(scf, census, by.x = "TRACTCE10", by.y = "Census.Tract")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate crime data by new neighborhood categories (manually encoded to follow census tracts)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
crime.ag.newneigh <- aggregate(merged[,1:58], by = list(merged$Manual.Neighborhood, merged$YearMonth, merged$gunviolence, merged$anyweapon), FUN=length)
crime.ag.newneigh <- crime.ag.newneigh[,1:5]
colnames(crime.ag.newneigh) <- c("mNeighborhood", "YearMonth", "Gun", "Any.Weapon", "Num.Crimes")

#write.csv(crime.ag.newneigh, "data/processed/crime_data/crime_aggr_by_neigh_ym_gun_aw.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate crime data by new neighborhood categories (manually encoded to follow census tracts)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime.ag.newneigh.ngw <- aggregate(merged[,1:58], by = list(merged$Manual.Neighborhood, merged$YearMonth), FUN=length)
crime.ag.newneigh.ngw <- crime.ag.newneigh.ngw[,1:3]
colnames(crime.ag.newneigh.ngw) <- c("mNeighborhood", "YearMonth", "Num.Crimes")

#write.csv(crime.ag.newneigh, "data/processed/crime_data/crime_aggr_by_neigh_ym.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate scf data by new neighborhood categories (manually encoded to follow census tracts)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(scf)
scf.ag.newneigh <- aggregate(merged.scf[,1:58], by = list(merged.scf$Manual.Neighborhood, merged.scf$YearMonth), FUN=length)
scf.ag.newneigh <- scf.ag.newneigh[,1:3]
colnames(scf.ag.newneigh) <- c("mNeighborhood", "YearMonth", "Num.SeeClickFix.Issues")

#write.csv(scf.ag.newneigh, "data/processed/scf_data/scf_aggr_by_mneigh_ym.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate the census data by neighborhood (sum cols by neighborhood)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

census_byNeighborhoods <- aggregate(census[,57:ncol(census)], by = list(Neighborhood=census$Manual.Neighborhood), FUN=sum)

#dir.create("data/processed/census/")
#write.csv(x = census_byNeighborhoods, file = "data/processed/census/census_aggr_byManualNeighborhood.csv", row.names = F)

census_ag <- read.csv("data/processed/census/census_aggr_byManualNeighborhood.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge the aggregated crime data and census data by the new, manual neighborhood labels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime.census <- merge(crime.ag.newneigh, census_ag, by.x = "mNeighborhood", by.y = "Neighborhood")

#write.csv(crime.census, file = "data/processed/combined/crime_census_by_mneigh_ym_gun_aw.csv")

crime.census.ngw <- merge(crime.ag.newneigh.ngw, census_ag, by.x = "mNeighborhood", by.y = "Neighborhood")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge the aggregated scf data and census data by the new, manual neighborhood labels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

scf.census <- merge(scf.ag.newneigh, census_ag, by.x = "mNeighborhood", by.y = "Neighborhood")

#write.csv(scf.census, file = "data/processed/combined/scf_census_by_mneigh_ym.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge the aggregated crime, scf and census data by YearMonth
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

neigh.ym.scf <- scf.census[1:3]

crime.scf.census <- merge(crime.census, neigh.ym.scf, by = c("mNeighborhood", "YearMonth"), all = T)
names(crime.scf.census)

#write.csv(crime.scf.census, file = "data/processed/combined/crime_scf_census_by_mneigh_ym_gun_aw.csv")


crime.scf.census.ngw <- merge(crime.census.ngw, neigh.ym.scf, by = c("mNeighborhood", "YearMonth"), all = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate crime and scf post rates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime.scf.census.ngw$Crime.Rate <- (crime.scf.census.ngw$Num.Crimes / crime.scf.census.ngw$Total.Population)*1E5
crime.scf.census.ngw$SeeClickFix.Issue.Rate <- (crime.scf.census.ngw$Num.SeeClickFix.Issues / crime.scf.census.ngw$Total.Population)*1E5

write.csv(crime.scf.census.ngw, file = "data/processed/combined/crime_scf_census_by_mneigh_ym.csv")
