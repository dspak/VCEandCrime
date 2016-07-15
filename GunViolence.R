#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  GunViolence_byNeighborhood.R 
# Version : 1.0
#
# Purpose : Read in all scf and crime data and output aggregation subdivided by gun violence
#  
# Version Notes : 
#
# Created.date  : 7 Jul 2016
# Created.by    : Dan Spakowicz
# Updated.date  :
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load/install required packages
list.of.packages <- c("maptools", "ggmap", "rgdal", "spatialEco", 
                      "RColorBrewer", "plyr", "rgeos", "ggplot2", 
                      "raster", "sp", "nlme", "xtable", "plyr",
                      "reshape2", "igraph")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate crime reports by neighborhood, year and gun violence

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime_df <- read.csv(file = "data/processed/crime_data/nh_crime_neigh_bg_gun.csv")
#crime_df$rpt_date <- as.POSIXct(crime_df$rpt_date, format="%YYYY)
crime_df$gunviolence <- as.factor(crime_df$gunviolence)
crime_df$year <- as.factor(crime_df$year)

# year and month cols were added to the crime data csv
# crime_df$year <- gsub(pattern = "^(\\d{4}).*$", "\\1", crime_df$rpt_date)

crime_ag_gun <- aggregate(crime_df, by=list(crime_df$name, crime_df$year, crime_df$gunviolence), FUN=length)
colnames(crime_ag_gun)[1:4] <- c("Neighborhood", "Year", "Gun.Violence", "Num.Crimes")
crime_ag_gun <- crime_ag_gun[,1:4]



# crime_ag_neigh <- aggregate(crime_df, by=list(crime_df$name), FUN=length)
# crime_ag_neigh <- crime_ag_neigh[,1:2]
# 
# crime_ag_month <- aggregate(crime_df, by=list(crime_df$name, crime_df$YearMonth), FUN=length)
# colnames(crime_ag_month)[1:3] <- c("Neighborhood", "YearMonth", "Num.Crimes")
# 
# # sanity check 
# nrow(crime_df) == sum(crime_ag$Num.Crimes)

# exclude 2013 because it lacks Oct-Dec
crime_ag.12 <- crime_ag_gun[which(crime_ag_gun$Year != 2013),]

crime_ag.12_gun <- crime_ag.12[which(crime_ag.12$Gun.Violence==1),]

ggplot(crime_ag.12[which(crime_ag.12$Gun.Violence==1),], aes(x = Year, y = Num.Crimes, label = Neighborhood))+
  geom_line(aes(group= Neighborhood, color = Neighborhood))+
  labs(x = "Year",
       y = "Number of Recorded Crimes",
       title = "Gun Violence Crimes in each Neighborhood by Year")+
  geom_text(data = crime_ag.12[crime_ag.12$Year == "2010",
                               ], aes(label = Neighborhood), check_overlap = T)+
  geom_label(data = crime_ag.12[crime_ag.12$Year == "2010",
                                ], aes(label = Neighborhood))

  ggsave("crime_neigh_year_line.pdf", path = figout, width = 12, height = 8)

  ggplot(crime_ag.12[which(crime_ag.12$Gun.Violence==0),], aes(x = Year, y = Num.Crimes, label = Neighborhood))+
    geom_line(aes(group= Neighborhood, color = Neighborhood))+
    labs(x = "Year",
         y = "Number of Recorded Crimes",
         title = "Not Gun Violence Crimes in each Neighborhood by Year")+
    geom_text(data = crime_ag.12[crime_ag.12$Year == "2010",
                               ], aes(label = Neighborhood), check_overlap = T)+
    geom_label(data = crime_ag.12[crime_ag.12$Year == "2010",
                                  ], aes(label = Neighborhood))+
    ggsave("crime_neigh_year_line_nogun.pdf", path = figout, width = 7, height = 7)
  
  
  ggplot(crime_ag.12_gun, aes(x = Year, y = Num.Crimes, label = Neighborhood))+
    geom_line(aes(group= Neighborhood, color = Neighborhood))+
    labs(x = "Year",
         y = "Number of Recorded Crimes",
         title = "Gun Violence Crimes in each Neighborhood by Year")+
    geom_text(data = crime_ag.12_gun[crime_ag.12_gun$Year == "2010",
                                 ], aes(label = Neighborhood), check_overlap = T)+
    geom_label(data = crime_ag.12_gun[crime_ag.12_gun$Year == "2010",
                                  ], aes(label = Neighborhood))+
    ggsave("crime_neigh_year_line_gun.pdf", path = figout, width = 7, height = 7)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Aggregate crime reports by neighborhood, year and any weapon
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
