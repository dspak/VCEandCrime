#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  SummaryAndAggregation.R 
# Version : 1.0
#
# Purpose : Input the SCF data files and process them to be model-ready
#  
# Version Notes : 
#
# Created.date  : 27 Apr 2016
# Created.by    : Dan Spakowicz
# Updated.date  :
# Tue May 24 16:10:06 2016 ------------------------------
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load/install required packages
list.of.packages <- c("maptools", "ggmap", "rgdal", "spatialEco", 
                      "RColorBrewer", "plyr", "rgeos", "ggplot2", 
                      "raster", "sp", "nlme", "xtable", "plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map of SCF posts by year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load issues file
scf_df <- read.csv(file = "raw_data/scf_data/scf_issues_neighborhoods_bg.csv")
scf_df$created_at <- as.POSIXct(scf_df$created_at)
scf_df$acknowledg <- as.POSIXct(scf_df$acknowledg)
scf_df$closed_at <- as.POSIXct(scf_df$closed_at)
scf_df$reopened_a <- as.POSIXct(scf_df$reopened_a)
scf_df$year <- strftime(scf_df$created_at, format = "%Y")

# Figure of posts by year  
ggplot(data = scf_df)+
  geom_point(aes(x = lng, y = lat, color = year), size=0.3)+
  #   geom_line(data=area.points, 
  #                aes(x=long, y = lat, fill = NULL))+
  ylim(41.25, 41.35)+
  xlim(-73, -72.85)+ 
  facet_wrap(~year)+
  labs(x = "Longitude",
       y = "Latitude", 
       title = "SeeClickFix Issue Locations by Year")+
  ggsave("scf_posts_loc_by_year.pdf", path = figout, width = 12, height = 8)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate scf posts by neighborhood and year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

scf_ag <- aggregate(scf_df, by=list(scf_df$name, scf_df$year), FUN=length)
colnames(scf_ag)[1:3] <- c("Neighborhood", "Year", "Num.Posts")

# sanity check
nrow(scf_df) == sum(scf_ag$Num.Posts)
nrow(scf_df) == sum(scf_ag$id)
sum(scf_ag$Num.Posts)
sum(scf_ag$id)
table(scf_df$id)

ggplot(scf_ag, aes(x = Year, y = Num.Posts))+
  geom_line(aes(group=Neighborhood, color = Neighborhood))+
  labs(x = "Year",
       y = "Number of Posts",
       title = "SeeClickFix Posts in each Neighborhood by Year")+
  geom_text(data = scf_ag[scf_ag$Year == "2015",
                               ], aes(label = Neighborhood), check_overlap = T)+
  geom_label(data = scf_ag[scf_ag$Year == "2015",
                                ], aes(label = Neighborhood))+
  ggsave("scf_neigh_year_line.pdf", path = figout, width = 12, height = 8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate crime reports by neighborhood and year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

crime_df <- read.csv(file = "raw_data/crime_data/nh_crime_neigh_bg.csv")
#crime_df$rpt_date <- as.POSIXct(crime_df$rpt_date, format="%YYYY)
crime_df$year <- gsub(pattern = "^(\\d{4}).*$", "\\1", crime_df$rpt_date)

crime_ag <- aggregate(crime_df, by=list(crime_df$name, crime_df$year), FUN=length)
colnames(crime_ag)[1:3] <- c("Neighborhood", "Year", "Num.Crimes")

# sanity check 
nrow(crime_df) == sum(crime_ag$Num.Crimes)

# exclude 2013 because it lacks Oct-Dec
crime_ag.12 <- crime_ag[which(crime_ag$Year != 2013),]

ggplot(crime_ag.12, aes(x = Year, y = Num.Crimes, label = Neighborhood))+
  geom_line(aes(group=Neighborhood, color = Neighborhood))+
  labs(x = "Year",
       y = "Number of Recorded Crimes",
       title = "Crimes in each Neighborhood by Year")+
  geom_text(data = crime_ag.12[crime_ag.12$Year == "2010",
                       ], aes(label = Neighborhood), check_overlap = T)+
  geom_label(data = crime_ag.12[crime_ag.12$Year == "2010",
                                ], aes(label = Neighborhood))+
  ggsave("crime_neigh_year_line.pdf", path = figout, width = 12, height = 8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Join crime and scf aggregations for a preliminary Linear Model

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

both <- merge(x = crime_ag.12[,1:3], y = scf_ag[,1:3],
              by = c("Neighborhood", "Year"), all = T)


lm0 <- lm(Num.Crimes ~ Num.Posts,
          data = both)
summary(lm0)

lm1 <- lm(Num.Crimes ~ Neighborhood + Num.Posts,
          data = both)
xtable(summary(lm1))
anova(lm1)

anova(lm0, lm1)

par(mfrow=c(2,2))
plot(lm1)

lm2 <- lm(Num.Crimes ~ Neighborhood + Num.Posts +
            Num.Posts*Year,
          data = both)
summary(lm2)

anova(lm1, lm2)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SCF time to resolution by Neighborhood

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

scf_df$timeToAckn <- scf_df$acknowledg - scf_df$created_at

scf_df$timeToClose <- scf_df$closed_at - scf_df$created_at

closed <- aggregate(scf_df, by = list("name", "closed_at"), FUN = length)

table(is.na(scf_df$closed_at) ~ scf_df$name)
issuesOfInterest <- ddply(scf_df, ~name, summarize, scf_df$title)
colnames(issuesOfInterest) <- c("Neighborhood", "Issue.Title")

ggplot(issuesOfInterest, aes(x = Neighborhood, y = Issue.Title))+
  geom_bar(stat = "identity")

# what fraction of issues are closed
table(is.na(scf_df$closed_at) == "TRUE")

table(scf_df$title)
sidewalks <- scf_df[which(scf_df$title == "Sidewalks and Curb damage"),]

scf_df$timeToAckn
ggplot(data = scf_df, aes(x = name, y = as.numeric(timeToAckn)))+
  geom_boxplot(aes(group=name))

table(is.na(scf_df$closed_at))
