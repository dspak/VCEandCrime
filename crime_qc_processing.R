#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title :  crime_qc_processing.R
# Version : 1.0
#
# Purpose : Input the SCF data files and process them to be model-ready
#  
# Version Notes : 
#
# Created.date  : 
# Thu May 26 14:22:22 2016 ------------------------------
# Created.by    : Dan Spakowicz
# Updated.date  :
# Tue May 24 16:10:06 2016 ------------------------------
# Updated.by    : DS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load/install required packages
list.of.packages <- c("ggplot2")
                      
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# set project base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"

library(ggplot2)

# original datafile
#df <- read.csv("raw_data/crime_data/IncidentsNH2000_2013.csv")
# with neighborhood and blockgroup coding
df <- read.csv("raw_data/crime_data/nh_crime_neigh_bg.csv")
# CJPs manual gun coding
guns <- read.csv("~/Downloads/cp.guns.csv")

# check which guns cols are unique
apply(guns, 2, function(x) length(unique(x)))

# merge with gun coding
guns$id_key <- NULL
df.g <- merge(df, guns, by.x = "ObjectID", by.y = "objectid", all = T)


# add in a year variable
df.g$year <- sub('([0-9]{4}).*', '\\1', df$rpt_date)

# sanity check
table(df.g$year)

# also include a column of the Year + Month (2013 only goes through Aug, so if we use per month we can incorporate the part of 2013 that's available)
df.g$YearMonth <- sub('([0-9]{6}).*', '\\1', df$rpt_date)

#sanity check
t.ym <- data.frame(table(df.g$YearMonth))



# write output to csv for general use
write.csv(x = df.g, file = "raw_data/crime_data/nh_crime_neigh_bg_gun.csv")




ggplot(t.ym, aes(x = 1:nrow(t.ym), y = Freq))+
  geom_point()+
  geom_line()+
  stat_smooth()+
  labs(x = "Month",
       y = "Number of Crimes",
       title = "Number of Crimes in each Month\nNew Haven, Jan 2000 - Aug 2013")+
  #scale_x_continuous(labels=t.ym$Var1, breaks=seq(1,nrow(t.ym), by =1), angle=90)
  theme(axis.text.x = element_blank())+
  ggsave("num_crimes_perMonth.pdf", path = figout, width = 6, height = 8)

barplot(table(df$YearMonth),las = 2)
tab.year <- data.frame(table(df$year))

ggplot(tab.year, aes(x = Var1, y = Freq))+
  geom_bar(stat = "identity", aes(fill=Var1))

desc <- data.frame(table(df$class_desc))

ggplot(desc, aes(x = reorder(Var1, Freq), y = Freq))+
  geom_bar(stat = "identity", aes(fill = Var1))

desc <- desc[order(desc$Freq, decreasing = T),]
barplot(desc$Freq[1:15], las = 2)

table(factor(df$class))
length(table(factor(df$class)))
length(table(df$class_desc))

# number of crimes by police district
district <- data.frame(table(df$district))

ggplot(district, aes(x = Var1, y = Freq))+
  geom_bar(stat = "identity", aes(color = Var1))+
  coord_flip()+
  labs(x = "Number of Crimes",
       y = "Police District",
       title = "Number of Crimes in each Police District")+
  theme(legend.position="none")+
  ggsave("num_crimes_by_police_district.pdf", path = figout, width = 6, height = 8)
