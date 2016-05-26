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

df <- read.csv("~/Downloads/IncidentsNH2000_2013.csv")


names(df)

par(mar=c(14,5,5,5))
barplot(table(df$district),las = 2)

df$year <- sub('([0-9]{4}).*', '\\1', df$rpt_date)
table(df$year)
barplot(table(df$year),las = 2)
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



#"ESRI:102256"
# http://spatialreference.org/ref/esri/102256/
# NAD 1983 HARN StatePlane Connecticut FIPS 0600