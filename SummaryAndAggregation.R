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
                      "raster", "sp", "nlme", "xtable", "plyr",
                      "reshape2", "igraph")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"



# load and format issues file
scf_df <- read.csv(file = "raw_data/scf_data/scf_issues_neighborhoods_bg.csv")
scf_df$created_at <- as.POSIXct(scf_df$created_at)
scf_df$acknowledg <- as.POSIXct(scf_df$acknowledg)
scf_df$closed_at <- as.POSIXct(scf_df$closed_at)
scf_df$reopened_a <- as.POSIXct(scf_df$reopened_a)
scf_df$year <- strftime(scf_df$created_at, format = "%Y")
scf_df$year_month <- strftime(scf_df$created_at, format = "%Y%m")

# load merged issues / comments file
comments <- read.csv("raw_data/scf_data/nh_scf_issues_comments.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map of SCF posts by year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# Unique users per neighborhood

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

users <- aggregate(x = scf_df, by = list(user_id, name), FUN = length)
users <- users[,1:3]

# sanity check
colSums(users[3])

# stacked barplot of 
ggplot(users, aes(x = Group.2, y = X, fill = factor(Group.1)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(legend.position="none")+
  labs(x = "",
       y = "Number of Posts",
       title = "Number of Posts by unique users by neighborhood")
  ggsave("posts_per_user_by_neighborhood.pdf", path = figout, width = 12, height = 8)
  

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate scf posts by neighborhood and year or month

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

scf_ag <- aggregate(scf_df, by=list(scf_df$name, scf_df$year), FUN=length)
colnames(scf_ag)[1:3] <- c("Neighborhood", "Year", "Num.Posts")

scf_ag_month <- aggregate(scf_df, by=list(scf_df$name, scf_df$year_month), FUN=length)
colnames(scf_ag_month)[1:3] <- c("Neighborhood", "YearMonth", "Num.Posts")

# sanity check
nrow(scf_df) == sum(scf_ag$Num.Posts)
nrow(scf_df) == sum(scf_ag$id)


# Plot of the # of posts in each neighborhood by year
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

crime_df <- read.csv(file = "raw_data/crime_data/nh_crime_neigh_bg_gun.csv")
#crime_df$rpt_date <- as.POSIXct(crime_df$rpt_date, format="%YYYY)

# year and month cols were added to the crime data csv
# crime_df$year <- gsub(pattern = "^(\\d{4}).*$", "\\1", crime_df$rpt_date)

crime_ag <- aggregate(crime_df, by=list(crime_df$name, crime_df$year), FUN=length)
colnames(crime_ag)[1:3] <- c("Neighborhood", "Year", "Num.Crimes")
crime_ag_neigh <- aggregate(crime_df, by=list(crime_df$name), FUN=length)
crime_ag_neigh <- crime_ag_neigh[,1:2]

crime_ag_month <- aggregate(crime_df, by=list(crime_df$name, crime_df$YearMonth), FUN=length)
colnames(crime_ag_month)[1:3] <- c("Neighborhood", "YearMonth", "Num.Crimes")

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

both <- merge(x = crime_ag_month[,1:3], y = scf_ag_month[,1:3],
              by = c("YearMonth", "Neighborhood"), all = T)
both <- both[order(both$YearMonth),]


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

lm3 <- lm(Num.Crimes ~ Neighborhood + Num.Posts + YearMonth,
          data = both)
summary(lm3)

lm4 <- lm(Num.Crimes ~  Num.Posts,
          data = both)
summary(lm4)
par(mfrow=c(2,2))
pdf(file = "figures/lm_residuals_crime_v_posts.pdf")
plot(lm4)
dev.off()

ggplot(data = both, aes(x = Num.Crimes, y = Num.Posts))+
  geom_point()+
  stat_smooth(method = lm)

ggplot(data = both, aes(x = Num.Crimes, y = Num.Posts))+
  geom_point(aes(color=Neighborhood))+
  stat_smooth(method = lm)+
  labs(x="Number of Crimes",
       y="Number of SeeClickFix Issues",
       title = "Per Month SeeClickFix Issues and Crime")+
  ggsave(filename = "scf_v_crime_permonth_scatter_lm.pdf",
         path = figout, width = 12, height = 8)

ggplot(data = both, aes(x = Num.Crimes, y = Num.Posts, color=Neighborhood))+
  geom_point()+
  stat_smooth(method = lm)+
  labs(x="Number of Crimes",
       y="Number of SeeClickFix Issues",
       title = "Per Month SeeClickFix Issues and Crime")+
  ggsave(filename = "scf_v_crime_permonth_scatter_lmByNeighborhood.pdf",
         path = figout, width = 12, height = 8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SCF issue categories by Neighborhood

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

issuesOfInterest <- count(scf_df, c("name", "title"))
issuesOfInterest <- issuesOfInterest[!is.na(issuesOfInterest$name),]

# stacked barplot of the issue types per neighborhood
ggplot(issuesOfInterest, aes(x = name, y = freq, fill=title))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "",
       y = "Number",
       fill = "SeeClickFix Issue Category")+
  ggsave("issue_titles_by_neighborhood.pdf", path = figout, width = 12, height = 8)

# now normalizing all to percent of total
cast.ioi <- dcast(issuesOfInterest, formula = name~title)

cast.ioi.rS <- rowSums(cast.ioi[2:25], na.rm = T)
perc.cast.ioi <- cast.ioi / cast.ioi.rS
perc.cast.ioi$name <- cast.ioi$name

mperc.cast.ioi <- melt(perc.cast.ioi)

# normalized stacked barplot of the issue types per neighborhood
ggplot(mperc.cast.ioi, aes(x = name, y = value*100, fill=variable))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "",
       y = "Percent of Issues",
       fill = "SeeClickFix Issue Category",
       title = "Categories of SeeClickFix Posts by Neighborhood")+
  ggsave("issue_titles_by_neighborhood_normalized.pdf", path = figout, width = 12, height = 8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SCF Percent Acknowledged and Closed by Neighborhood

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# what fraction of issues are acknowledged by neighborhood
timing <- data.frame(scf_df$id, scf_df$user_id, scf_df$name, scf_df$created_at, scf_df$acknowledg, scf_df$closed_at, scf_df$reopened_a, scf_df$title)
names <- names(timing)
names <- gsub("scf_df.", "", names)
colnames(timing) <- names
attach(timing)

# create new data frame with F/T for acknowledg
acknowledged_fraction <- ddply(.data = timing, .variables = c("name"), .fun = summarize,
  ack_table = table(is.na(acknowledg))
)
# add column for casting
acknowledged_fraction$FT <- NA
acknowledged_fraction$FT[seq(1, nrow(acknowledged_fraction), by = 2)] = "Ack"
acknowledged_fraction$FT[seq(2, nrow(acknowledged_fraction), by = 2)] = "NotAck"

# cast back into short form for calculating as a percent
ack.sh <- dcast(acknowledged_fraction, name ~ FT, value.var = "ack_table", na.rm = T)

ack.sh$total = ack.sh$Ack + ack.sh$NotAck
ack.sh$Percent.Acknowledged <- ack.sh$Ack/ack.sh$total * 100

ggplot(ack.sh[1:20,], aes(x = name, y = Percent.Acknowledged))+
  geom_bar(stat = "identity", aes(color = name))+
  coord_flip()+
  geom_hline(data = ack.sh, aes(yintercept = mean(Percent.Acknowledged)), 
             linetype = "dashed")+
  labs(x = "",
       y = "Percent Acknowledged",
       title = "The Percent of Issues that are Acknowledged\nfor each Neighborhood")+
  theme(legend.position = "none")
  ggsave(filename = "percent_acknowledged_byNeighborhood.pdf",
         path = figout, width = 6, height = 8)
  
# now do the same for closures

# create new data frame with F/T for acknowledg
  closed_fraction <- ddply(.data = timing, .variables = c("name"), .fun = summarize,
                                 closed_table = table(is.na(closed_at)))
  # add column for casting
  closed_fraction$FT <- NA
  closed_fraction$FT[seq(1, nrow(closed_fraction), by = 2)] = "Closed"
  closed_fraction$FT[seq(2, nrow(closed_fraction), by = 2)] = "NotClosed"
  
  # cast back into short form for calculating as a percent
  closed.sh <- dcast(closed_fraction, name ~ FT, value.var = "closed_table", na.rm = T)
  
  closed.sh$total = closed.sh$Closed + closed.sh$NotClosed
  closed.sh$Percent.Closed <- closed.sh$Closed/closed.sh$total * 100

ack.closed <- merge(ack.sh, closed.sh, by = "name")
ack.closed.2 <- ack.closed[1:20,c(1,5,9)]
ack.closed.2.melt <- melt(ack.closed.2)

ggplot(ack.closed.2.melt, aes(x = name, value, fill=variable))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  geom_hline(data = ack.closed, aes(yintercept = mean(Percent.Acknowledged)), 
             linetype = "dashed", show.legend = T) +
  geom_hline(data = ack.closed, aes(yintercept = mean(Percent.Closed), linetype = "Means"), 
              show.legend = T)+
  labs(x = "",
       y = "Percent",
       title = "The Percent of Issues Acknowledged and Closed\nfor each Neighborhood")+
  theme(legend.title=element_blank())+
#   scale_linetype_manual(values = c(mean(Percent.Acknowledged), mean(Percent.Closed)))+
#   scale_color_manual(values = c("black", "black"))+
  ggsave(filename = "percent_acknowledged+closed_byNeighborhood.pdf",
       path = figout, width = 6, height = 8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SCF Number Acknowledged, Closed, Reopened

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

length(which(!is.na(scf_df$id)))
length(which(!is.na(scf_df$acknowledg)))

tab <- data.frame(apply(timing[4:7], 2, function(x) {length(which(!is.na(x)))}))
colnames(tab) <- "num"
tab$event <- row.names(tab)
tab$event <- factor(tab$event, levels = c("created_at","acknowledg", "closed_at", "reopened_a"))

ggplot(tab, aes(x= event, y = num)) +
  geom_bar(stat = "identity", aes(fill = event)) +
  labs(x = "",
       y = "Count",
       title = "The Number of Issses and Follow-up Events")+
  ggsave("num_issues_ack_closed_reopened.pdf", path = figout, width = 6, height = 8)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time to Acknowledgment

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# durations
timing$timeToAckn <- difftime(timing$acknowledg, timing$created_at, units = c("days"))
timing$timeToClosed <- difftime(timing$closed_at, timing$created_at, units=c("days"))

timing <- timing[!is.na(timing$name),]

ggplot(timing, aes(x=name, y = timeToAckn))+
  geom_boxplot(aes(group=name, color = name))+
  coord_flip()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Time to Acknowledgement (days)",
       title = "Time to Issue Acknowledgment")+
  ggsave("time_to_acknowledgement.pdf", path = figout, width = 6, height = 8)

# Just plot the median values of time to Acknowlegement
acknowledged_summary <- ddply(.data = timing, .variables = c("name"), .fun = summarise,
                               median = median(timeToAckn, na.rm=T),
                               mean = mean(timeToAckn, na.rm=T),
                               sd = sd(timeToAckn, na.rm = T))

ggplot(acknowledged_summary, aes(x=name, y = median))+
  geom_bar(stat = "identity",
           aes(group=name, color = name))+
  coord_flip()+
  labs(x = "",
       y = "Days",
       title = "Median time to Issue Acknowlegement")+
  theme(legend.position = "none")+
  ggsave("median_time_to_acknowledgement.pdf", path = figout, width = 6, height = 8)

# compare median acknowlegeagainst time to crime
ack_crime <- merge(acknowledged_summary, crime_ag_neigh, by.x = "name", by.y = "Group.1", all=T)

ggplot(ack_crime, aes(x=median, y = X, label = name))+
  geom_text() +
  stat_smooth(method="lm")+
  labs(x = "Median Time to Issue Acknowledgment (Days)",
       y = "Number of Crimes",
       title = "Crimes and Time to Issue Acknowledgement")+
  ggsave("crime_v_acknowledgement.pdf", path = figout, width = 6, height = 8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time to Closure

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Box plot of time to closure by neighborhood
ggplot(timing, aes(x=name, y = timeToClosed))+
  geom_boxplot(aes(group=name, color = name))+
  coord_flip()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Days",
       title = "Time to Issue Closure")+
  ggsave("time_to_closure.pdf", path = figout, width = 6, height = 8)


# Get summary stats for each neighborhood
closed_summary <- ddply(.data = timing, .variables = c("name"), .fun = summarise,
                              median = median(timeToClosed, na.rm=T),
                              mean = mean(timeToClosed, na.rm=T),
                              sd = sd(timeToClosed, na.rm = T))


# barplot of the median time to closure
ggplot(closed_summary, aes(x=name, y = median))+
  geom_bar(stat = "identity",
           aes(group=name, color = name))+
  coord_flip()+
  labs(x = "",
       y = "Days",
       title = "Median time to Issue Closure")+
  theme(legend.position = "none")+
  ggsave("median_time_to_closure.pdf", path = figout, width = 6, height = 8)

# merge with # of crimes
clos_crime <- merge(closed_summary, crime_ag_neigh, by.x = "name", by.y = "Group.1", all=T)

# scatterplot of the number of crimes and the time to closure
ggplot(clos_crime, aes(x=median, y = X, label = name))+
  geom_text() +
  stat_smooth(method="lm")+
  labs(x = "Median Time to Issue Closure (Days)",
       y = "Number of Crimes",
       title = "The relationship between the number of crimes by neighborhood\nand the median time to SeeClickFix issue closure") +
  ggsave("crime_v_closure.pdf", path = figout, width = 6, height = 8)

lm3 <- lm(formula = X~median, data = ack_crime)
lm4 <- lm(formula = X~median, data = clos_crime)
summary(lm3)
summary(lm4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time to Closure for just sidewalk related issues

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# subset to just the sidewalks
sidewalks <- timing[which(timing$title == "Sidewalks and Curb damage"),]


# Box plot of time to closure by neighborhood
ggplot(sidewalks, aes(x=name, y = timeToClosed))+
  geom_boxplot(aes(group=name, color = name))+
  coord_flip()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Days",
       title = "Time to Issue Closure")+
  ggsave("sidewalks_time_to_closure.pdf", path = figout, width = 6, height = 8)


# Get summary stats for each neighborhood
sidewalks_closed_summary <- ddply(.data = sidewalks, .variables = c("name"), .fun = summarise,
                        median = median(timeToClosed, na.rm=T),
                        mean = mean(timeToClosed, na.rm=T),
                        sd = sd(timeToClosed, na.rm = T))

# barplot of the median time to closure
ggplot(sidewalks_closed_summary, aes(x=name, y = median))+
  geom_bar(stat = "identity",
           aes(group=name, color = name))+
  coord_flip()+
  labs(x = "",
       y = "Days",
       title = "Median time to Issue Closure")+
  theme(legend.position = "none")+
  ggsave("sidewalks_median_time_to_closure.pdf", path = figout, width = 6, height = 8)

# merge with # of crimes
side_clos_crime <- merge(sidewalks_closed_summary, crime_ag_neigh, by.x = "name", by.y = "Group.1", all=T)

# scatterplot of the number of crimes and the time to closure
ggplot(side_clos_crime, aes(x=median, y = X, label = name))+
  geom_text() +
  stat_smooth(method="lm")+
  labs(x = "Median Time to Issue Closure (Days)",
       y = "Number of Crimes",
       title = "Does the amount of crime in a neighborhood correlate\nwith the time to resolve sidewalk complaints?") +
  ggsave("sidewalks_crime_v_closure.pdf", path = figout, width = 6, height = 8)

# Slope is not significant
lm5 <- lm(formula = X~median, data = side_clos_crime)
summary(lm5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Network analysis

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# following http://kateto.net/network-visualization
# ensure id vars are factors
comments$user_id.x <- as.factor(comments$user_id.x)
comments$user_id.y <- as.factor(comments$user_id.y)

# count the interactions (edge file, called links here)
com.num <- count(df = comments, vars = c("user_id.x","user_id.y"))

# remove NAs
#com.num.n <- com.num[apply(com.num,2,function(x) !any(is.na(x))),]
com.num.n <- com.num[!is.na(com.num$user_id.x),]
links <- com.num.n[!is.na(com.num.n$user_id.y),]

# label the cols like in the edged tutorial implying direction of interaction
colnames(links) <- c("to", "from", "weight")

# make a user id list (node file)
nodes <- as.data.frame(as.factor
                       (c(as.character(edges$to), as.character(edges$from))))
nodes <- unique(nodes)
row.names(nodes) <- seq(1,nrow(nodes))
colnames(nodes) <- "id"

net <- graph.data.frame(links, nodes, directed=T)
net
plot(net)
net <- simplify(net, remove.multiple = F, remove.loops = T)
plot(net, edge.arrow.size=.1,vertex.label=NA, vertex.size=3,
     margin(t = c(-10), r = 0.1, b = 0.1, l = 0.1))


deg <- degree(net, mode="all")
V(net)$size <- deg*3
par(mar=c(,0,0,0))
pdf(file = "scf_network_zoomout.pdf")
plot(net, edge.arrow.size=.4,vertex.label=NA, vertex.size=3)
dev.off()

write.csv(links, "scf_network_edges.csv")

table(links$to)
net2 <- net
l <- layout.circle(net2)
plot(net2, layout=l)
