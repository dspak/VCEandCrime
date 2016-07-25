# census data summary


# load/install required packages
list.of.packages <- c("ggplot2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"

# Load and format data
df <- read.csv("data/processed/combined/crime_scf_census_by_mneigh_ym_ITSv2.csv", stringsAsFactors = F)

census <- df[,c(1,4:112)]
census <- unique(census)

census$Total.Population <- as.integer(census$Total.Population)

hist(df$Total.Population)

str(census)


ggplot(df, aes(x = mNeighborhood, y = Total.Population))+
  geom_bar(stat="identity")+
  coord_flip()

boxplot(census$Total.Population)

ggplot(census, aes(unique(Total.Population)))+
  geom_histogram(stat = "bin", bins = 5)

qs <- quantile(census$Total.Population)

ggplot(census, aes(x = reorder(mNeighborhood, Total.Population), y = Total.Population, label = Total.Population))+
  geom_bar(stat="identity", aes(fill = mNeighborhood))+
#  geom_hline(yintercept = qs, linetype = "dotted")+
  geom_text()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(x = "",
       y = "Population",
       title = "2014 ACS 5-year estimates of neighborhood populations\nManually aggregated from tracts to neighborhoods")+
  ggsave("population_byNeighborhood_bar.pdf", path = figout, height = 6, width = 6)

ggplot(census)+
  geom_boxplot()
