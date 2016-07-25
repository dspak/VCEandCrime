# Interrupted Time Series Analysis
# Fri Jul 15 15:06:04 2016 ------------------------------
# Thu Jul 21 15:55:41 2016 ------------------------------


# Model building attempt

# load/install required packages
list.of.packages <- c("Wats", "ggplot2","nlme", "forecast", "lmtest")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, require, character.only = TRUE)


# base working directory
setwd("/Users/danielspakowicz/Box Sync/projects/scfbw/")

# figs output dir
figout <- "/Users/danielspakowicz/Box Sync/projects/scfbw/figures"


# read in data
df <- read.csv(file = "data/processed/combined/crime_scf_census_by_mneigh_ym_ITS.csv", stringsAsFactors = F)

df$mNeighborhood <- gsub(pattern = " ", "", df$mNeighborhood)
df$mNeighborhood <- gsub(pattern = "/", "_", df$mNeighborhood)


# set SCF NA's to 0
df$SeeClickFix.Issue.Rate[is.na(df$SeeClickFix.Issue.Rate)] <- 0

# reformat YearMonth to time

df$YearMonth <- as.character(df$YearMonth)
df$YearMonth <- gsub(pattern = "(\\d{4})(\\d{2})", replacement = "\\1-\\2-01", x = df$YearMonth)
df$YearMonthTime <-as.Date(df$YearMonth, format = "%Y-%m-%d")


# date of SCF introduction
dateSCF <- as.Date("2008-01-01", format = "%Y-%m-%d")

# indicator for post scf dates
df$postSCF <- (df$YearMonth >= dateSCF)

# center the date at introduction of SCF
df$dateSCFctr <- df$YearMonthTime - dateSCF

df$X.1 <- df$X <- NULL

###
### Clean up and write out this file to distribute
###
###
write.csv(df, file = "data/processed/combined/crime_scf_census_by_mneigh_ym_ITSv2.csv", row.names = F)
write.csv(df[df$mNeighborhood=="Hill",], file = "data/processed/combined/crime_scf_census_Hill_ym_ITSv2.csv", row.names = F)

# subset to just look at Downtown
downtown <- subset(df, mNeighborhood == "Downtown")

# plot for downtown
ggplot(data = downtown, mapping = aes(x = YearMonth, y = Crime.Rate)) +
  geom_line(aes(group=1))

# label downtown times as a time series
downtown$Crime.Rate <- ts(data = downtown$Crime.Rate, )


#########################################################################################################################
###########
########### Models using generalized least squares
###########

glsFit1 <- gls(model       = Crime.Rate ~ dateSCFctr + postSCF + dateSCFctr:postSCF,
               data        = downtown,
               correlation = corAR1(0.25))
summary(glsFit1)


## Create prediction dataset
newdata <- data.frame(DateNumCtr = seq(min(downtown$dateSCFctr), max(downtown$dateSCFctr), by  = 1))
newdata$postBombing <- (newdata$DateNumCtr >= 0)

## Predict
newdata$BirthRateMonthly <- predict(glsFit1, newdata = newdata)

ggplot(data = canadian, mapping = aes(x = DateNumCtr, y = BirthRateMonthly)) +
  layer(geom = "line") +
  layer(data = subset(newdata, DateNumCtr < 0), geom = "line", color = "red", size = 1.5) +
  layer(data = subset(newdata, DateNumCtr >= 0), geom = "line", color = "red", size = 1.5) +
  theme_bw() + theme(legend.key = element_blank())


# model
glsFit2 <- gls(model       = Crime.Rate ~ mNeighborhood + dateSCFctr + postSCF + dateSCFctr:postSCF,
               data        = df,
               correlation = corAR1(0.25))
summary(glsFit2)

glsFit3 <- gls(model       = Crime.Rate ~ mNeighborhood + dateSCFctr + postSCF + mNeighborhood:postSCF,
               data        = df,
               method = "ML",
               correlation = corAR1(0.25))
sink(file = "models/SeeClickFix_Crime_ITS_gls_v1.txt")
summary(glsFit3)
sink()

glsFit4 <- gls(model       = Crime.Rate ~ SeeClickFix.Issue.Rate + mNeighborhood + dateSCFctr + postSCF + 
                 mNeighborhood:postSCF,
               data        = df,
               correlation = corAR1(0.25))
summary(glsFit4)


#########################################################################################################################
###########
########### Models using linear mixed effects
###########


lmeFit1 <- lme(Crime.Rate ~ SeeClickFix.Issue.Rate + dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
                 random = list(mNeighborhood = ~1),
               data = df)
summary(lmeFit1)


lmeFit2 <- lme(Crime.Rate ~ dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(mNeighborhood = ~1),
               data = df)
sink(file = "models/SeeClickFix_Crime_ITS_lme_v1.txt")
summary(lmeFit2)
sink()

# model with random intercept and slope
lmeFit3 <- lme(Crime.Rate ~ dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(~1 + dateSCFctr | mNeighborhood),
               data = df)
sink(file = "models/SeeClickFix_Crime_ITS_lme_v2.txt")
summary(lmeFit3)
sink()

hist(lmeFit3$residuals)
lrtest(lmeFit2, lmeFit3)
??lrtest

## Create prediction dataset
pred.lmeFit3 <- predict(lmeFit3)
plot.pred.lmeFit3 <- data.frame(df$YearMonthTime, df$mNeighborhood, df$postSCF, df$Crime.Rate, as.numeric(pred.lmeFit3))

plot(pred.lmeFit3)

ggplot(data = canadian, mapping = aes(x = DateNumCtr, y = BirthRateMonthly)) +
  layer(geom = "line") +
  layer(data = subset(newdata, DateNumCtr < 0), geom = "line", color = "red", size = 1.5) +
  layer(data = subset(newdata, DateNumCtr >= 0), geom = "line", color = "red", size = 1.5) +
  theme_bw() + theme(legend.key = element_blank())


colors20 <- c("#466961", "#6f3cc6", "#77d952", "#c94abf", "#cfd344", "#7170d0", "#579140", "#542c6c", "#6cd6a1", "#cc4872", "#8ecbcd", "#d54d34", "#718cba", "#c98937", "#3b2935", "#c5c881", "#ca8abe", "#595f2b", "#cc9f91", "#7f3e33")

ggplot(plot.pred.lmeFit3, aes(x = df.YearMonthTime, y = as.numeric.pred.lmeFit3.))+
  geom_line(data = subset(plot.pred.lmeFit3, df.postSCF == 0), aes(group=df.mNeighborhood, color = df.mNeighborhood))+
  geom_line(data = subset(plot.pred.lmeFit3, df.postSCF == 1), aes(group=df.mNeighborhood, color = df.mNeighborhood))+
  geom_line(data = subset(plot.pred.lmeFit3, df.postSCF == 0), aes(x = df.YearMonthTime, y = mean(as.numeric.pred.lmeFit3.)), color = "black", linetype = "dotted", size = 1)+
  geom_line(data = subset(plot.pred.lmeFit3, df.postSCF == 1), aes(x = df.YearMonthTime, y = mean(as.numeric.pred.lmeFit3.)), color = "black", linetype = "dotted", size = 1)+
  labs(x="", y="Crime Rate", title="Interrupted Time Series analysis of SeeClickFix effect on Crime Rate\nMixed Effects Model")+
  ggsave("lmeFit.CrimeRatevTimebyNeighborhood.pdf", path = figout, width = 8, height = 6)


# make individual plot for each neighborhood

neigh <- unique(df$mNeighborhood)
dir.create(paste(figout, "lmeFits",sep =  "/"))
for (n in 1:length(neigh)) {
  ggplot(plot.pred.lmeFit3[plot.pred.lmeFit3$df.mNeighborhood == neigh[n],], aes(x = df.YearMonthTime, y = as.numeric.pred.lmeFit3.))+
    geom_line(data = subset(plot.pred.lmeFit3[plot.pred.lmeFit3$df.mNeighborhood == neigh[n],], df.postSCF == 0), color = "red")+
    geom_line(data = subset(plot.pred.lmeFit3[plot.pred.lmeFit3$df.mNeighborhood == neigh[n],], df.postSCF == 1), color = "red")+
    geom_line(data = subset(plot.pred.lmeFit3[plot.pred.lmeFit3$df.mNeighborhood == neigh[n],]), aes(x = df.YearMonthTime, y = df.Crime.Rate))+
    labs(x="", y="Crime Rate", title=paste("Interrupted Time Series analysis of SeeClickFix effect on Crime Rate\nMixed Effects Model", neigh[n], sep="\n"))+
    ggsave(filename = paste("lmeFit.CrimeRatevTime", neigh[n], "pdf", sep = "."), path = paste(figout, "lmeFits",sep =  "/"), height = 6, width = 8)
}


# model excluding Long Wharf (exceptionally high crime rate, positive slope post scf)
lw <- df$mNeighborhood!="LongWharf"
df.noLW <- df[lw,]

lmeFit4 <- lme(Crime.Rate ~ dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(~1 + dateSCFctr | mNeighborhood),
               data = df.noLW)
sink(file = "models/SeeClickFix_Crime_ITS_lme_v3.txt")
summary(lmeFit4)
sink()

# model of just the Hill
hill <- df[df$mNeighborhood=="Hill",]
lmeFit5 <- lme(Crime.Rate ~ dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(~1 + dateSCFctr | mNeighborhood),
               data = hill)
sink(file = "models/SeeClickFix_Crime_ITS_lme_v4.txt")
summary(lmeFit5)
sink()

# include a correlation matrix
lmeFit6 <- lme(Crime.Rate ~ dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(~1 + dateSCFctr | mNeighborhood),
               correlation = corAR1(0.66),
               data = df.noLW)

sink("models/SeeClickFix_Crime_ITS_lme_v5.txt")
summary(lmeFit6)
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model that lacks an AR1 correlation structure")
cat("\n")
lrtest(lmeFit4, lmeFit6)
sink()



#**************CURRENT BEST MODEL************************

# include a correlation matrix for all data (including LW) 
lmeFit7 <- lme(Crime.Rate ~ dateSCFctr + postSCF +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(~1 + dateSCFctr | mNeighborhood),
               correlation = corAR1(0.66),
               data = df)

sink("models/SeeClickFix_Crime_ITS_lme_v6.txt")
summary(lmeFit7)
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model that lacks an AR1 correlation structure")
cat("\n")
lrtest(lmeFit3, lmeFit7)
sink()
#*********************************************************



#########################################################################################################################
###########
########### Models using ARIMA
###########

plot(downtown$Crime.Rate)
arimaFit1 <- arima(downtown$Crime.Rate, order = c(1,0,0))
summary(arimaFit1)
arimaFit2 <- arima(downtown$Crime.Rate, order = c(4,0,0))
summary(arimaFit2)

arimaForecast1 <- forecast(arimaFit1, h = 12)
arimaForecast1
plot(arimaForecast1)


arimaForecast2 <- forecast(arimaFit2, h = 12)
plot(arimaForecast2)


arimaFit3 <- arima(hill$Crime.Rate, order = c(1,0,0))
summary(arimaFit3)
arimaFit4 <- arima(hill$Crime.Rate, order = c(4,0,0))
summary(arimaFit4)
