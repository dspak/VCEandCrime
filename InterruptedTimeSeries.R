# Interrupted Time Series Analysis
# Fri Jul 15 15:06:04 2016 ------------------------------
# Thu Jul 21 15:55:41 2016 ------------------------------
# Wed Aug 24 13:47:29 2016 ------------------------------


# Model building attempt

# load/install required packages
list.of.packages <- c("Wats", "ggplot2","nlme", "forecast", "lmtest", "tseries", "changepoint")

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

# correction for % white?
df$Frac.White <- df$Total.Population..White.Alone / df$Total.Population

# correction for % graduated from high school
df$Frac.GradHighSchool <- df$Civilian.Population.16.to.19.Years..High.school.graduate..or.enrolled..in.school. / df$Total.Population

# correction for age
df$Frac.Ov64 <- (df$Total.Population..65.to.74.Years + df$Total.Population..75.to.84.Years + df$Total.Population..85.Years.and.over)/df$Total.Population




##################################################
## Subset to some neighborhoods

# model excluding Long Wharf (exceptionally high crime rate, positive slope post scf)
lw <- df$mNeighborhood!="LongWharf"
df.noLW <- df[lw,]

# subset to just look at Downtown, Hill, etc
downtown <- subset(df, mNeighborhood == "Downtown")
hill <- subset(df, mNeighborhood == "Hill")

# label downtown times as a time series
downtown$Crime.Rate <- ts(data = downtown$Crime.Rate, )

###
### Clean up and write out this file to distribute
###
###
# write.csv(df, file = "data/processed/combined/crime_scf_census_by_mneigh_ym_ITSv2.csv", row.names = F)
# write.csv(df[df$mNeighborhood=="Hill",], file = "data/processed/combined/crime_scf_census_Hill_ym_ITSv2.csv", row.names = F)

# plot for downtown
ggplot(data = downtown, mapping = aes(x = YearMonth, y = Crime.Rate)) +
  geom_line(aes(group=1))

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



lmeFit9 <- lme(Crime.Rate ~ dateSCFctr + postSCF + Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. +
                 dateSCFctr*postSCF,
               method = "ML",
               random = list(~1 + dateSCFctr | mNeighborhood),
               correlation = corAR1(0.66),
               data = df)
summary(lmeFit9)

lmefit10 <- lme(Crime.Rate ~ dateSCFctr + postSCF + 
                  Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                  Frac.GradHighSchool + dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corAR1(0.66),
                data = df)
sink("models/SeeClickFix_Crime_ITS_lme_v7.txt")
summary(lmefit10)
sink()

lmefit11 <- lme(Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                  Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                  Frac.GradHighSchool + dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corAR1(0.66),
                data = df)
sink("models/SeeClickFix_Crime_ITS_lme_v7.txt")
summary(lmefit11)
sink()

lmefit12 <- lme(Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                  Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                  Frac.GradHighSchool + dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)
sink("models/SeeClickFix_Crime_ITS_lme_v8.txt")
summary(lmefit12)
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model with an AR1 correlation structure")
cat("\n")
lrtest(lmefit11,lmefit12)
sink()
#*********************************************************

lmefit13 <- lme(Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                  Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                  Frac.GradHighSchool + Frac.Ov64 + dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)
sink("models/SeeClickFix_Crime_ITS_lme_v9.txt")
summary(lmefit13)
sink()
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model with an AR1 correlation structure")
cat("\n")
lrtest(lmefit11,lmefit12)
sink()

# lme with just the most basic fixed effects (+autocorrelation structure)
lmefit14 <- lme(Num.Crimes ~ dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)

sink("models/SeeClickFix_Crime_ITS_lme_v10.txt")
summary(lmefit14)
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model with fixed effects for education, income, race and age")
cat("\n")
lrtest(lmefit14,lmefit13)
sink()

lmefit15 <- lme(Num.Crimes ~ dateSCFctr*postSCF*mNeighborhood,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)
summary(lmefit15)

lmefit16 <- lme(Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                  Frac.White + 
                  dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)

lmefit17 <- lme(Num.Crimes ~ dateSCFctr*postSCF*mNeighborhood,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)
sink("models/SeeClickFix_Crime_ITS_lme_v11.txt")
summary(lmefit17)
lrtest(lmefit17, glmp.5)
sink()


lmefit18 <- lme(Num.Crimes ~ dateSCFctr*postSCF,
                method = "ML",
                random = list(~1 + dateSCFctr | mNeighborhood),
                correlation = corARMA(p = 1, q = 2),
                data = df)
sink("models/SeeClickFix_Crime_ITS_lme_v12.txt")
summary(lmefit18)
sink()

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

plot(hill$Crime.Rate, type='l')
arimaFit3 <- arima(hill$Crime.Rate, order = c(1,0,0))
summary(arimaFit3)
arimaFit4 <- arima(hill$Crime.Rate, order = c(1,1,1))
summary(arimaFit4)
arimaFit4 <- arima(hill$Crime.Rate, order = c(4,0,0))
summary(arimaFit2)


arimaForecast1 <- forecast(arimaFit1, h = 12)
plot(arimaForecast1)

arimaForecast1 <- forecast(arimaFit2, h = 12)
plot(arimaForecast2)


arimaForecast3 <- forecast(arimaFit3, h = 12)
plot(arimaForecast3)

arimaForecast4 <- forecast(arimaFit4, h = 12)
plot(arimaForecast4)

arima.auto1 <- auto.arima(hill$Crime.Rate, 
                          stationary = T, 
                          seasonal = T, 
                          xreg = hill$postSCF)
summary(arima.auto1)
arimaForecast5 <- forecast(arime.auto1, xreg = hill$postSCF)
plot(arimaForecast5, main="The Hill\nARIMA(5,1,1)")
summary(arime.auto1)

hill$Crime.Rate <- ts(hill$Crime.Rate)
arimaFit5 <- arima(hill$Crime.Rate, order = c(2,0,0))

arimaFit6 <- Arima(hill$Crime.Rate, order = c(1,0,0), xreg = hill$postSCF)
arimaForecast6 <- forecast(arimaFit6, xreg = hill$postSCF)
plot(arimaForecast6)
arimaFit6

arimaFit7 <- Arima(hill$Crime.Rate, order = c(5,1,1), xreg = hill$postSCF)
arimaFit7

arimaFit8 <- Arima(hill$Crime.Rate, order = c(5,1,1), xreg = hill$postSCF)
arimaForecast8 <- forecast(arimaFit8, xreg = hill$postSCF)
plot(arimaForecast8)

tsdisplay(diff(hill$Crime.Rate))


arimaFit9 <- Arima(hill$Crime.Rate, 
                   order = c(1,1,0),
                   seasonal = list(order=c(1,1,0),period=12),
                   xreg = hill$postSCF,
                   include.mean = F)
summary(arimaFit9)

plot(forecast(arimaFit9, xreg = hill$postSCF))

adf.test(diff(hill$Crime.Rate), alternative = "stationary", k=0)

acf(hill$Crime.Rate)
pacf(hill$Crime.Rate)


#########################################################################################################################
###########
########### Changepoint analysis
###########

set.seed(10)
mean <- cpt.mean(hill$Crime.Rate, method = 'PELT')
mean <- cpt.mean(hill$Crime.Rate, method = 'BinSeg')
mean <- cpt.mean(hill$Crime.Rate, method = 'AMOC')


par(mfrow=c(1,2))
plot(mean, xaxt = "n", main = "Change Point Analysis\nThe Hill", ylab = "Crime Rate")
axis(1, at = 132, labels = (hill$YearMonth[133]))
plot(x=1:length(hill$SeeClickFix.Issue.Rate), y=hill$SeeClickFix.Issue.Rate, 
     xaxt = "n", xlim = c(100,length(hill$SeeClickFix.Issue.Rate)),
     ylab = "SeeClickFix Issue Rate",
     main = "SeeClickFix Issue Rate over time\nThe Hill",
     xlab = "Time")
axis(1, at = 132, labels = (hill$YearMonth[133]))
abline(v = 132)
dev.off()


cpts(mean)
plot(x=seq(from = 1, to=163, by = 1),
     y=mean@param.est$mean)
plot(mean)
length(coef(mean))
length(mean@param.est$mean)

# calculate the changepoint for each neighborhood and create dataframe with the date
neigh <- unique(df$mNeighborhood)
changepoints <- list()
changepoint.nums <- list()
for (n in 1:length(neigh)) {
  changepoints[[n]] <- cpt.mean(df$Crime.Rate[df$mNeighborhood == neigh[n]])
  changepoint.nums[[n]] <- cpts(changepoints[[n]])
}
unlist.cpn <- unlist(changepoint.nums)
dates <- c()
for (i in 1:length(unlist.cpn)) {
  dates[i] <- hill$YearMonth[unlist.cpn[i]]
}
changes.dates <- data.frame(neigh, unlist.cpn, dates) 

dir.create("figures/changepoint")
for (e in 1:length(neigh)){
  jpeg(filename = paste("figures/changepoint/changepoint_", neigh[e], ".jpg", sep = ""))
  plot(changepoints[[e]], xaxt = "n", 
       main = paste("Change Point Analysis\n",neigh[e], sep = ""),
       ylab = "Crime Rate")
  axis(1, at = unlist.cpn[e], labels = (hill$YearMonth[unlist.cpn[e]]))
  dev.off()
}
scfstart <- as.POSIXlt("2008-01-01", format = "%Y-%m-%d")
str(scfstart)
str(changes.dates)
changes.dates$dates.2 <- strptime(as.character(changes.dates$dates), format = "%Y-%m-%d")
ggplot(changes.dates, aes(x=neigh, y=dates.2))+
  geom_point(aes(color=neigh, size=3))+
  geom_hline(aes(yintercept = as.numeric(scfstart)), linetype=2)+
  coord_flip()+
  labs(x="",
       y="Date",
       title="Date at Change-Point\nAMOC Fitting")+
  theme(legend.position="none") +
  ggsave(filename = "changepoint/changepoint_dates_perNeighborhood.pdf", path = figout, 
         height = 6, width = 6)


#########################################################################################################################
###########
########### Models using Poisson 
###########

glmp.1 <- glm(formula = Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                Frac.GradHighSchool + Frac.Ov64 + dateSCFctr*postSCF, 
              family = poisson(),
              data = df)
summary(glmp.1)                

glmp.2 <- glm(formula = Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                Frac.GradHighSchool + Frac.Ov64 + dateSCFctr*postSCF + mNeighborhood*postSCF, 
              family = poisson(),
              data = df)
summary(glmp.2)
glm.2f <- predict(glmp.2)
plot.glm.2f <- data.frame(df$YearMonthTime, df$mNeighborhood, df$postSCF, df$Num.Crimes, as.numeric(glm.2f))

glmp.3 <- glm(formula = Num.Crimes ~ dateSCFctr + postSCF + Total.Population +
                Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                Frac.GradHighSchool + Frac.Ov64 + dateSCFctr*postSCF*mNeighborhood, 
              family = poisson(),
              data = df)
sink("models/SeeClickFix_Crime_ITS_glmPoisson_v3.txt")
summary(glmp.3)
sink()
anova(glmp.3)
plot(df)

glmp.4 <- glm(formula = Num.Crimes ~ dateSCFctr + postSCF + mNeighborhood-1 + Total.Population +
                Frac.White + Median.household.income..In.2014.Inflation.Adjusted.Dollars. + 
                Frac.GradHighSchool + Frac.Ov64 + dateSCFctr*postSCF*mNeighborhood, 
              family = poisson(),
              data = df)
sink("models/SeeClickFix_Crime_ITS_glmPoisson_v4.txt")
summary(glmp.4)
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model with with an estimated intercept")
cat("\n")
lrtest(glmp.3,glmp.4)
sink()

glmp.5 <- glm(formula = Num.Crimes ~ dateSCFctr*postSCF*mNeighborhood, 
              family = poisson(),
              data = df)
sink("models/SeeClickFix_Crime_ITS_glmPoisson_v5.txt")
summary(glmp.5)
cat("\n")
cat("\n")
cat("*********************")
cat("\n")
cat("# lrtest comparing this model to the model with with an estimated intercept")
cat("\n")
lrtest(glmp.4,glmp.5)
sink()
par(mfrow=c(2,2))
plot(glmp.5)

names(df)
plot(df[,c(11,79,119:121)])
par(mfrow=c(2,2))
plot(glmp.4)

neigh <- unique(df$mNeighborhood)
dir.create(paste(figout, "glmPoissonFits",sep =  "/"))
for (n in 1:length(neigh)) {
  ggplot(plot.glm.2f[plot.glm.2f$df.mNeighborhood == neigh[n],], aes(x = df.YearMonthTime, y = as.numeric.glm.2f.))+
    geom_line(data = subset(plot.glm.2f[plot.glm.2f$df.mNeighborhood == neigh[n],], df.postSCF == 0), color = "red")+
    geom_line(data = subset(plot.glm.2f[plot.glm.2f$df.mNeighborhood == neigh[n],], df.postSCF == 1), color = "red")+
    geom_line(data = subset(plot.glm.2f[plot.glm.2f$df.mNeighborhood == neigh[n],]), aes(x = df.YearMonthTime, y = df.Num.Crimes))+
    labs(x="", y="Crime Rate", title=paste("Interrupted Time Series analysis of SeeClickFix effect on Crime Rate\nPoisson Model", neigh[n], sep="\n"))+
    ggsave(filename = paste("glmpFit.CrimeRatevTime", neigh[n], "pdf", sep = "."), path = paste(figout, "glmPoissonFits",sep =  "/"), height = 6, width = 8)
}


##################################################
## Check other scf introduction dates

# col for diff dates of SCF introduction
dateBreak <- seq.Date(from = as.Date("2001-01-01"), to = as.Date("2011-08-01"), by = "month")

# indicator for post pre/post date break
breakmat <- matrix(nrow = length(df$YearMonth), ncol = length(dateBreak))
for (b in 1:length(dateBreak)){
  breakmat[,b] <- df$YearMonth >= dateBreak[b]
}

# incorporate into a modeling dataframe

breaktest <- data.frame(Num.Crimes=df$Num.Crimes,
                        mNeighborhood=df$mNeighborhood,
                        dateSCFctr=df$dateSCFctr,
                        breakmat)

# create list of cols to interate through
breakcols <- seq(4,ncol(breaktest), by =1)

# create vector for storing the AIC values
breaktest.AIC <- c()

# Poisson model interating on which col sets the breakpoint
for (c in breakcols){
  f <- as.formula(paste("Num.Crimes ~ dateSCFctr*mNeighborhood*", colnames(breaktest)[c], sep = ""))
  glmp.tmp <- glm(formula = f, 
                family = poisson(),
                data = breaktest)
  breaktest.AIC[c] <- AIC(glmp.tmp)
}

# eliminate first three NA values for plotting
breaktest.AIC <- breaktest.AIC[4:length(breaktest.AIC)]

# plot the result
plotbreak <- data.frame(dateBreak, breaktest.AIC)
ggplot(plotbreak, aes(x=dateBreak, y=breaktest.AIC))+
  geom_point()+
  geom_line()+
  labs(x="Date",
       y="AIC",
       title="AIC values of Interrupted Time Series Poisson Linear Models\nvarying the date of Interruption")+
  ggsave("glmp_ITS_AICvalueComparison.pdf", path = figout, height = 6, width = 6)

plotbreak[which(plotbreak$breaktest.AIC==min(plotbreak$breaktest.AIC)),]

min(plotbreak$breaktest.AIC)
