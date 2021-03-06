############################################
########  TIMES SERIES HW3  ################
############################################


rm(list=ls())

library(forecast)
library(tseries)
library(dyn)         ## Allows time series objects to interface with various regression functions
library(tidyverse)


#setwd('C:\\Users\\gavin\\Desktop\\Time_Series_Data\\')
#setwd("C:\\Users\\Grant\Downloads\\")
#setwd ("C:\\Users\\molly\\OneDrive\\Documents\\GitHub\\TS_HW2")
setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")

# Import final output Homework #2 .Rdata file from HW2 reposity
#path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW2\\HW2-Repo\\TS_HW2\\HW2.RData"
#path <- "C:\\Users\\gavin\\Desktop\\Time_Series_Data\\HW2.RData"
path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework\\HW2.RData"
#path <- "C:\\Users\\Grant\\Documents\\MSA\\Fall\\Time Series\\HW2.RData"
#path <- "C:\\Users\\molly\\OneDrive\\Documents\\GitHub\\TS_HW2\\HW2.RData"

load(path)

well_ts <- ts(well_df$well, start=c(2007, 10), frequency=12)

############################################
########  ADDRESS SEASONALITY ##############
############################################


well <- well_ts[-(1:3)]
y08 <- well[1:12]
y09 <- well[13:24]
y10 <- well[25:36]
y11 <- well[37:48]
y12 <- well[49:60]
y13 <- well[61:72]
y14 <- well[73:84]
y15 <- well[85:96]
y16 <- well[97:108]
y17 <- well[109:120]

dataframe <- data.frame(cbind(y08,y09,y10,y11,y12,y13,y14,y15,y16,y17))
well_ts
many_plot <- ggplot(dataframe, aes(x=1:length(y08)))+
  geom_line(aes(y=y08))+
  geom_line(aes(y=y09))+
  geom_line(aes(y=y10))+
  geom_line(aes(y=y11))+
  geom_line(aes(y=y12))+
  geom_line(aes(y=y13))+
  geom_line(aes(y=y14))+
  geom_line(aes(y=y15))+
  geom_line(aes(y=y16))+
  geom_line(aes(y=y17))+
  scale_x_continuous(breaks=seq(1, 12, 1))+
  labs(title="Change in Well Depth Between 2007 and 2017", x="Month of Year", y="Change in Well Depth (Ft)")
many_plot

# SEASONAL ADF TESTING
# Automated Seasonal Differencing Test Function to find best differences #
  ## comments by B.Jenista
nsdiffs(well_ts)    ## Result of 0 indicates no differencing required at seasonal level

#ndiffs(diff(well_ts, lag = 12))  ## Alternate method to test seasonal stationarity
#?ndiffs

# Since no differencing --> dummy variables to remove any seasonality (deterministic)
# Fit with dummy variables, need a season variable
season=matrix(rep(c(0,0,0,0,0,0,0,0,0,1,0,
                    0,0,0,0,0,0,0,0,0,0,1,
                    0,0,0,0,0,0,0,0,0,0,0,
                    1,0,0,0,0,0,0,0,0,0,0,
                    0,1,0,0,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,0,0,0,
                    0,0,0,1,0,0,0,0,0,0,0,
                    0,0,0,0,1,0,0,0,0,0,0,
                    0,0,0,0,0,1,0,0,0,0,0,
                    0,0,0,0,0,0,1,0,0,0,0,
                    0,0,0,0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,0,0,1,0,0),11),byrow=T,nrow=132)
season <- season[-(130:132),]

season2 <- data.frame(cbind(season, well_ts))

season3 <- lm(well_ts ~ season)
summary(season3)                     ## R-squared = 0.441
well_noseason <- season3$residuals     
fitted <- ts(season3$fitted.values, start=c(2007,10), frequency = 12)
wns <- ts(well_noseason, start=c(2007,10), frequency=12)

############################################
########  IDENTIFY/ADDRESS TREND  ##########
############################################

#ADF TESTING#
#Check rho with lag0
#adf.test(well_ts, alternative = "stationary", k = 0)

# check Tau with other lags (0-2), though Simmons said industry never checks more than "lag2" 
ADF.Pvalues <- rep(NA, 3)
for(i in 0:2){
  ADF.Pvalues[i+1] <- adf.test(well_ts, alternative = "stationary", k = i)$p.value
}
ADF.Pvalues

ndiffs(well_ts, test="adf")     ## Result of 0
ndiffs(well_ts, test="kpss")    ## Result of 0
ndiffs(well_ts, test="pp")      ## Result of 0

ndiffs(wns, test="adf")     ## Result of 0
ndiffs(wns, test="kpss")    ## Result of 1 indicates 1 difference required for stationarity
ndiffs(wns, test="pp")      ## Result of 0


# IF DETERMINISTIC TREND (all rho/tau p-values < alpha)
t <- rep(1:129)
pot_trend1 <- lm(well_ts ~ t)
summary(pot_trend1)               ## Coefficient of t is significant but very small (0.0043) -> essentially zero, conclude no trend

pot_trend2 <- lm(wns ~ t)
summary(pot_trend2)               ## Coefficient of t is significant but very small (0.0046) -> essentially zero, conclude no trend

############################################
########  FINAL STATIONARY SERIES  #########
############################################

stat1 <- pot_trend1$residuals
stat2 <- pot_trend2$residuals

dates <- data.frame(seq(
  from=as.Date("2007-10-05"),
  to=as.Date("2018-06-13"),
  by="month"))
colnames(dates) <- 'dates'

dataframe <- data.frame(cbind(well_ts, wns, fitted, stat1, stat2))

## Original time series
ggplot(dataframe, aes(x=dates$dates)) + geom_line(aes(y=well_ts), color='red') +
  labs(title="Stationary Time Series: Original Well Depth for Well G-561", x="Year", y="Change in Well Depth (Ft)")

## Stationary series -> residuals after modeling deterministic seasonality
ggplot(dataframe, aes(x=dates$dates)) + geom_line(aes(y=wns), color='green') +
  labs(title="Stationary Time Series: Residuals of Deterministic Seasonality", x="Year", y="Change in Well Depth (Ft)")

## Stationary series -> residuals after modeling deterministic trend
ggplot(dataframe, aes(x=dates$dates)) + geom_line(aes(y=stat1), color='black') +
  labs(title="Stationary Time Series: Residuals of Deterministic Trend", x="Year", y="Change in Well Depth (Ft)")

## Stationary series -> residuals after modeling deterministic seasonality and trend
ggplot(dataframe, aes(x=dates$dates)) + geom_line(aes(y=stat2), color='purple') +
  labs(title="Stationary Time Series: Residuals of Deterministic Seasonality and Trend", x="Year", y="Change in Well Depth (Ft)")

## Comparison of original data and residuals after removing deterministic seasonality
ggplot(dataframe, aes(x=dates$dates)) + geom_line(aes(y=well_ts), color='red') + 
  geom_line(aes(y=wns), color='green') +
  labs(title="Comparison Between Original and Residuals of Deterministic Seasonality", x="Year", y="Change in Well Depth (Ft)")

# CLEAN ENVIRONMENT
#rm(list=ls(-ts.final))
