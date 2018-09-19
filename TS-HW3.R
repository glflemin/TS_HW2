############################################
########  TIMES SERIES HW3  ################
############################################


rm(list=ls())

library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tseries)
library(dyn)         ## Allows time series objects to interface with various regression functions
library(tidyverse)


#setwd('C:\\Users\\gavin\\Desktop\\Time_Series_Data\\')
#setwd("C:\\Users\\Grant\Downloads\\")
#setwd ('C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\')
setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")

# Import final output Homework #2 .Rdata file from HW2 reposity
#path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW2\\HW2-Repo\\TS_HW2\\HW2.RData"
#path <- "C:\\Users\\gavin\\Desktop\\Time_Series_Data\\HW2.RData"
path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework\\HW2.RData"
#path <- "C:\\Users\\Grant\\Documents\\MSA\\Fall\\Time Series\\HW2.RData"
#path <- "C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\HW2.RData"

load(path)

well_ts

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

ggplot(dataframe, aes(x=1:length(y08)))+
  geom_line(aes(y=y08))+
  geom_line(aes(y=y09))+
  geom_line(aes(y=y10))+
  geom_line(aes(y=y11))+
  geom_line(aes(y=y12))+
  geom_line(aes(y=y13))+
  geom_line(aes(y=y14))+
  geom_line(aes(y=y15))+
  geom_line(aes(y=y16))+
  geom_line(aes(y=y17))

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
summary(season3)
well_noseason <- season3$residuals
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
  ADF.Pvalues[i+1] <- adf.test(wns, alternative = "stationary", k = i)$p.value
}
ADF.Pvalues

ndiffs(well_ts, test="adf")     ## Result of 0
ndiffs(well_ts, test="kpss")    ## Result of 1 indicates 1 difference required for stationarity
ndiffs(well_ts, test="pp")      ## Result of 0

# IF FIT STOCHASTIC TREND 
# Take differences
ndiffs(diff(well_ts))

# IF DETERMINISTIC TREND (all rho/tau p-values < alpha)
t <- rep(1:129)
pot_trend <- lm(well_ts ~ t)
summary(pot_trend)               ## Coefficient of t is significant but very small (0.0038) -> essentially zero, conclude no trend

############################################
########  FINAL STATIONARY SERIES  #########
############################################

# PLOT STATIONARY TIME SERIES
# appears stationary around y = 1.0
dataframe <- data.frame(cbind(well_ts, wns))
ggplot(dataframe, aes(x=1:length(well_ts))) +geom_line(aes(y=well_ts), color='red') + geom_line(aes(y=wns), color='blue')
       

# CLEAN ENVIRONMENT
#rm(list=ls(-ts.final))
