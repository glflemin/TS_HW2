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

# SEASONAL ADF TESTING
# Automated Seasonal Differencing Test Function to find best differences #
  ## comments by B.Jenista
nsdiffs(well_ts)    ## Result of 0 indicates no differencing required at seasonal level

#ndiffs(diff(well_ts, lag = 12))  ## Alternate method to test seasonal stationarity
#?ndiffs
# IF DETERMINISTIC SEASONALITY
# Fit with dummy variables, need a season variable
lm(well_ts ~ season_variable)

# IF STOCHASTIC SEASONALITY
# Take Differences (differences should be length of season)
diff(well_ts, lag = 12)


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
ndiffs(well_ts, test="kpss")    ## Result of 1 indicates 1 difference required for stationarity
ndiffs(well_ts, test="pp")      ## Result of 0

# IF FIT STOCHASTIC TREND 
# Take differences
ndiffs(diff(well_ts))
z <- ts(diff(well_ts), start=C(2007,10), frequency = 12)  ## Test for plotting

# IF DETERMINISTIC TREND (all rho/tau p-values < alpha)
# Simmons had no example code on this. Not sure of the simplest way to take errors from trend.
lm(well_ts ~ time_variable)

############################################
########  FINAL STATIONARY SERIES  #########
############################################

# PLOT STATIONARY TIME SERIES
# appears stationary around y = 1.0
plot(well_ts, xlab='Time',ylab='Change in Height (Ft)',main='Stationary Well Time Series Graph')
plot(diff(well_ts), xlab='Time',ylab='Change in Height (Ft)',main='First Difference Well Time Series Graph')   ## test   
# CLEAN ENVIRONMENT
#rm(list=ls(-ts.final))
