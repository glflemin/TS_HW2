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
library(tidyverse)


#setwd('C:\\Users\\gavin\\Desktop\\Time_Series_Data\\')
#setwd("C:\\Users\\Grant\Downloads\\")
#setwd ('C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\')
#setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")

# Import final output Homework #2 .Rdata file from HW2 reposity
path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW2\\HW2-Repo\\TS_HW2\\HW2.RData"
#path <- "C:\\Users\\gavin\\Desktop\\Time_Series_Data\\HW2.RData"
#path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework\\HW2.RData"
#path <- "C:\\Users\\Grant\\Downloads\\HW2.RData"
#path <- "C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\HW2.RData"

load(path)


############################################
########  ADDRESS SEASONALITY ##############
############################################

# SEASONAL ADF TESTING
# Automated Seasonal Differencing Test Function to find best differences #
nsdiffs(well_ts)
ndiffs(diff(well_ts, lag = 12))

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
adf.test(well_ts, alternative = "stationary", k = 0)

# check Tau with other lags (1-5), though Simmons said industry never checks more than "lag2" 
#... or was it 2 steps behind ("lag1"), I can't remember.
ADF.Pvalues <- rep(NA, 3)
for(i in 1:5){
  ADF.Pvalues[i+1] <- adf.test(well_ts, alternative = "stationary", k = i)$p.value
}
ADF.Pvalues
ndiffs(well_ts)

# IF FIT STOCHASTIC TREND 
# Take differences
diff(well_ts)

# IF DETERMINISTIC TREND (all rho/tau p-values < alpha)
# Simmons had no example code on this. Not sure of the simplest way to take errors from trend.
lm(well_ts ~ time_variable)

############################################
########  FINAL STATIONARY SERIES  #########
############################################


# PLOT STATIONARY TIME SERIES


# CLEAN ENVIRONMENT
#rm(list=ls(-ts.final))
