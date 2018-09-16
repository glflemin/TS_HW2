#Library to manipulate dates and excel and dataframes

#if you don't have any of these, install them
library(readxl)
library(lubridate)
library(dplyr)
library(zoo)
library(stringr)
library(tidyverse)
library(ggfortify)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)

# for cleaning global environment

rm(list=ls())

# ensuring correct work directory

#this.dir <- dirname(parent.frame(2)$ofile)

#setwd(this.dir)



#setwd('C:\\Users\\gavin\\Desktop\\Time_Series_Data\\')
setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW2")
#setwd("C:\\Users\\Grant\Downloads\\")
#setwd ('C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\')
#setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")

# importing the Excel file

#wbpath <- "C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\G-561_T.xlsx"
wbpath <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW1\\G_561_T.xlsx"
#wbpath <- "C:\\Users\\gavin\\Desktop\\Time_Series_Data\\G-561_T.xlsx"
#wbpath <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework\\G-561_T.xlsx"
#wbpath <- "C:\\Users\\Grant\\Downloads\\G_561_T.xlsx"


G_561_T <- read_excel(wbpath, sheet=3) # need the full filepath to make this work

#building ideal vector of dates

#necessary to convert it to data.frame for merge later

vdate <- data.frame(seq(
  
  from=as.POSIXct("2007-10-05 0:00", tz="EST"),
  
  to=as.POSIXct("2018-06-13 0:00", tz="EST"),
  
  by="hour"
  
) )

length(vdate[[1]])

colnames(vdate) <- 'date_time' #renaming vector to match other merge datafram


# Rounding dates to their hour, Frankensteining date & hour together, then converting to POSIX

# Assumes all times are EST

# TODO: Correct EST assumption for timestamps in EDT 

G_561_T$date_time <- paste(G_561_T$date," ", lubridate::hour(G_561_T$time),":00:00", sep = "")

G_561_T$date_time <- as.POSIXct(G_561_T$date_time, tz="EST")


#View(G_561_T)


#grouping water levels by taking average of each hour

clean_well <- G_561_T %>%
  
  group_by(date_time) %>%
  
  summarise(mean_corr=mean(Corrected)) %>%
  
  select(date_time, mean_corr)

#View(clean_well)

#calc avg stdev

meandepth = mean(clean_well$mean_corr, na.rm=TRUE)

stdevdepth = sd(clean_well$mean_corr, na.rm=TRUE)



#THE MERGE

final_df <- left_join(vdate, clean_well, by='date_time')

rm(list=setdiff(ls(), "final_df"))

str(final_df)

#View(final_df)


##########################################################
###  Starting HW2: average hourly into monthly data    ###
##########################################################

hw2 <- cbind(final_df, month(final_df$date_time),
             (year(final_df$date_time)))

colnames(hw2) <- c('datetime', 'well', 'month', 'year')

#View(hw2)

hw2$MonYear <- do.call(paste, c(hw2[c("year", "month")], sep = "-")) 
#View(hw2)

hw2_agg <-aggregate(well ~ MonYear, hw2, mean)

#View(hw2_agg)

############################################# GRANT CODE

# Now to split into train and test split 

# The actual, working stuff!
#testlist = c(119, 129, 22, 33, 44, 11) # row #'s that need to be pulled out for the test set
testset <- tibble('MonYear'='goddamnit', 'well'=1.1)
names(testset) <- c('MonYear', 'well')


r1 <- as.tibble(hw2_agg[124, ])
r2 <- as.tibble(hw2_agg[125, ])
r3 <- as.tibble(hw2_agg[126, ])
r4 <- as.tibble(hw2_agg[127, ])
r5 <- as.tibble(hw2_agg[128, ])
r6 <- as.tibble(hw2_agg[129, ])

testset <- r1
testset <- rbind(testset, r2)
testset <- rbind(testset, r3)
testset <- rbind(testset, r4)
testset <- rbind(testset, r5)
testset <- rbind(testset, r6)

testset <- testset[order(testset$MonYear),]

trainset <- hw2_agg[-c(124:129), ]

testset # this our test (holdout) set
trainset # this is our training set with the test rows removed   ##THIS NEEDS TO BE SORTED

## Sorts the training set ##
test <- grepl(".*-[0-9][0-9]$", trainset$MonYear)

#test2=c(1:123)
for (i in 1:123) {
  if (!test[i]) {
    trainset$MonYear[i] <- paste(substr(trainset$MonYear[i],1,4), "-0", substr(trainset$MonYear[i], 6, 6), sep="")
  }
  #else {
  #  trainset$MonYear[i] <- trainset$MonYear[i]
  #}
}

#hw2_agg2 <- cbind(hw2_agg, test2)
trainset <- trainset[order(trainset$MonYear),]

################################################################### END OF GRANT CODE 
#to come later....
#Creation of Time Series Data Object

df <- ts(trainset$well, start = c(2007,10), frequency = 12)     

# Time Series Decomposition ...STL# #STL=Seasonal, Trend, Low S

decomp_stl <- stl(df, s.window = 7, na.action = na.approx) 

class(df)
#Well= time series object, 

#Plot Decomposition

plot(decomp_stl)
plot.ts(df, xlab = "Year", ylab = "Depth (Ft)")
plot(df, xlab = "Year", ylab = "Depth (Ft)")
abline(v = seq(2008,2018), col = "red", lty = "dashed")

#Plotting the Trend/Cycle over the actual Values of Well Depth
plot(df, col = "grey", main = "Well Depth - Trend over Actual Values", xlab = "Year", ylab = "Depth (Feet) ", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)#plotting the trend line on the time series data

#Plotting Seasonally Adjusted water values
well_pass <- df-decomp_stl$time.series[,1]
plot(df, col = "grey", main = "Well Depth - Seasonally Adjusted", xlab = "", ylab = "Well Depth", lwd = 2)
lines(well_pass, col = "red", lwd = 2)
################################ ESM Models ##########################

## Holt-Winters Additive ##
#Model and Summary
HWES.welldepth <- hw(df, seasonal = "additive", h=6)
summary(HWES.welldepth)

#Basic Plot
plot(HWES.welldepth, main = "Well G_561_T water depth with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Depth (Ft)")
abline(v = 2018, col = "red", lty = "dashed")


#Calulate MAPE
HWES.test.results=forecast(HWES.welldepth, h=6)
error=testset$well-HWES.test.results$mean
HWES_MAPE=mean(abs(error)/abs(testset$well))   ##Model Accuracy (MAPE) = 120%
#######################################################################

## Single Exponential Smoothing ##
SES.welldepth <- ses(df, initial = "optimal", h = 6)
summary(SES.welldepth)

plot(SES.welldepth, main = "Well G_561_T water depth with Simple ESM Forecast", xlab = "Date", ylab = "Depth (Ft)")
abline(v = 2018, col = "red", lty = "dashed")

SES.test.results=forecast(SES.welldepth, h=6)
error=testset$well-SES.test.results$mean
SES_MAPE=mean(abs(error)/abs(testset$well))   ##Model Accuracy = 237%


#######################################################################

# Building a Linear/Holt Exponential Smoothing Model#
LES.welldepth <- holt(df, initial = "optimal", h = 6)
summary(LES.welldepth)

plot(LES.welldepth, main = "Well Water Depth w/ Linear Exponential Smoothing", xlab = "Date", ylab = "Depth (Units)")
abline(v = 2018, col = "red", lty = "dashed")

LES.test.results=forecast(LES.welldepth, h=6)
error=testset$well-LES.test.results$mean
LES_MAPE=mean(abs(error)/abs(testset$well))   ##Model Accuracy = 236%

#######################################################################

## Holt Damped ##
LDES.welldepth <- holt(df, initial = "optimal", h = 6, damped = TRUE)
summary(LDES.welldepth)

plot(LDES.welldepth, main = "Well G_561_T water depth with Damped Holt ESM Forecast", xlab = "Date", ylab = "Depth (Ft)")
abline(v = 2018, col = "red", lty = "dashed")

LDES.test.results=forecast(LDES.welldepth, h=6)
error=testset$well-LDES.test.results$mean
LDES_MAPE=mean(abs(error)/abs(testset$well))   
LDES_MAPE ##Model Accuracy = 239%


#######################################################################

## Holt-Winters Multiplicative ##
#Model and Summary
HWES_Mult.welldepth <- hw(df+1, seasonal = "multiplicative", h=6) 
# transformed by adding constant of 1 for successful exectuion
# otherwise, get error value "Inappropriate model for data with negative or zero values".
# We should discard this model for this reason
# I'm still curious on the results but didn't have the time to identify which components of the list output
# needed be corrected by -1 and/or discarded entirely.


# reverting some of the values back to original water levels
# WARNING: PROBABLY NOT ALL NECESSARY VALUES WER REVERTED
# I'm don't trust this model enough to use it.
HWES_Mult.welldepth$mean <- HWES_Mult.welldepth$mean-1
HWES_Mult.welldepth$x <- HWES_Mult.welldepth$x-1
HWES_Mult.welldepth$upper <- HWES_Mult.welldepth$upper-1
HWES_Mult.welldepth$lower <- HWES_Mult.welldepth$lower-1
HWES_Mult.welldepth$fitted <- HWES_Mult.welldepth$fitted-1
summary(HWES_Mult.welldepth) #summary looks decent

# plotting
plot(HWES_Mult.welldepth, 
     main = "Well G_561_T water depth \n with Holt Winters Multiplicative Model", 
     xlab = "Date", ylab = "Depth (Ft)")
abline(v = 2018, col = "red", lty = "dashed")
abline(v = seq(2008,2017), col = "gray", lty = "dashed")

# adding actual and additive predictions for comparison
actual <- ts(testset$well, start=c(2018,1), frequency=12)
predictedHWES <- ts(HWES.welldepth$mean, start=c(2018,1), frequency=12)
lines(actual, col="red")
lines(predictedHWES, col='green') #prediction from additive HWES model

HWES_Mult.test.results=forecast(HWES_Mult.welldepth, h=6)
error=(testset$well)-(HWES_Mult.test.results$mean)
HWES_Mult_MAPE=mean(abs(error)/abs(testset$well))   
HWES_Mult_MAPE

##################################   Plot best model forecast with actual testset   ########################################################
predictedHWES <- ts(HWES.welldepth$mean, start=c(2018,1), frequency=12)
predictedLES <- ts(LES.welldepth$mean, start=c(2018,1), frequency=12)
actual <<- ts(testset$well, start=c(2018,1), frequency=12)
all_data <<- ts(hw2_agg$well, start=c(2007, 10), frequency=12) #declared as global variable for use in plotting functions later

#plotting the HWES Model Forcast with actual testset
plot(all_data, main = 'Holt-Winters ESM Forecast', ylab = "Depth (Ft)")
lines(predictedHWES, col="red")
abline(v = seq(2008,2017), col = "gray", lty = "dashed")
abline(v = 2018, col = "red", lty = "dashed")

#plotting the LES Model Forcast with actual testset
plot(all_data, main = 'Linear/Holt ESM Forecast')
lines(predictedLES, col="red")
predictedHWES <- ts(HWES.welldepth$mean, start=c(2018,1), frequency=12)
abline(v = seq(2008,2017), col = "gray", lty = "dashed")
abline(v = 2018, col = "red", lty = "dashed")

# Some example plotting code
#autoplot(all_data, ts.geom = 'ribbon', ts.colour = 'purple', ts.linetype = 'dashed')
#autoplot(all_data, ts.geom = 'point', shape = 3, ts.colour = 'red') # don't use this
# Some example plotting code

#requires the ggfortify package I added at the top
#possible graph format. I can't get the vertical lines to show up, though. Let me know if we want to use this style instead of the default.
auto <- autoplot(HWES.welldepth, ts.alpha = 0.5, predict.colour = 'red') + labs(title = "Linear/Holt ESM Forecast", x= "Year", y = 'Depth')
auto + geom_vline(xintercept = as.Date("2018-01-01"))
auto
#Different Holt-Winters, with intervals
plot(HWES.welldepth, main = "Well G-561-T Water Depth \nHolt-Winters ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (Ft)")
abline(v = seq(2008,2017), col = "gray", lty = "dashed")
abline(v = 2018, col = "red", lty = "dashed")
lines(actual, col='red')


####################################### SUMMARY TABLE

#Collecting models into a list
# Only lapply with a list worked. Other apply/vector combos didn't work 
fmodels <- list(HWES_Mult.welldepth,
                HWES.welldepth,
                SES.welldepth,
                LDES.welldepth,
                LES.welldepth)


f.summarize <- function(forecast.model){
  # A summarizing function to organize and compare all our models
  
  errors <- accuracy(forecast.model)
  mlabel <- forecast.model$method
  
  #MAPE of test set
  test.results=forecast(forecast.model, h=6)
  t.error=testset$well-test.results$mean
  forecast.MAPE=100*mean(abs(t.error)/abs(testset$well))
  
  #Calling our personal plot function
  f.plot(forecast.model, mlabel)
  
  results <- cbind(errors, mlabel, forecast.MAPE)
  return(results)
}

f.plot <- function(forecast.model, mlabel){
  #standardized plots
  #all_data declared as global variable above for use in these functions
  
  #Plot of all available data and model
  png(paste(mlabel,'.png'), width=500, height=400)
  plot(all_data,
       main = paste("G-561 Water Elev\n(",mlabel,")"), ylab='Elev (Ft)', xlab='Date',
       xlim=c(2007.5,2018.5))
  lines(forecast.model$mean, col="blue", lwd=2)
  grid(col = "gray80", lty = "dotted", lwd = par("lwd"), equilogs = TRUE) # added gridlines
  abline(v = 2018, col = "red", lty = "dashed")
  dev.off()
  
  #Plot of ONLY 2018 with predicted and actual results
  png(paste('2018',mlabel,'.png'), width=500, height=400)
  plot(actual,
       main = paste("G-561 Water Elev\n(",mlabel,")"), ylab='Elev (Ft)', xlab='Date',
       xlim=c(2018,2018.6)
  )
  lines(forecast.model$mean, col="blue", lwd=2)
  grid(col = "gray80", lty = "dotted", lwd = par("lwd"), equilogs = TRUE) # added gridlines
  abline(v = 2018, col = "red", lty = "dashed")
  legend("bottomright", cex=0.8, legend=c("Prediction", "Observed", "Training Boundary"), 
         col=c("blue","black", "red"), lwd = c(2, 1, 1), lty = c(1, 1, 2))
  dev.off()
  
}

#COMMENTED OUT LINES BELOW TO AVOID RECREATING CSV OVER AND OVER
#apply the summarizing funciton to each model, then reformat results into clean dataframe
#fmodels.summ <- lapply(fmodels, f.summarize)
#fmodels.final <- as.data.frame(do.call(rbind, fmodels.summ)) #do.call is a wierd function that I still don't fully understand

#review of final forecast model summary and working directory prior to saving results
#COMMENTED OUT LINES BELOW TO AVOID RECREATING CSV OVER AND OVER
#View(fmodels.final)
#getwd()
#write.csv(fmodels.final, file="forecast_models.csv")


#cleaning up environment
ts_train <- df
ts_val <- actual
well_ts <- all_data
rm(list=setdiff(ls(),c("HWES.welldepth", "well_ts", "ts_val", "ts_train", "f.plot", "f.summarize")))


# if there's another vairable/df/model we want ot pass along to the next HW, just include it in the save() function below
setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW2\\HW2-Repo\\TS_HW2\\")
save(HWES.welldepth, well_ts, ts_val, ts_train, f.plot, f.summarize, file="HW2.RData")
