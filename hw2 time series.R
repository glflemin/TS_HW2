#Library to manipulate dates and excel and dataframes

#if you don't have any of these, install them

install.packages("stringi")

library(readxl)

library(lubridate)

library(dplyr)

library(zoo)

library(stringi)

library(tidyverse)

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
#setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW1\\Homework-1\\")
#setwd("C:\\Users\\Grant\Downloads\\")
#setwd ('C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\')
setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")

# importing the Excel file

#wbpath <- "C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\G-561_T.xlsx"
#wbpath <- "C:\\Users\\gavin\\Desktop\\Time_Series_Data\\G-561_T.xlsx"
wbpath <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework\\G-561_T.xlsx"
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

vdate

# Rounding dates to their hour, Frankensteining date & hour together, then converting to POSIX

# Assumes all times are EST

# TODO: Correct EST assumption for timestamps in EDT 

G_561_T$date_time <- paste(G_561_T$date," ", lubridate::hour(G_561_T$time),":00:00", sep = "")

G_561_T$date_time <- as.POSIXct(G_561_T$date_time, tz="EST")


View(G_561_T)


#grouping water levels by taking average of each hour

clean_well <- G_561_T %>%
  
  group_by(date_time) %>%
  
  summarise(mean_corr=mean(Corrected)) %>%
  
  select(date_time, mean_corr)

View(clean_well)

#calc avg stdev

meandepth = mean(clean_well$mean_corr, na.rm=TRUE)

stdevdepth = sd(clean_well$mean_corr, na.rm=TRUE)



#THE MERGE

final_df <- left_join(vdate, clean_well, by='date_time')

rm(list=setdiff(ls(), "final_df"))

str(final_df)

View(final_df)


##########################################################
###  Starting HW2: average hourly into monthly data    ###
##########################################################

hw2 <- cbind(final_df, month(final_df$date_time),
             (year(final_df$date_time)))

colnames(hw2) <- c('datetime', 'well', 'month', 'year')

View(hw2)

hw2$MonYear <- do.call(paste, c(hw2[c("year", "month")], sep = "-")) 
View(hw2)

hw2_agg <-aggregate(well ~ MonYear, hw2, mean)

View(hw2_agg)

############################################# GRANT CODE

# Now to split into train and test split 

# The actual, working stuff!
testlist = c(119, 129, 22, 33, 44, 11) # row #'s that need to be pulled out for the test set
testset <- tibble('MonYear'='goddamnit', 'well'=1.1)
names(testset) <- c('MonYear', 'well')


r1 <- as.tibble(hw2_agg[124, ])
r2 <- as.tibble(hw2_agg[125, ])
r3 <- as.tibble(hw2_agg[126, ])
r4 <- as.tibble(hw2_agg[127, ])
r5 <- as.tibble(hw2_agg[128, ])
r6 <- as.tibble(hw2_agg[129, ])

testset <- rbind(testset, r1)
testset <- rbind(testset, r2)
testset <- rbind(testset, r3)
testset <- rbind(testset, r4)
testset <- rbind(testset, r5)
testset <- rbind(testset, r6)

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


############################################# JUNK CODE
hw2_test <-  gsub("-", ".", hw2_agg$MonYear)
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
test<- strReverse(hw2_agg$MonYear)
############################################# JUNK CODE 


################################################################### END OF GRANT CODE 
#to come later....
#Creation of Time Series Data Object

df <- ts(trainset$well, start = 2007, frequency = 12)       ### start value is not correct


# Time Series Decomposition ...STL# #STL=Seasonal, Trend, Low S

decomp_stl <- stl(df, s.window = 7, na.action = na.approx) 


#Well= time series object, 

#Plot Decomposition

plot(decomp_stl)
plot.ts(df, xlab = "Year", ylab = "Depth (Ft)")
plot(df, xlab = "Year", ylab = "Depth (Ft)")


#Plotting the Trend/Cycle over the actual Values of Well Depth
plot(df, col = "grey", main = "Well Depth - Trend/Cycle", xlab = "Year", ylab = "Depth (Feet) ", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)#plotting the trend line on the time series data

#Plotting Seasonally Adjusted water values
well_pass <- df-decomp_stl$time.series[,1]
plot(df, col = "grey", main = "Well Depth - Seasonally Adjusted", xlab = "", ylab = "Well Depth", lwd = 2)
lines(well_pass, col = "red", lwd = 2)
################################ ESM Models ##########################

## Holt-Winters Additive
HWES.welldepth <- hw(df, seasonal = "additive")
summary(HWES.welldepth)

plot(HWES.welldepth, main = "Well G_561_T water depth with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Depth (units)")
abline(v = 2008.25, col = "red", lty = "dashed")