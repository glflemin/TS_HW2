#Library to manipulate dates and excel and dataframes

#if you don't have any of these, install them

library(readxl)

library(lubridate)

library(dplyr)

library(zoo)

# for cleaning global environment

rm(list=ls())

# ensuring correct work directory

#this.dir <- dirname(parent.frame(2)$ofile)

#setwd(this.dir)



#setwd('C:\\Users\\gavin\\Desktop\\Time_Series_Data\\')
#setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\HW1\\Homework-1\\")

setwd ('C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\')

# importing the Excel file

wbpath <- "C:\\Users\\molly\\OneDrive\\Documents\\R\\data\\G-561_T.xlsx"
#wbpath <- "C:\\Users\\gavin\\Desktop\\Time_Series_Data\\G-561_T.xlsx"



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





#grouping water levels by taking average of each hour

clean_well <- G_561_T %>%
  
  group_by(date_time) %>%
  
  summarise(mean_corr=mean(Corrected)) %>%
  
  select(date_time, mean_corr)



#calc avg stdev

meandepth = mean(clean_well$mean_corr, na.rm=TRUE)

stdevdepth = sd(clean_well$mean_corr, na.rm=TRUE)



#THE MERGE

final_df <- left_join(vdate, clean_well, by='date_time')

rm(list=setdiff(ls(), "final_df"))

str(final_df)



#Creation of Time Series Data Object

df <- ts(final_df$mean_corr, start = c(2007,10,5,0), frequency = 8760)



# Time Series Decomposition ...STL# #STL=Seasonal, Trend, Low S

decomp_stl <- stl(df, s.window = 7, na.action = na.approx) 

#Depth= time series object, 

#s.window you have to have this, and it should be odd and no less than 7.  Moving average.



#Plot Decomposition

plot(decomp_stl)

plot.ts(df, xlab = "Year", ylab = "Depth (Ft)")

plot(df, xlab = "Year", ylab = "Depth (Ft)")



plot(df, col = "grey", main = "Well Depth - Trend/Cycle", xlab = "Year", ylab = "Depth (Feet) ", lwd = 2)

lines(decomp_stl$time.series[,2], col = "red", lwd = 2)#plotting the trend line on the time series data