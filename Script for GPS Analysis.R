
# Load Packages 
library(tidyverse)
library(lubridate)


# Load data 
RawData <- read.csv("Data/2022SeasonGPS.csv", header = TRUE, stringsAsFactors = TRUE)

# assess structure of the rawdata
str(RawData)

# edit the date column so that they are readable dates 
RawData$Date <- as.Date(RawData$Date, origin = '1899-12-30')
RawData$Split.Start.Time <- as.Date(RawData$Split.Start.Time, origin = '1899-12-30')
class(RawData$Date) # Check the class, make sure it is date 
RawData$Date <- as.POSIXct(as.Date(RawData$Date, origin = '1899-12-30'))
RawData$Split.Start.Time <- as.POSIXct(as.Date(RawData$Split.Start.Time, origin = '1899-12-30'))
RawData$Split.End.Time <- as.POSIXct(as.Date(RawData$Split.End.Time, origin = '1899-12-30'))
class(RawData$Split.Start.Time)
# Add a days column and a month column 
RawData$Day <- weekdays(as.Date(RawData$Date))
RawData$Month <- months(as.Date(RawData$Date))
# Split the date, split start and end columns to have time only 

RawData$Hours <- format(as.POSIXct(RawData$Date, "%D-%M-%y", tz = "", format = "%H:%M:%S"))


# Edit column names for better analysis
GPSSummary <- RawData %>% 
  select()