
# Load Packages 
library(tidyverse)
library(readxl)
library(lubridate)

# Load data 
RawData <- read.csv("Data/2022SeasonGPS.csv", header = TRUE, stringsAsFactors = FALSE)

# assess structure of the rawdata
str(RawData)

# edit the date column so that they are readable dates 
RawData %>% 
  group_by(Player.Name) %>% 
  mutate(Date = ymd(Date))

GPSData <- RawData %>% 
  ymd(RawData$Date)
  

# Edit column names for better analysis
GPSSummary <- RawData %>% 
  select()