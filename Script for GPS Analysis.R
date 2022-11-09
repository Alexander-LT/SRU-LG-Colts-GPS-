# Section 1 ####
# Load Packages 
library(tidyverse)
library(lubridate)
library(DT)

# Section 2 ####
# Load data 
RawData <- read.csv("Data/2022SeasonGPS.csv") #header = TRUE, stringsAsFactors = TRUE) # Add this in to make each column factored

# assess structure of the rawdata
str(RawData)

class(RawData$Date)
# Section 3 ####
# Tidying 
# edit the date column so that they are readable dates 
RawData$Date <- ymd(RawData$Date) # This turns the date column into date format
# Add a month column to help with analyis
RawData$Month <- month(ymd(RawData$Date))
# RawData$Date <- format(RawData$Date, format = "%d/%m/%y") # Dehash this for character date

# Unite session title and dat e
RawData <- RawData %>%
  unite(SessionDate, "Date":"Session.Title", sep = " - ", remove = FALSE)

# Tidy the tibble 
# Select the columns we want for analysis. Rename for easier call up. Mutate new columns for new variables i.e. Raw Duration is in seconds
# so convert to mins. # remove the columns we no longer need after the mutation. 
RawData_Tidy <- RawData %>% 
  select(Date, Month, Day, SessionDate, Session.Title, Tags, Split.Name, Split.Start.Time, Split.End.Time, Player.Name, Distance..metres., Distance.Per.Min..m.min., Top.Speed..m.s.,
         Sprint.Distance..m., Duration, Distance.in.Speed.Zone.3...metres., 
         Distance.in.Speed.Zone.4...metres., Distance.in.Speed.Zone.5...metres.) %>% 
  rename(Athlete = Player.Name, 
         Distance = Distance..metres., 
         TopSpeed = Top.Speed..m.s., 
         MetresPerMin = Distance.Per.Min..m.min., 
         Tag = Tags, SessionTitle = Session.Title,
         SpintDistance = Sprint.Distance..m.) %>% 
  mutate(Duration_mins = round(Duration / 60, 1),  
         HSR = round(Distance.in.Speed.Zone.3...metres. + Distance.in.Speed.Zone.4...metres. + Distance.in.Speed.Zone.5...metres., 1),
         Distance = round(Distance, 1), 
         MetresPerMin = round(MetresPerMin, 1), 
         TopSpeed = round(TopSpeed, 1),
         SpintDistance = round(SpintDistance, 1)) %>% 
  select(-c(Duration, Distance.in.Speed.Zone.3...metres., Distance.in.Speed.Zone.4...metres. , Distance.in.Speed.Zone.5...metres.)) %>% 
  na.omit() 
  



