
# Load Packages 
library(tidyverse)
library(lubridate)
library(DT)


# Load data 
RawData <- read.csv("Data/2022SeasonGPS.csv")
                    
                    #header = TRUE, stringsAsFactors = TRUE)

# assess structure of the rawdata
str(RawData)

class(RawData$Date)
# edit the date column so that they are readable dates 
RawData$Date <- dmy(RawData$Date)
RawData$Date <- format(RawData$Date, format = "%d/%m/%y")

# Tidy the tibble 
RawData_Tidy <- RawData %>% 
  select(Date, Session.Title, Tags, Split.Name, Split.Start.Time, Split.End.Time, Player.Name, Distance..metres., Distance.Per.Min..m.min., Top.Speed..m.s.,
         Sprint.Distance..m., Duration, Distance.in.Speed.Zone.3...metres., 
         Distance.in.Speed.Zone.4...metres., Distance.in.Speed.Zone.5...metres.) %>% 
  rename(Athlete = Player.Name, Distance = Distance..metres., TopSpeed = Top.Speed..m.s., MetresPerMin = Distance.Per.Min..m.min.) %>% 
  mutate(Duration_mins = round(Duration / 60, 1),  
         HSR = round(Distance.in.Speed.Zone.3...metres. + Distance.in.Speed.Zone.4...metres. + Distance.in.Speed.Zone.5...metres., 1)) %>% 
  select(-c(Duration, Distance.in.Speed.Zone.3...metres. , Distance.in.Speed.Zone.4...metres. , Distance.in.Speed.Zone.5...metres.))

ggplot(RawData_Tidy, aes(x = Date, y = Distance, colour = Athlete)) +
  geom_point() +
  facet_wrap(~Athlete)


