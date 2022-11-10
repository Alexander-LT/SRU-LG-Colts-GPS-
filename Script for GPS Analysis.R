
# Deployment ####
install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='alextitley',
                          token='DC7928C72E56FF825A46CE06953BF4CA',
                          secret='Y+fBjcW6KXIyQCJRaKVvps1ndZdbQSv0u8pBqrXH')


rsconnect::deployApp('\GitHub\SRU-LG-Colts-GPS-')


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
RawData$Date <- dmy(RawData$Date)
RawData$Date <- format(as.Date(RawData$Date),'%d/%m/%Y')# This turns the date column into date format
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
  select(Date, SessionDate, Session.Title, Tags, Split.Name, Player.Name, Distance..metres., Distance.Per.Min..m.min., Top.Speed..m.s.,
         Sprint.Distance..m., Duration, Distance.in.Speed.Zone.3...metres., 
         Distance.in.Speed.Zone.4...metres., Distance.in.Speed.Zone.5...metres.) %>% 
  rename(Athlete = Player.Name, 
         Distance = Distance..metres., 
         TopSpeed = Top.Speed..m.s., 
         MetresPerMin = Distance.Per.Min..m.min., 
         Tag = Tags, SessionTitle = Session.Title,
         SpintDistance = Sprint.Distance..m.) %>% 
  mutate(DurationMins = round(Duration / 60, 1),  
         HSR = round(Distance.in.Speed.Zone.3...metres. + Distance.in.Speed.Zone.4...metres. + Distance.in.Speed.Zone.5...metres., 1),
         Distance = round(Distance, 1), 
         MetresPerMin = round(MetresPerMin, 1), 
         TopSpeed = round(TopSpeed, 1),
         SpintDistance = round(SpintDistance, 1)) %>% 
  select(-c(Duration, Distance.in.Speed.Zone.3...metres., 
            Distance.in.Speed.Zone.4...metres. , 
            Distance.in.Speed.Zone.5...metres.,
            )) %>% 
  na.omit() 
  

# Pivot the table long 
RawData_Long <- RawData_Tidy %>% 
  pivot_longer(cols = c('Distance':'HSR'), names_to = 'Variable', values_to = "Value") %>% 
  arrange(Date) %>% 
  filter(Split.Name == "all" | Split.Name == "game")

class(RawData_Long$Date)

# Gather all athlete names in the dataset so that can deidentify names 
RawData_Long$Athlete %>% 
  unique()

RawData_Long <- RawData_Long %>% 
  mutate(Athlete = case_when(
    Athlete == "Luca Rossi" ~ 'Athlete1',
    Athlete == "Joey Ryan" ~ 'Athlete2',
    Athlete == "Charlie Moses" ~ 'Athlete3',
    Athlete == "Luca Antico" ~ 'Athlete3',
    Athlete == "Leo Ferguson" ~ 'Athlete4',
    Athlete == "Dylan Hansen" ~ 'Athlete5',
    Athlete == "Jack Ricci" ~ 'Athlete6',
    Athlete == "Carter Dombkins" ~ 'Athlete7',
    Athlete == "Ollie Little" ~ 'Athlete8',
    Athlete == "Henry Hyde" ~ 'Athlete9',
    Athlete == "Harvey McGregor" ~ 'Athlete10',
    Athlete == "Connor Bond" ~ 'Athlete11',
    Athlete == "Callan Oeding" ~ 'Athlete12',
    Athlete == "Andrew Blackburn" ~ 'Athlete13',
    Athlete == "Ben Wallace" ~ 'Athlete14',
    Athlete == "Jack Tougher-Wells" ~ 'Athlete15',
    Athlete == "Ben Falla" ~ 'Athlete16',
    Athlete == "Digby Lilburne" ~ 'Athlete17',
    Athlete == "Tyson Cogan" ~ 'Athlete18',
    Athlete == "Connor McMullen" ~ 'Athlete19',
    Athlete == "Liam Sartena" ~ 'Athlete20',
    Athlete == "Ben Adams" ~ 'Athlete21',
    Athlete == "Vereniki Duarara" ~ 'Athlete22',
    Athlete == "Jonah O Sullivan" ~ 'Athlete23',
    Athlete == "Ezrome Iosefa" ~ 'Athlete24',
    Athlete == "Lisate Tupou" ~ 'Athlete25',
    Athlete == "Darcy Mulrooney" ~ 'Athlete26',
    TRUE ~ Athlete))


