
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
library(monochromeR)

# Real Section ####
RawData <- read.csv("Data/2022SeasonGPS.csv")
# Format dates 
RawData$Date <- dmy(RawData$Date)
#RawData$Date <- format(as.Date(RawData$Date),'%d/%m/%Y')
# Unite session title and date
RawData <- RawData %>%
  unite(SessionDate, "Date":"Session.Title", sep = " - ", remove = FALSE)


# Tidy data 
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
  select(-c(Duration, Distance.in.Speed.Zone.3...metres., Distance.in.Speed.Zone.4...metres. , Distance.in.Speed.Zone.5...metres.)) %>% 
  na.omit() 

# Pivot the table long 
RawData_Long <- RawData_Tidy %>% 
  pivot_longer(cols = c('Distance':'HSR'), names_to = 'Variable', values_to = "Value") %>% 
  arrange(Date) %>% 
  filter(Split.Name == "all" | Split.Name == "game")

# Create Test block so the graphs look less congested with many days
# Added in Position Groups 
testdata <- RawData_Long %>% 
  filter(Date == "2022-08-28" | Date == "2022-08-30" | Date == '2022-08-31' | Date == '2022-09-03') %>% 
  mutate(PositionGroup = case_when(
    Athlete == "Luca Rossi" ~ 'Front Row',
    Athlete == "Joey Ryan" ~ 'Halves',
    Athlete == "Charlie Moses" ~ 'Front Row',
    Athlete == "Luca Antico" ~ 'Halves',
    Athlete == "Leo Ferguson" ~ 'Back Row',
    Athlete == "Dylan Hansen" ~ 'Back Row',
    Athlete == "Jack Ricci" ~ 'Back Three',
    Athlete == "Carter Dombkins" ~ 'Front Row',
    Athlete == "Ollie Little" ~ 'Back Three',
    Athlete == "Henry Hyde" ~ 'Halves',
    Athlete == "Harvey McGregor" ~ 'Front Row',
    Athlete == "Connor Bond" ~ 'Back Three',
    Athlete == "Callan Oeding" ~ 'Back Three',
    Athlete == "Andrew Blackburn" ~ 'Center',
    Athlete == "Ben Wallace" ~ 'Front Row',
    Athlete == "Jack Tougher-Wells" ~ 'Back Row',
    Athlete == "Ben Falla" ~ 'Front Row',
    Athlete == "Digby Lilburne" ~ 'Second Row',
    Athlete == "Tyson Cogan" ~ 'Center',
    Athlete == "Connor McMullen" ~ 'Halves',
    Athlete == "Liam Sartena" ~ 'Second Row',
    Athlete == "Ben Adams" ~ 'Halves',
    Athlete == "Vereniki Duarara" ~ 'Back Three',
    Athlete == "Jonah O Sullivan" ~ 'Halves',
    Athlete == "Ezrome Iosefa" ~ 'Unknown',
    Athlete == "Lisate Tupou" ~ 'Center',
    Athlete == "Darcy Mulrooney" ~ 'Front Row'))

# Create a df for just games over the whole season 
testdataGame <- RawData_Long %>% 
  filter(Split.Name == "game") %>% 
  mutate(PositionGroup = case_when(
    Athlete == "Luca Rossi" ~ 'Front Row',
    Athlete == "Joey Ryan" ~ 'Halves',
    Athlete == "Charlie Moses" ~ 'Front Row',
    Athlete == "Luca Antico" ~ 'Halves',
    Athlete == "Leo Ferguson" ~ 'Back Row',
    Athlete == "Dylan Hansen" ~ 'Back Row',
    Athlete == "Jack Ricci" ~ 'Back Three',
    Athlete == "Carter Dombkins" ~ 'Front Row',
    Athlete == "Ollie Little" ~ 'Back Three',
    Athlete == "Henry Hyde" ~ 'Halves',
    Athlete == "Harvey McGregor" ~ 'Front Row',
    Athlete == "Connor Bond" ~ 'Back Three',
    Athlete == "Callan Oeding" ~ 'Back Three',
    Athlete == "Andrew Blackburn" ~ 'Center',
    Athlete == "Ben Wallace" ~ 'Front Row',
    Athlete == "Jack Tougher-Wells" ~ 'Back Row',
    Athlete == "Ben Falla" ~ 'Front Row',
    Athlete == "Digby Lilburne" ~ 'Second Row',
    Athlete == "Tyson Cogan" ~ 'Center',
    Athlete == "Connor McMullen" ~ 'Halves',
    Athlete == "Liam Sartena" ~ 'Second Row',
    Athlete == "Ben Adams" ~ 'Halves',
    Athlete == "Vereniki Duarara" ~ 'Back Three',
    Athlete == "Jonah O Sullivan" ~ 'Halves',
    Athlete == "Ezrome Iosefa" ~ 'Unknown',
    Athlete == "Lisate Tupou" ~ 'Center',
    Athlete == "Darcy Mulrooney" ~ 'Front Row'))

# Play with plots now 

# generate palettes for plot texts 
dark_text <- "#1A242F"

generate_palette(
  dark_text,
  "go_lighter",
  n_colours = 5,
  view_palette = TRUE
)

mid_text <- generate_palette(
  dark_text, "go_lighter",
  n_colours = 5) [2]

light_text <- generate_palette(
  dark_text, "go_lighter",
  n_colours = 5) [3]

# Text theme 
basic_plot <- ggplot(testdata, aes(x = Date, y = Value)) +
  geom_point() +
  theme_minimal() +
  theme(
    text = element_text(colour = mid_text,
                    family = "BrandonText"),
plot.title = element_text(colour = dark_text,
                          size = rel(1.6)),
plot.subtitle = element_text(size = rel(1.1)),
axis.text.y = element_text(colour = light_text,
                           size = rel(0.8)),
axis.title.y = element_text(size = 12),
axis.text.x = element_text(colour = mid_text,
                           size = 12),
axis.title.x = element_blank(),
panel.grid = element_line(colour = "#F3F4F5"),
legend.position = 'top',
legend.justification = 1,
plot.caption = element_text(size = rel(0.8),
                            margin = margin(8, 0, 0, 0)),
plot.margin = margin(0.25, 0.25, 0.25, 0.25, 'cm')
)

basic_plot

# Package the theme up 

theme_rladiesdemo <- function(base_size = 12,
                              dark_text = "#1A242F") {
  mid_text <- generate_palette(
    dark_text, "go_lighter",
    n_colours = 5) [2]
  
  light_text <- generate_palette(
    dark_text, "go_lighter",
    n_colours = 5) [3]
  
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = mid_text,
                          family = "BrandonText",
                          lineheight = 1.1),
      plot.title = element_text(colour = dark_text,
                                size = rel(1.6),
                                margin = margin(12, 0, 8, 0)),
      plot.subtitle = element_text(size = rel(1.1),
                                   margin = margin(4, 0, 0, 0)),
      axis.text.y = element_text(colour = light_text,
                                 size = rel(0.8)),
      axis.title.y = element_text(size = 12,
                                  margin = margin(0, 4, 0, 0)),
      axis.text.x = element_text(colour = mid_text,
                                 size = 12),
      axis.title.x = element_blank(),
      panel.grid = element_line(colour = "#F3F4F5"),
      legend.position = 'top',
      legend.justification = 1,
      plot.caption = element_text(size = rel(0.8),
                                  margin = margin(8, 0, 0, 0)),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, 'cm')
    )
}

# Change the plots 

ggplot(data = testdataGame, aes(x = Date, y = Value)) + 
  geom_line(linetype = "dashed", colour = "grey") +
  geom_jitter(aes(colour = PositionGroup), size = 4) +
  labs(x = NULL, y = NULL) +
  scale_x_date(breaks = unique(testdataGame$Date), date_labels = "%b %d") +
  theme_rladiesdemo() +
  facet_wrap(~Variable, scales = "free") +
  labs(title = 'GPS Values from a Shute Shield Colts Team.', caption = 'Data sourced during the 2022 season and deidentified',
       subtitle = ' From the data we have the total distance ran, Duration the athlete was on the field, High-speed running distance, Intensity or metres per minute,
       Sprint distance of each athlete, and the top speed achieved by each athlete') +
  scale_colour_manual(
    values = colorRampPalette(my_anchor_colours)(23))

# Test out colour schemes 

my_anchor_colours <- c("red", 'blue', 'black')

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
  pivot_longer(cols = c('Distance':'HSR'), names_to = 'Variable', values_to = "Value")



  filter(Split.Name == 'game' | Split.Name == 'training')

#### DUMMY 

df=data.frame(x=c(12,31,10,2,99),
              y=c(22.1,44.5,6.1,10,99),
              z=c("Apple","Guava", "Mango", "Apple","Mango"))

df %>%
  filter(z %in% c("Apple", "Mango"))



# Create a block of test dates so that we can make better visualisations without the clutter of the whole season 
TestDates <- c('2022-09-03', '2022-08-31', '2022-08-30', '2022-08-28')

RawDataLongTest <- RawData_Long %>% 
  filter(Tag %in% c(' training drill', 'game'))


RawDataLongTest <- RawData_Long %>% 
  filter(Date %in% c('2022-09-03', '2022-08-31', '2022-08-30', '2022-08-28'))

library(dplyr)
target <- c("Tom", "Lynn")
filter(dat, name %in% target)

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

# Change the plots 

ggplot(data = RawData_Long, aes(x = Date, y = Value)) + 
  geom_line(linetype = "dashed", colour = "grey") +
  geom_point(aes(colour = Tag), size = 4) +
  labs(x = NULL, y = NULL) +
  scale_x_date(breaks = unique(RawDataLongTest$Date), date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"), 
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"), 
        legend.position = "bottom", 
        legend.title = element_blank()) +
  facet_wrap(~Variable, scales = "free")


# Have a real go 