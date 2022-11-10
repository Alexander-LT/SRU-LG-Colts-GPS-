

# Load Packages 
library(shiny)
library(lubridate)
library(tidyverse)
library(viridis)
library(plotly)
library(bslib)
library(DT)
library(rsconnect)


# Load data
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
    mutate(Duration_mins = round(Duration / 60, 1),  
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
    filter(Split.Name == "all" | Split.Name == "game") %>% # Remove a lot of session tags. 
    mutate(Athlete = case_when(    # Deidentify players 
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


    

# Define UI for application - can change title below, to add in team name
ui <- navbarPage("Example GPS Report",
                 # Add navbar - can change name (to "By Athlete", for example)
                 tabPanel("Athlete Analysis", 
                          # Create a dropdown selection for individual athlete analysis
                          selectInput("AthleteName", label = h4("Select Athlete:"), 
                                      choices = unique(RawData_Long$Athlete), 
                                      selected = 1),
                          # Plot output (code below in server)
                          mainPanel(plotOutput("plot1"), width = "100%", height = "100%")),
                 # Repeat for session/ by date analysis
                 tabPanel("Session Analysis", 
                          selectInput("SessionDate1", label = h3("Select Session Date:"), 
                                      choices = unique(RawData_Long$SessionDate), 
                                      selected = 1),
                          # Create plot output
                          mainPanel(plotOutput("plot2"), width = "100%", height = "100%")), 
                 # Repeat for session/ by date analysis (interactive)
                 tabPanel("Session Analysis (Interactive)", 
                          selectInput("SessionDate2", label = h3("Select Session Date:"), 
                                      choices = unique(RawData_Long$SessionDate), 
                                      selected = 1),
                          # Create plot output
                          mainPanel(plotlyOutput("plot3"), width = "100%", height = "100%")), 
                 # Repeat for session type
                 tabPanel("Session Type Analysis (Interactive)", 
                          selectInput("SessionType", label = h3("Select Session Type:"), 
                                      choices = unique(RawData_Long$Split.Name), 
                                      selected = 1),
                          # Create plot output
                          mainPanel(plotlyOutput("plot4"), width = "100%", height = "100%")), 
                 # Present all raw data in a table
                 tabPanel("All Data", 
                          DT::dataTableOutput("AllDataTable")),
                 theme = bs_theme(version = 4, bootswatch = "cosmo"))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot({
        SelectedAthlete <- RawData_Long[RawData_Long$Athlete==input$AthleteName,] 
        ggplot(data = SelectedAthlete, aes(x = Date, y = Value)) + 
            geom_line(linetype = "dashed", colour = "grey") +
            geom_point(aes(colour = Split.Name), size = 4) +
            labs(x = NULL, y = NULL) +
            scale_x_date(breaks = unique(SelectedAthlete$Date), date_labels = "%b %d") +
            scale_colour_viridis_d() +
            theme_classic() +
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1, colour = "black"),
                  axis.text.y = element_text(size = 10, colour = "black"), 
                  strip.text.x = element_text(size = 12, face = "bold"),
                  panel.spacing = unit(2, "lines"), 
                  legend.position = "bottom", 
                  legend.title = element_blank()) +
            facet_wrap(~Variable, scales = "free")
    }, height = 900)
    
    output$plot2 <- renderPlot({
        SelectedDate <- RawData_Long[RawData_Long$SessionDate==input$SessionDate1,] 
        ggplot(data = SelectedDate, aes(x = Variable, y = Value)) + 
            geom_boxplot(alpha = 0.7, colour = "lightgrey", outlier.shape = NA) +
            geom_jitter(aes(colour = Athlete), size = 4) +
            labs(x = NULL, y = NULL) +
            scale_colour_viridis_d(option = "plasma") +
            theme_classic() +
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 10, colour = "black"), 
                  strip.text.x = element_text(size = 12, face = "bold"),
                  panel.spacing = unit(2, "lines"), 
                  legend.position = "bottom", 
                  legend.title = element_blank()) +
            facet_wrap(~Variable, scales = "free")
    }, height = 900)
    
    output$plot3 <- renderPlotly({
        SelectedDate2 <- RawData_Long[RawData_Long$SessionDate==input$SessionDate2,] 
        print(
            ggplotly(
                ggplot(data = SelectedDate2, aes(x = Variable, y = Value)) + 
                    geom_boxplot(alpha = 0.7, colour = "lightgrey", outlier.shape = NA) +
                    geom_jitter(aes(colour = Athlete), size = 4) +
                    labs(x = NULL, y = NULL) +
                    scale_colour_viridis_d(option = "plasma") +
                    theme_classic() +
                    theme(axis.title.x = element_blank(), 
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(size = 10, colour = "black"), 
                          strip.text.x = element_text(size = 12, face = "bold"),
                          panel.spacing = unit(2, "lines"), 
                          legend.position = "bottom", 
                          legend.title = element_blank()) +
                    facet_wrap(~Variable, scales = "free")) %>% layout(height = 1200, width = 2000))
    })
    
    output$plot4 <- renderPlotly({
        SelectedSessionType <- RawData_Long[RawData_Long$Split.Name==input$SessionType,] 
        print(
            ggplotly(
                ggplot(data = SelectedSessionType, aes(x = Variable, y = Value, label = Date)) + 
                    geom_violin(alpha = 0.7) +
                    stat_summary(fun=median, geom="crossbar", size=2, color="darkgrey") +
                    geom_jitter(aes(colour = Athlete), size = 4) +
                    labs(x = NULL, y = NULL) +
                    scale_colour_viridis_d(option = "plasma") +
                    theme_classic() +
                    theme(axis.title.x = element_blank(), 
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(size = 10, colour = "black"), 
                          strip.text.x = element_text(size = 12, face = "bold"),
                          panel.spacing = unit(2, "lines"), 
                          legend.position = "bottom", 
                          legend.title = element_blank()) +
                    facet_wrap(~Variable, scales = "free")) %>% layout(height = 1200, width = 2000))
    })
    
    output$AllDataTable = DT::renderDataTable({
        RawData_Tidy 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
