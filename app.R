
# Read Data ---------------------------------------------------------------

getwd()
setwd("/Users/tilmannhogrefe/Desktop/R/R Kurs/Datasets")

library(tidyverse)
library(readxl)
library(dplyr)

d_ACLED_2018_bis_2024 <- read_excel("Ukraine_Black_Sea_2020_2024_Feb23.xlsx")

#Quelle: https://acleddata.com/ukraine-conflict-monitor/
#Methodology of Source: https://acleddata.com/knowledge-base/acled-methodology-and-coding-decisions-around-conflict-in-ukraine/
#Anmerkung: This file contains all political violence events, demonstration events, and strategic developments recorded in Ukraine and the Black Sea from the beginning of ACLED coverage in 2018 to the present.

d_humandata.org_2014_bis_2021 <- read_csv("conflict_data_ukr.csv")

# Daten bearbeiten --------------------------------------------------------

#Die gleichen Spalten auswählen

d_ACLED_bearbeitet <- d_ACLED_2018_bis_2024 %>%
  select(EVENT_DATE, LATITUDE, LONGITUDE, FATALITIES, SUB_EVENT_TYPE, CIVILIAN_TARGETING)

d_humandata_bearbeitet <- d_humandata.org_2014_bis_2021 %>%
  select(latitude, longitude, date_start, high) 

# Jetztz erstelle ich zwei neue Spalten im ACLED Datensatz

d_ACLED_bearbeitet <- d_ACLED_bearbeitet %>%
  mutate(Armed_Clash = ifelse(SUB_EVENT_TYPE %in% c("Armed Clash", "Attack"), SUB_EVENT_TYPE, NA),
         Artillery = ifelse(SUB_EVENT_TYPE %in% c("Shelling/artillery/missile attack", "Air/drone strike"), SUB_EVENT_TYPE, NA))

# ... Und entferne die alte Spalte

d_ACLED_bearbeitet <- d_ACLED_bearbeitet %>%
  select(-SUB_EVENT_TYPE) 

#Spalten umbenennen

d_humandata_bearbeitet <- d_humandata_bearbeitet %>%
  rename(LATITUDE = latitude, LONGITUDE = longitude, EVENT_DATE = date_start, FATALITIES = high)

d_ACLED_bearbeitet <- d_ACLED_bearbeitet %>%
  rename(ARMED_CLASH = Armed_Clash, ARTILLERY = Artillery)

#Spaltenreihenfolge ändern im HUMANDATA Datensatz

d_humandata_bearbeitet <- d_humandata_bearbeitet[, c("EVENT_DATE", "LATITUDE", "LONGITUDE", "FATALITIES")]

#1. Zeile entfernen im HUMANDATA Datensatz

d_humandata_bearbeitet <- tail(d_humandata_bearbeitet, -1)

# Format des Datums angleichen 

library(lubridate)
d_humandata_bearbeitet$EVENT_DATE <- ymd_hms(d_humandata_bearbeitet$EVENT_DATE)
print(d_humandata_bearbeitet$EVENT_DATE)
d_humandata_bearbeitet$EVENT_DATE_YMD <- format(d_humandata_bearbeitet$EVENT_DATE, format = "%Y-%m-%d %H:%M:%S")

d_humandata_bearbeitet <- d_humandata_bearbeitet %>%
  select( EVENT_DATE, LATITUDE, LONGITUDE, FATALITIES) 

# der d_humandata_bearbeitet Datensatz soll nur bis dahin gehen, wo der ACLED Datansatz anfängt, deswegen kürze ich ihn: 

d_humandata_bearbeitet <- d_humandata_bearbeitet %>%
  filter(d_humandata_bearbeitet$EVENT_DATE <= as.Date("2018-01-01"))

#Jetzt geht der eine Datensatz von 2014 bis 2018 und der andere von 2018 bis 2024

#Jetzt das Format der Koordinaten ändern

d_humandata_bearbeitet$LONGITUDE <- as.numeric(d_humandata_bearbeitet$LONGITUDE)
d_humandata_bearbeitet$LATITUDE <- as.numeric(d_humandata_bearbeitet$LATITUDE)

d_ACLED_bearbeitet$LONGITUDE <- as.numeric(d_ACLED_bearbeitet$LONGITUDE)
d_ACLED_bearbeitet$LATITUDE <- as.numeric(d_ACLED_bearbeitet$LATITUDE)


# Merge Data --------------------------------------------------------------

d_2014_bis_2018 <- bind_rows(d_humandata_bearbeitet, d_ACLED_bearbeitet)

# Interaktive Karte erstellen ---------------------------------------------

# install.packages(c("shiny", "slider"))
# install.packages("leaflet.extras")

library(leaflet)
library(dplyr)
library(shiny)
library(slider)
library(leaflet.providers)
library(leaflet.extras)

#Grenze der Ukraine:
# Ohne Krim und nicht besonders genua, aber ein besseres GEOJSON File konnte ich nicht finden

library(sf)

ukraine_geojson <- st_read("/Users/tilmannhogrefe/Desktop/R/R Kurs/Datasets/ukraine-detailed-boundary_1059.geojson")



d_2014_bis_2018_YM <- d_2014_bis_2018 %>%
  mutate(EVENT_MONTH = ym(paste0(year(EVENT_DATE),
                                 "-", month(EVENT_DATE))))

#Shiny App:

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$style(
        HTML("
          .leaflet-container {
            height: calc(100vw / 1.85) !important; # Hier wird die Höhe berechnet, um das Seitenverhältnis zu erreichen 
          }
        ")
      )
    ),
    sliderInput("date_slider", "Datum auswählen", 
                min = min(d_2014_bis_2018$EVENT_DATE), max = max(d_2014_bis_2018$EVENT_DATE), 
                value = min(d_2014_bis_2018$EVENT_DATE), step = 1),
    checkboxInput("show_fatalities", "Fatalities", value = FALSE),
    checkboxInput("show_armed_clash", "Armed Clash", value = FALSE),
    checkboxInput("show_artillery", "Artillery", value = FALSE),
    checkboxInput("show_civilian_targeting", "Civilian Targeting", value = FALSE),
    leafletOutput("map")
  ),
  server = function(input, output, session) {
    filtered_data <- reactive({ 
      subset(d_2014_bis_2018, as.Date(EVENT_DATE) == as.Date(input$date_slider))
    })  
    
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles("OpenStreetMap.HOT") 
      
      # Bedingungen für Anzeige basierend auf Checkboxen
      if (input$show_fatalities) {
        m <- m %>% addMarkers(data = filtered_data() %>% filter(FATALITIES != 0), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              label = ~paste("Fatalities: ", FATALITIES))
      }
      
      if (input$show_armed_clash) {
        m <- m %>% addCircles(data = filtered_data() %>% filter(ARMED_CLASH == "Armed Clash" | ARMED_CLASH == "Attack"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              radius = 10,  # Kreise 10 mal größer machen
                              color = "red",  # Rot
                              fillOpacity = 0.5)  # Leicht durchsichtig
      }
      
      if (input$show_artillery) {
        m <- m %>% addHeatmap(data = filtered_data() %>% filter(ARTILLERY == "Shelling/artillery/missile attack" | ARTILLERY == "Air/drone strike"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              blur = 20, max = 0.5, radius = 15)
      }
      
      if (input$show_civilian_targeting) {
        m <- m %>% addMarkers(data = filtered_data() %>% filter(CIVILIAN_TARGETING == "Civilian Targeting"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              label = ~paste("Civilian Casualties", FATALITIES))
      }
      
      # Hinzufügen der Grenzen der Ukraine
      m <- m %>% addPolygons(data = ukraine_geojson, 
                             color = "blue",
                             fillOpacity = 0.005,
                             weight = 2)
      
      # Zoom beim Öffnen
      m <- m %>% setView(lng = mean(d_2014_bis_2018$LONGITUDE), 
                         lat = mean(d_2014_bis_2018$LATITUDE), zoom = 6.4)
      
      m
    })
  }
)



# Version mit YM, anstelle von YMD, funktioniert nicht -----------------------------------------------------------

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$style(
        HTML("
          .leaflet-container {
            height: calc(100vw / 1.85) !important; # Hier wird die Höhe berechnet, um das Seitenverhältnis zu erreichen 
          }
        ")
      )
    ),
    sliderInput("date_slider", "Datum auswählen", 
                min = min(d_2014_bis_2018_YM$EVENT_MONTH), 
                max = max(d_2014_bis_2018_YM$EVENT_MONTH), 
                value = min(d_2014_bis_2018_YM$EVENT_MONTH), 
                timeFormat = "%Y-%m", 
                ticks = FALSE), # Schritte des Sliders anzeigen
    
    checkboxInput("show_fatalities", "Fatalities", value = FALSE),
    checkboxInput("show_armed_clash", "Armed Clash", value = FALSE),
    checkboxInput("show_artillery", "Artillery", value = FALSE),
    checkboxInput("show_civilian_targeting", "Civilian Targeting", value = FALSE),
    leafletOutput("map")
  ),
  server = function(input, output, session) {
    filtered_data <- reactive({ 
      subset(d_2014_bis_2018_YM, as.Date(EVENT_MONTH) == as.Date(input$date_slider))
    })  
    
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles("OpenStreetMap.HOT") 
      
      # Bedingungen für Anzeige basierend auf Checkboxen
      if (input$show_fatalities) {
        m <- m %>% addMarkers(data = filtered_data() %>% filter(FATALITIES != 0), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              label = ~paste("Fatalities: ", FATALITIES))
      }
      
      if (input$show_armed_clash) {
        m <- m %>% addCircles(data = filtered_data() %>% filter(ARMED_CLASH == "Armed Clash" | ARMED_CLASH == "Attack"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE)
      }
      
      if (input$show_artillery) {
        m <- m %>% addHeatmap(data = filtered_data() %>% filter(ARTILLERY == "Shelling/artillery/missile attack" | ARTILLERY == "Air/drone strike"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              blur = 20, max = 0.5, radius = 15)
      }
      
      if (input$show_civilian_targeting) {
        m <- m %>% addMarkers(data = filtered_data() %>% filter(CIVILIAN_TARGETING == "Civilian Targeting"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              label = ~paste("Civilian Casualties", FATALITIES))
      }
      
      # Hinzufügen der Grenzen der Ukraine
      m <- m %>% addPolygons(data = ukraine_geojson, 
                             color = "red",
                             fillOpacity = 0.005,
                             weight = 4)
      
      # Zoom beim Öffnen
      m <- m %>% setView(lng = mean(d_2014_bis_2018$LONGITUDE), 
                         lat = mean(d_2014_bis_2018$LATITUDE), zoom = 6.4)
      
      m
    })
  }
)



# Try out stuff -----------------------------------------------------------

library(shiny)
library(leaflet)

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$style(
        HTML("
          .leaflet-container {
            height: calc(100vw / 1.85) !important; # Hier wird die Höhe berechnet, um das Seitenverhältnis zu erreichen 
          }
        ")
      )
    ),
    fluidRow(
      column(3,  # Spalte für den Slider und die Checkboxen
             sliderInput("date_slider", "Datum auswählen", 
                         min = min(d_2014_bis_2018$EVENT_DATE), max = max(d_2014_bis_2018$EVENT_DATE), 
                         value = min(d_2014_bis_2018$EVENT_DATE), step = 1),
             checkboxInput("show_fatalities", "Fatalities", value = FALSE),
             checkboxInput("show_armed_clash", "Armed Clash", value = FALSE),
             checkboxInput("show_artillery", "Artillery", value = FALSE),
             checkboxInput("show_civilian_targeting", "Civilian Targeting", value = FALSE)
      ),
      column(9,  # Spalte für die Karte
             leafletOutput("map")
      )
    )
  ),
  server = function(input, output, session) {
    filtered_data <- reactive({ 
      subset(d_2014_bis_2018, as.Date(EVENT_DATE) == as.Date(input$date_slider))
    })  
    
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles("OpenStreetMap.HOT") 
      
      # Bedingungen für Anzeige basierend auf Checkboxen
      if (input$show_fatalities) {
        m <- m %>% addMarkers(data = filtered_data() %>% filter(FATALITIES != 0), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              label = ~paste("Fatalities: ", FATALITIES))
      }
      
      if (input$show_armed_clash) {
        m <- m %>% addCircles(data = filtered_data() %>% filter(ARMED_CLASH == "Armed Clash" | ARMED_CLASH == "Attack"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              radius = 10,  # Kreise 10 mal größer machen
                              color = "red",  # Rot
                              fillOpacity = 0.5)  # Leicht durchsichtig
      }
      
      if (input$show_artillery) {
        m <- m %>% addHeatmap(data = filtered_data() %>% filter(ARTILLERY == "Shelling/artillery/missile attack" | ARTILLERY == "Air/drone strike"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              blur = 20, max = 0.5, radius = 15)
      }
      
      if (input$show_civilian_targeting) {
        m <- m %>% addMarkers(data = filtered_data() %>% filter(CIVILIAN_TARGETING == "Civilian Targeting"), 
                              lat = ~LATITUDE, lng = ~LONGITUDE,
                              label = ~paste("Civilian Casualties", FATALITIES))
      }
      
      # Hinzufügen der Grenzen der Ukraine
      m <- m %>% addPolygons(data = ukraine_geojson, 
                             color = "red",
                             fillOpacity = 0.005,
                             weight = 4)
      
      # Zoom beim Öffnen
      m <- m %>% setView(lng = 30.94672, 
                         lat = 49.44336, zoom = 5.4)
      
      m
    })
  }
)

## Die App veröffentlichen, um einen Link und QR Code erstellen  --------

#hat nicht funktioniert:



# # Shiny App exportieren
# #install.packages("htmlwidgets")
# library(htmlwidgets)
# 
# app <- shinyApp(ui = ui, server = server)
# 
# # Extrahiere den HTML-Code der Shiny-App
# app_html <- as.tags(app$ui)
# 
# # Speichere den HTML-Code als HTML-Datei
# writeLines(as.character(app_html), "meine_app.html")
# 
# 

# #install.packages("rsconnect")
# 
# library(rsconnect)
# 
# #Connect with token
# rsconnect::setAccountInfo(name='projektrkurstilmannhogrefeukrainewar',
#                           token='851B14A7B6BD736057F799F6FC8DF509',
#                           secret='yMITmxxlx8IbtRy3IkUfm029r/ciH8hMrpMUz2Fv')
# 
# 
# rsconnect::deployApp("/Users/tilmannhogrefe/Desktop/R/R Kurs/rsconnect/shinyapps.io/projektrkurstilmannhogrefeukrainewar")
# 
# 


...



















#2. Graph: Fatalities -------------------------------------------------------
# 
# library(ggplot2)
# 
# ggplot(d_2014_bis_2018, aes(x = EVENT_DATE, y = FATALITIES)) +
#   geom_point() +
#   labs(title = "Fatalities per day", x = "Date", y = "Deaths")
# 
# 
# 
# 




















# 3. Arbeiten mit dem ACLED Datensatz -------------------------------------








































# Spielerein --------------------------------------------------------




# Plot Fatalities and Year

ggplot(data = d_ACLED_2018_bis_2024, aes(x = EVENT_DATE, y = FATALITIES)) +
  geom_line() +
  theme_bw()

#Jetzt möchte ich verschiedene Events aus der Spalte SUB_EVENT_TYPE filtern

ggplot(data = d_ACLED_2018_bis_2024, aes(x = EVENT_DATE, y = SUB_EVENT_TYPE == "Attack")) +
  geom_point() +
  theme_bw()

#Jetzt habe ich Data Points TRUE or FALSE, ich möchte aber gerne die kummulierten Attacks haben:

d_attacks <- d_ACLED_2018_bis_2024 %>%
  filter(SUB_EVENT_TYPE == "Attack" )


#Hier der Regressions Versuch:

ggplot(data = d_ACLED_2018_bis_2024, aes(x = EVENT_DATE, y = FATALITIES)) +
  geom_point() +
  geom_smooth(method=lm, color="blue") +
  theme_classic()

# Plot Line Drone Losses

ggplot(d_daily_losses_2022, aes(x=date, y=drone)) + 
  geom_line() +
  labs(x = "Month", y = "Number of drone losses") +
  ggtitle("Number of drone losses per day") +
  theme_bw()


