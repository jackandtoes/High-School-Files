# LOAD IN PACKAGES
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(DataExplorer)
library(leaflet)

# LOAD IN DATASET
graf <- read.csv("C:/Users/EJacinto/data/graf.csv")
View(graf)

# AUTOMATED EDA
summary(graf)
create_report(graf)

# CLEANING THE DATA
graf <- janitor::clean_names(graf)
graf <- graf  %>% rename(graffiti_surface_type = what_is_the_graffiti_on, 
                         owner_of_property = are_you_the_owner_of_the_property_the_graffiti_is_on, 
                         pay_station_num = pay_station_number_or_location_number, 
                         obscene = is_the_graffiti_obscene_racial_or_hateful,
                         service_req_num = service_request_number,
                         request_date = created_date)

# MAKING THE MAP
leaflet(data = graf) %>%
  addTiles() %>%
  setView(lng = -122.3, lat = 47.6, zoom = 9) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
    radius = 3, stroke = FALSE,fillOpacity = 0.6)
