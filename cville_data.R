# Globals ----
library(magrittr)
library(tidyverse)
library(geojsonsf)
library(ggmap)
library(jsonlite)
library(readr)
library(sf)

# Crime ----
# CRIME == CALLS != ARRESTS 

# Get crime data from Open Data Portal api:https://opendata.charlottesville.org/datasets/crime-data/api

crime_api <- "https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/6/query?where=1%3D1&outFields=*&outSR=4326&f=json"
response <- fromJSON(crime_api) #takes ~5 minutes, reads max 10k rows or approx only 2 years from 2020-2022

data_raw <- response$features$attributes %>% 
  janitor::clean_names() %>%
  mutate(block_number = ifelse(block_number == "", NA, block_number)) %>%
  mutate(date_reported = date_reported %>% gsub('000$', '', .) %>% as.numeric() %>% as.POSIXct()) %>%
  filter(date_reported < as.Date(Sys.Date())) %>% #remove future dates 
  filter(block_number != "NA") #remove missing block numbers (6%)


# Get just gun-related data from ODP: weapons, armed, shots
gun_api <- "https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/6/query?where=Offense%20%3D%20'%25ARMED%25'&outFields=*&outSR=4326&f=json"
gun_response <- fromJSON(gun_api)
gun_raw <- gun_response$fields
  
  
# Geocode:
data_raw %<>% mutate(address = paste(block_number, street_name, "Charlottesville VA"))
lon_lat <- geocode(data_raw$address) #takes ~10 minutes
crime <- bind_cols(data_raw, lon_lat) %>%
  filter(lon > -78.54, lat > 38.00 & lat < 38.08) #9265

# write_csv(crime, "crime.csv")
crime <- read_csv("crime.csv")

# Filter:
drugs <- crime %>%
  filter(grepl("Drug|Narcotics", offense)) #102 obs
guns <- crime %>% 
  filter(grepl("Robbery - Armed|Weapons Violations|Shots Fired/Illegal Hunting", offense)) #149 obs
other <- crime %>%
  filter(!grepl("Drug|Narcotics|Robbery - Armed|Weapons Violations|Shots Fired/Illegal Hunting", "offense"))

# Density----

cville_map <- get_map(c(left = -78.53, bottom = 38.00, right = -78.45, top = 38.07), maptype = "roadmap", color = "bw")

# All crime:
all_density <- ggmap(cville_map) +
  stat_density2d(data = crime, aes(fill = ..level.., alpha = 0.1),
                 geom = "polygon") +
  theme(legend.position="none") +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("All Crime Reports")

# Drug calls:
drug_density <- ggmap(cville_map) +
  stat_density2d(data = drugs, aes(fill = ..level.., alpha = ..level..),
                 geom = "polygon") +
  theme(legend.position="none") +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Drug-Related")

# Gun calls:
gun_density <- ggmap(cville_map) +
  stat_density2d(data = guns, aes(fill = ..level.., alpha = ..level..),
                 geom = "polygon") +
  theme(legend.position="none") +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Gun-Related")

plot_grid(all_density, gun_density, drug_density, ncol = 3)


# Census Blocks ----

# Get census block data:
census <- geojson_sf("US_Census_Tract_Area_2010.geojson") %>% 
  set_names(tolower)

ggplot(census) + 
  geom_sf() + 
  guides(fill = guide_none()) #census base view

# Convert to sf:

crime %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))
drugs %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))
guns %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))
other %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))

# Filter to only census blocks:
filter_census <- function(x) {
  x %<>% mutate(within = st_within(x, census) %>% as.numeric()) 
  x %<>% filter(!is.na(within))
}

filter_census(other)

crime %<>% mutate(within = st_within(crime, census) %>% as.numeric()) 
crime %<>% filter(!is.na(within))

drugs %<>% mutate(within = st_within(drugs, census) %>% as.numeric()) 
drugs %<>% filter(!is.na(within))

guns %<>% mutate(within = st_within(guns, census) %>% as.numeric()) 
guns %<>% filter(!is.na(within))

# View individual reports: 
ggplot(sample_frac(other, 0.1)) +
  geom_sf(jitter = .1, aes(color = 'Other'), alpha = .3) +
  geom_sf(data = census, alpha = .1) +
  geom_sf(data = drugs, aes(color = 'Drugs'), alpha = .3) +
  geom_sf(data = guns, aes(color = 'Guns'), alpha = .3)


# 2016 ----

# *Drugs ----

drug_raw <- read_csv("drug_raw.csv") %>%
  janitor::clean_names() %>%
  mutate(block_number = ifelse(block_number == "", NA, block_number)) %>%
  mutate(date_reported = date_reported %>% gsub('000$', '', .) %>% as.numeric() %>% as.POSIXct()) %>%
  filter(block_number != "NA") #remove missing block numbers

drug_raw %<>% mutate(address = paste(block_number, street_name, "Charlottesville VA"))
lon_lat_drugs <- geocode(drug_raw$address) 
drug_data <- bind_cols(drug_raw, lon_lat_drugs) %>%
  filter(lon > -78.54, lat > 38.00 & lat < 38.08)

drug_data %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))
filter_census(drug_data)

#write_csv(drug_data, "drug_data.csv")
drug_data <- read_csv("drug_data.csv")

#crime %<>% mutate(drug_flag = ifelse(grepl("Drug|Narcotics", offense, ignore.case = TRUE), "drugs", "not_drugs"))
#filter(crime, drug_flag == "drugs") %>% with(table(offense))

drug_counts <- drug_data %>%
  group_by(address) %>% 
  count() %>%
  ungroup() %>% 
  arrange(n)

ggplot(drug_counts) +
  geom_sf(data = census) +
  geom_sf(aes(size = n, color = n, alpha = n))

# *Violence----
# Assault:
crime %<>% mutate(assault_flag = ifelse(grepl("Assault", offense, ignore.case = TRUE),
                                     "assault", "not_assault"))

filter(crime, assault_flag == "assault") %>% with(table(offense))

assault_counts <- crime %>%
  group_by(address, assault_flag) %>% 
  count() %>%
  ungroup() %>% 
  arrange(n)

ggplot(assault_counts) +
  geom_sf(data = census) +
  geom_sf(aes(size = n, color = n, alpha = n)) +
  facet_wrap(~assault_flag)

#Guns: 
crime %<>% mutate(gun_flag = ifelse(grepl("Robbery - Armed|Weapons Violations|Shots Fired/Illegal Hunting", 
                                          offense, ignore.case = TRUE), "guns", "not_guns"))

filter(crime, gun_flag == "guns") %>% with(table(offense))

gun_counts <- crime %>%
  group_by(address, gun_flag) %>% 
  count() %>%
  ungroup() %>% 
  arrange(n)

ggplot(gun_counts) +
  geom_sf(data = census) +
  geom_sf(aes(size = n, color = n, alpha = n)) +
  facet_wrap(~gun_flag)

# Frequent addresses ----
station_props <- arrange(drug_counts, -n) %>%
  add_count(wt = n) 

all_counts <- crime %>%
  group_by(address) %>%
  count() %>%
  ungroup() %>%
  arrange(n)


# Next steps----

arrests <- read_csv("Desktop/Arrests.csv") %>%
  janitor::clean_names()

table(arrests$statute_description) %>% sort()


#FIREARM: POSSESS BY FELON NONVIOLENT W/IN 10 YRS (76), FIREARM/ETC: POINTING/BRANDISHING (73), 
#FIREARM: USE IN COMMISSION OF FELONY, 1ST OFF (42), FIREARM: RECKLESS HANDLING (29), 
#Carry Concealed Weapon (26), FIREARM: SHOOT IN PUBLIC PLACE, NOT CAUSE INJURY (24), 
#CONCEALED WEAPON: CARRY, 2 OFF (23), FIREARM: POSS/TRANSPORT BY FELON W/ VIOLENT OFF (21), 
#FIREARM: POSSESSION W/ SCH I OR II DRUG (18), Armed Robbery w/firearm - Highway/street (13), 
#FIREARM: POSSESS BY NON VIOLENT FELON, >10 YRS (12), Robbery by using or displaying a firearm (8),
#FIREARM: SHOOT FROM VEHICLES (8), FIREARM: RECEIVE STOLEN OR AID IN CONCEALING (8),
#UNLAWFULLY SHOOT OR THROW MISSLE AT OCC. BLDG (7), FIREARM:PURCHASE/TRANSPT FIREARM WHILE PROTECT ORDER IN EFFECT (7),
#TRAIN/CAR/BOAT: MALICIOUSLY SHOOT/THROW (6), Maliciously shoot/throw object in/at dwelling (6), 
#FIREARM:PURCHASE/TRANSPT FIREARM WHILE PROTECT ORDER IN EFFECT (6), WEAPON: SHOOT ACROSS ROAD OR STREET (5),
#FIREARM: SHOOT ON SCHOOL GROUNDS (5), EMERGENCY VEHICLE: UNLAWFULLY SHOOT/THROW (5), Poss of any other Illegal weapon/firearm (4),
#MACHINE GUN: POSSESS/USE FOR AGGRESSIVE PURPOSE (4), FIREARM: USE IN COMMISSION OF FELONY, 2ND+ OFF, 
#WEAPON LAW VIOLATIONS (3), FIREARM/ETC: POINTING/BRANDISHING,ON/NEAR SCHOOL (3), FIREARM:RECKLESS HANDLING CAUSES SERIOUS INJURY (3), 
#DISCHARGE FIREARM, MISSILE IN/AT OCC. SCHOOL (3), FIREARM: REMOVE/ALTER SERIAL NUMBERS (2),
#FIREARM: POSSESS ON/ABOUT PERSON W/SCH I,II DRUG (2), FIREARM: ALLOW CHILD <12 TO USE W/O ADULT SUPERV (2),
#DISCHARGE FIREARM IN CITY LIMITS, CARJACKING: WITH GUN OR SIMULATED GUN, SAWED-OFF GUN: POSSESS/USE FOR ANY OTHER PURPOSE,
# GUN DEALER: UNLAWFUL TRANSFER,  FIREARM:PURCH/TRANSPORT BY PROTECT.ORDER SUBJECT, FIREARM: POSSESSION BY PERSON <18Y,  
#FIREARM: POSSESS AFTER INVOL. COMMITTED



# Live gun API ----

# There are 3 incident types that directly pertain to guns: "Shots Fired/Illegal Hunting", "Robbery - Armed", "Weapons Violations"

gun_where_clause <- "%28Offense%20LIKE%20'%shot%'%29OR%28Offense%20LIKE%20'%armed%'%29OR%28Offense%20LIKE%20'%weapon%'%29"
gun_api <- glue::glue("https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/6/query?where={gun_where_clause}&outFields=*&outSR=4326&f=json")
gun_response <- fromJSON(gun_api)

gun_data <- gun_response$features$attributes%>% #~500 obs
  janitor::clean_names() %>%
  mutate(block_number = ifelse(block_number == "", NA, block_number)) %>%
  mutate(date_reported = date_reported %>% gsub('000$', '', .) %>% as.numeric() %>% as.POSIXct())

#TODO - mechanism for missing block numbers (17%) which are primary intersections 

gun_data %<>% mutate(address = paste(block_number, street_name, "Charlottesville VA"))
lon_lat <- geocode(gun_data$address) #takes ~10 minutes
gun_calls <- bind_cols(gun_data, lon_lat)

#TODO - schedule this report to run (daily? weekly?)

ggmap(cville_map) +
  stat_density2d(data = gun_calls, aes(fill = ..level.., alpha = 0.1),
                 geom = "polygon") +
  theme(legend.position="none") +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Gun-Related Calls Available in the Open Data Portal")








# Leaflet ----

gun_calls <- read_csv("data/guns.csv") %>%
  janitor::clean_names() %>%
  mutate(date_reported = as.Date(date_reported)) %>%
  mutate(address = paste(block_number, street_name, "Charlottesville VA"))

lon_lat_gun <- geocode(gun_calls$address)
gun_data <- bind_cols(gun_calls, lon_lat_gun)


# Density map
ggmap(cville_map) +
 stat_density2d(data = gun_data, aes(fill = ..level..), alpha = 0.4,
               geom = "polygon") +
  theme(legend.position="none") +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Gun-Related Calls to the Charlottesville Police Department",
       subtitle = "Feb 2019 - Jan 2024", 
       caption = "n = 457")
# TODO: figure out 51 rows containing non-finite values

# Interactive map 

map <- leaflet() %>%
  setView(lng = -78.49, lat = 38.03, zoom = 13) %>%
  addTiles()

map
