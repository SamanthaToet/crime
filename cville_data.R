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

# Get crime data from Open Data Portal api:

crime_api <- "https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/6/query?where=1%3D1&outFields=*&outSR=4326&f=json"
response <- fromJSON(crime_api) #takes ~5 minutes, reads max 10k rows or approx only 2 years from 2020-2022

data_raw <- response$features$attributes %>% 
  janitor::clean_names() %>%
  mutate(block_number = ifelse(block_number == "", NA, block_number)) %>%
  mutate(date_reported = date_reported %>% gsub('000$', '', .) %>% as.numeric() %>% as.POSIXct()) %>%
  filter(date_reported < "2022-09-03 00:00:00") %>% #remove future dates 
  filter(block_number != "NA") #remove missing block numbers (6%)

# Geocode:

data_raw %<>% mutate(address = paste(block_number, street_name, "Charlottesville VA"))

lon_lat <- geocode(data_raw$address) #takes ~10 minutes
crime <- bind_cols(data_raw, lon_lat) %>%
  filter(lon > -78.54, lat > 38.00 & lat < 38.08)

# Get census block data:
census <- geojson_sf("US_Census_Tract_Area_2010.geojson") %>% 
  set_names(tolower)

# Convert crime to sf:
crime %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))

# Plot ----

ggplot(census) + 
  geom_sf() + 
  guides(fill = guide_none()) #census base view

# Filter to only census blocks:
crime %<>% mutate(within = st_within(crime, census) %>% as.numeric()) 
crime %<>% filter(!is.na(within))


ggplot(crime) +
  geom_sf(alpha = .1, jitter = .1) +
  geom_sf(data = census, fill = "blue", alpha = .1)

# Add streets
library(remotes)
library(osmdata)
library(rvest)
getbb("Charlottesville Virginia")


# Get features
big_streets <- getbb("Charlottesville Virginia") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Charlottesville Virginia") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- getbb("Charlottesville Virginia") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street","unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

water <- getbb("Charlottesville Virginia") %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = c("river", "lake")) %>%
  osmdata_sf()

railway <- getbb("Charlottesville Virginia") %>%
  opq() %>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf()

ggplot(crime) +
  geom_sf(alpha = .1, jitter = .1) +
  geom_sf(data = census, fill = "blue", alpha = .1) +
  geom_sf(data = water$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-78.54, -78.44), 
           ylim = c(38.0, 38.08),
           expand = FALSE)
  
  



ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black")
  



  
  
# More:                             
# Can this be filtered to only drug-related offenses?
# Drug Equipment Violation (16), Drug Investigation (27), Narcotics (33), Drug/Narcotics Violation (62)

# Can this be filtered to only gun-related offenses? # CAVEAT
#Robbery - Armed (29), Weapons Violations (45), Shots Fired/Illegal Hunting (120)


# Next steps----

arrests <- read_csv("Desktop/Arrests.csv") %>%
  janitor::clean_names()

table(arrests$statute_description) %>% sort()

#DRUGS: POSSESS SCH I OR II (131), DRUG:SELL-DISTRIBUTE-MANUFACTURE: TYPE NOT CLEAR (43), 
#DRUGS: POSSESS W/INTENT TO MANUF/SELL SCH I, II (26), FIREARM: POSSESSION W/ SCH I OR II DRUG (18),
#DRUG/NARCOTICS VIOLATIONS (16), DRUGS: POSSESS MARIJUANA, 1ST OFF (10), DRUGS: POSSESS MARIJUANA, 2+ OFF (8),
#DRUGS: DISTRIB/PWI MARIJUANA >1/2 OZ TO 5 LBS (8),  MARIJUANA SYNTHETIC:POSSES CANNABIMIMETIC AGENT (6),
#DRUGS: DISTRIB/PWI MARIJUANA <1/2 OZ (6), Under the Influence of Marijuana (3), DRUGS: MANUFACTURE/DISTRIBUTE SCH I, II, 2 OFF (3),
# DRUG PARAPHERNALIA: SELL/POSSESS TO SELL, DRUGS: POSSESS SCH IV,  DRUGS: POSSESS SCH III, DRUGS: INHALING DRUGS OR NOXIOUS CHEMICALS,
# DRUGS: TRANSPORT TO VA 5+ LBS MARIJUANA,  DRUGS: MANUFACTURE/DISTRIBUTE SCH III, IV OR V





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









