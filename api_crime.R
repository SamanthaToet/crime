library(magrittr)
library(tidyverse)
library(jsonlite)
library(ggmap)

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
lon_lat <- geocode(gun_data$address) #takes ~10 minutes, eventually should be cached 
gun_calls <- bind_cols(gun_data, lon_lat)

#TODO - schedule this to run (daily? weekly?)

#Exploratory visualizations:

cville_map <- get_map(c(left = -78.53, bottom = 38.00, right = -78.45, top = 38.07), maptype = "roadmap", color = "bw")

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
