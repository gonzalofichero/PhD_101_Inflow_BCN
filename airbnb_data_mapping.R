##########################
####  AIRBNB data  ########

airbnb <- read_csv("Airbnb/tomslee_airbnb_barcelona_0199_2015-11-06.csv")

airbnb <- airbnb %>%  select(room_id, bedrooms, price, latitude, longitude)




##########################
####  MAPPING  ########
# 
library(tidyverse)
library(sf)
library(colorspace)


bcn_map <- st_read("Maps/0301100100_UNITATS_ADM_POLIGONS.json")

bcn_map2 <- bcn_map %>% filter(SCONJ_DESC == "Barri")
