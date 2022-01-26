##########################
####  AIRBNB data  ########

airbnb <- read_csv("Airbnb/tomslee_airbnb_barcelona_0199_2015-11-06.csv")

airbnb <- airbnb %>% select(room_id, bedrooms, price, latitude, longitude)




##########################
####  MAPPING  ########
# 
library(tidyverse)
library(sf)
library(colorspace)


bcn_map <- st_read("Maps/0301100100_UNITATS_ADM_POLIGONS.json")

bcn_map2 <- bcn_map %>% filter(SCONJ_DESC == "Barri") %>% select(DISTRICTE, BARRI, CODI_UA,
                                                                 geometry)



####################################
# Joining Airbnb data to Json
airbnb_sf <- airbnb %>%
  mutate_at(vars(latitude, longitude), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("latitude", "longitude"),
    agr = "constant",
    crs = 25831,
    stringsAsFactors = FALSE,
    remove = TRUE
  )

air_full <- st_join(airbnb_sf, bcn_map2, join = st_within)

air_per_barri <- count(as_tibble(air_full), BARRI)


