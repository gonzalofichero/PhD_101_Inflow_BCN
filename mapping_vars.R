# Importing libraries for mapping and wrangling data
library(tidyverse)
library(sf)
library(colorspace)


# Uploading Barcelona Map
bcn_map <- st_read("Maps/0301100100_UNITATS_ADM_POLIGONS.json")

# Keeping only geometry information at neighborhood level
bcn_map2 <- bcn_map %>% filter(SCONJ_DESC == "Barri")


bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(rent, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = avg_rent_2015)) +
  scale_fill_continuous_sequential(palette= "Purples") +
  guides(fill=guide_legend(title="Avg Rent ($/m2)")) +
  theme_bw()