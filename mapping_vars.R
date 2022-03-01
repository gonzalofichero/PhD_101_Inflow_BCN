# Importing libraries for mapping and wrangling data
library(tidyverse)
library(sf)
library(colorspace)


# Uploading Barcelona Map
bcn_map <- st_read("Maps/0301100100_UNITATS_ADM_POLIGONS.json")

# Keeping only geometry information at neighborhood level
bcn_map2 <- bcn_map %>% filter(SCONJ_DESC == "Barri")


# Dataset with explanatory variables only (73 rows)
bcn_full_neighbor <- indiv_barri73_sensitity_beach %>% 
                      select(BARRI_COD, transitory_pca, avg_rent_2015, amenities_pca_mintime) %>% 
                      mutate(transitory_pca = (-1) * transitory_pca,
                             avg_rent_2015 = case_when(BARRI_COD == "47" ~ 8.14,
                                                       TRUE ~ avg_rent_2015)) %>% 
                      rename(BARRI = BARRI_COD) %>% 
                      unique()


# Mapping Transitority variable
bcn_map2 %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(bcn_full_neighbor, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = transitory_pca)) +
  scale_fill_continuous_sequential(palette= "Purples") +
  guides(fill=guide_legend(title="Transitority")) +
  theme_bw()


# Mapping Avg Renting variable
bcn_map2 %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(bcn_full_neighbor, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = avg_rent_2015)) +
  scale_fill_continuous_sequential(palette= "Purples") +
  guides(fill=guide_legend(title="Average Renting price ($/m2)")) +
  theme_bw()


# Mapping Amenities variable
bcn_map2 %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(bcn_full_neighbor, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = amenities_pca_mintime)) +
  scale_fill_continuous_sequential(palette= "Purples") +
  guides(fill=guide_legend(title="Amenities")) +
  theme_bw()

