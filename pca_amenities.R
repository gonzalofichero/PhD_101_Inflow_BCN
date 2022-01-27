library(tidyverse)

setwd("C:/Users/ggarcia/Desktop/PhD GG/10 - Data/01 - Paper 1")

bcn <- read_delim("discrete_choice_dataset_bar.txt", 
                  delim = "|", col_names = TRUE)


bcn %>%
  filter(!is.na(BARRI_COD)) %>% 
  select(BARRI_COD, Nom_Barri_dest,
         Poblacio, Domicilis, bars, age_building,
         perc_left, excess_uni, perc_domi_uni_25_40,
         Cinemas, Teatres, avg_rent_2015, sum_old) %>% 
  group_by(BARRI_COD, Nom_Barri_dest) %>% 
  unique() %>% 
  ungroup() -> data_barri


# Import data on time to beach for each barri
beach <- read_delim("beach_distance_BCN.txt", delim = "|")


data_barri2 <- data_barri %>% left_join(beach, by = "BARRI_COD")


# Checking PCA for "amenities" restrictions: Bars, Cultural, Beach

data_barri2 %>% 
  mutate(bars_pop = bars / Poblacio,
         cultural_pop = (Cinemas + Teatres) / Poblacio) %>% 
  rename(beach_time = Time_bike_Barceloneta) %>% 
  select(BARRI_COD, bars_pop, cultural_pop, beach_time) -> amenities

# Checking correlation
library(psych)

pairs.panels(amenities[,-1],
             gap = 0,
             pch=21)


pc <- prcomp(amenities[,-1],
             center = TRUE,
             scale. = TRUE)

attributes(pc)

# Check factors
summary(pc)
# 1st factor gets 2/3rds of total variance...
# makes sense to use this method?


# Trying to understand what the factors are...
library(ggbiplot)
ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68) + 
              scale_color_discrete(name = '') +
              theme(legend.direction = 'horizontal',
              legend.position = 'top')


# Extract first factor
amenities_pca <- cbind(data_barri2$BARRI_COD,pc$x[,1])



