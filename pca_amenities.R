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
amenities_pca <- data.frame(cbind(data_barri2$BARRI_COD,pc$x[,1])) %>% 
                  rename(BARRI_COD = X1,
                         amenities_pc = X2) %>% 
                  mutate(amenities_pc = as.double(amenities_pc))




###################################
# PCA on transitority

feature_transitority <- indiv_barri73_fixed %>% 
                        select(sum_old, airbnb_dom) %>% 
                        unique() %>% 
                        mutate_if(is.numeric, ~replace_na(., 0))


# Checking correlation
library(psych)

pairs.panels(feature_transitority,
             gap = 0,
             pch=21)


pc_transitority <- prcomp(feature_transitority,
                     center = TRUE,
                     scale. = TRUE)

# Check factors
summary(pc_transitority)
# 1 factor gets 85% of total variance...
# I'm reducing to half the variables = higher factor means less transitority


# Trying to understand what the factors are...
library(ggbiplot)
ggbiplot(pc_transitority,
         obs.scale = 1,
         var.scale = 1,
         ellipse = TRUE,
         circle = TRUE,
         ellipse.prob = 0.68) + 
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')


# Extract first factor
transitority_vars_pca <- data.frame(cbind(data_barri2$BARRI_COD,pc_transitority$x[,1])) %>% 
                                    dplyr::rename(BARRI_COD = X1,
                                                  transitory_pca = X2) %>% 
                                    mutate(transitory_pca = as.double(transitory_pca))


###############################
# PCA for control variables

feature_control <- indiv_barri73_fixed %>% 
  select(sum_old, age_building, perc_left,
         perc_domi_uni_25_40, excess_uni, airbnbs) %>% 
  unique()


# Checking correlation
library(psych)

pairs.panels(feature_control,
             gap = 0,
             pch=21)


pc_control <- prcomp(feature_control,
                     center = TRUE,
                     scale. = TRUE)

attributes(pc_control)

# Check factors
summary(pc_control)
# First 3 factors gets 90% of total variance...
# I'm reducing to half the variables


# Trying to understand what the factors are...
library(ggbiplot)
ggbiplot(pc_control,
         obs.scale = 1,
         var.scale = 1,
         ellipse = TRUE,
         circle = TRUE,
         ellipse.prob = 0.68) + 
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')


# Extract first factor
control_vars_pca <- data.frame(cbind(data_barri2$BARRI_COD,pc_control$x[,1:3])) %>% 
  dplyr::rename(BARRI_COD = V1,
                control_pca_factor1 = PC1,
                control_pca_factor2 = PC2,
                control_pca_factor3 = PC3) %>% 
  mutate(control_pca_factor1 = as.double(control_pca_factor1),
         control_pca_factor2 = as.double(control_pca_factor2),
         control_pca_factor3 = as.double(control_pca_factor3))
