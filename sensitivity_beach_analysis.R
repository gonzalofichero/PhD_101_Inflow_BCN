# Loading necessary libraries
library(tidyverse)
library(mlogit)
library(gmnl)
library(psych)
library(ggbiplot)


########################################################################
# Taking a sample of full df + creating binary feature for nation group

indiv_barri73_20220218_final %>%
  select(BARRI_COD, bars_pop, cultural_pop, 
         Time_bike_Barceloneta, Time_bike_Bogatell,
         Time_bike_NovaMarBella, Min_Time)  %>% 
  unique() -> amenities


# Checking correlation for Barceloneta
pairs.panels(amenities[,c(2,3,4)],
             gap = 0,
             pch=21)


pca_barceloneta <- prcomp(amenities[,c(2,3,4)],
                 center = TRUE,
                 scale. = TRUE)

pca_bogatell <- prcomp(amenities[,c(2,3,5)],
                          center = TRUE,
                          scale. = TRUE)

pca_novamarb <- prcomp(amenities[,c(2,3,6)],
                          center = TRUE,
                          scale. = TRUE)

pca_mintime <- prcomp(amenities[,c(2,3,7)],
                          center = TRUE,
                          scale. = TRUE)


# Check factors
summary(pca_barceloneta)
summary(pca_bogatell)
summary(pca_novamarb)
summary(pca_mintime)

# Barceloneta: 1st factor gets 66% of total variance
# Bogatell: 1st factor gets 60% of total variance
# NovaMarBella: 1st factor gets 55% of total variance
# Min Time: 1st factor gets 63% of total variance


# Trying to understand what the factors are...
ggbiplot(pca_mintime,
         obs.scale = 1,
         var.scale = 1,
         ellipse = TRUE,
         circle = TRUE,
         ellipse.prob = 0.68) + 
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')


# Extract first factor
amenities_pca <- data.frame(cbind(data_barri2$BARRI_COD,
                            pca_barceloneta$x[,1],
                            pca_bogatell$x[,1],
                            pca_novamarb$x[,1],
                            pca_mintime$x[,1]
                            )) %>% 
  dplyr::rename( BARRI_COD = X1,
                 amenities_pca_barceloneta = X2,
                 amenities_pca_bogatell = X3,
                 amenities_pca_novamarb = X4,
                 amenities_pca_mintime = X5) %>% 
  mutate(amenities_pca_barceloneta = as.double(amenities_pca_barceloneta),
         amenities_pca_bogatell = as.double(amenities_pca_bogatell),
         amenities_pca_novamarb = as.double(amenities_pca_novamarb),
         amenities_pca_mintime = as.double(amenities_pca_mintime))



########################################################################
# Creating mlogit.data file for gmnl package needs

full_sample_73 <- mlogit.data(full_sample, shape = "long",
                              choice = "ind_choice")

print(Sys.time())
full_nation_73_interaction <- gmnl(formula = ind_choice ~ transitory_pca + avg_rent_2015 + amenities_pc - 1 | perc_ethnic + Sexe - 1 | 0 | binary_nation - 1,
                                   data = full_sample_73,
                                   model = "mixl",
                                   ranp = c(avg_rent_2015 = "ln", amenities_pc = "n"),
                                   mvar = list(avg_rent_2015 = c("binary_nation"),
                                               amenities_pc = c("binary_nation")),
                                   method = "bfgs",
                                   R = 50
)
print(Sys.time())    