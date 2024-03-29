# Loading necessary libraries
library(tidyverse)
library(mlogit)
library(gmnl)
library(psych)
library(ggbiplot)
library(stargazer)


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


###########################################
# Joining to main dataframe

indiv_barri73_20220218_final %>% 
  select(-amenities_pc) %>% 
  left_join(amenities_pca, by = "BARRI_COD") -> indiv_barri73_sensitity_beach



########################################################################
# Creating mlogit.data file for gmnl package needs

full_beach_73 <- mlogit.data(indiv_barri73_sensitity_beach, shape = "long",
                              choice = "ind_choice")

# Regression for Barceloneta
print(Sys.time())
full_beach_73_barceloneta <- mlogit(formula = ind_choice ~ transitory_pca + avg_rent_2015 + amenities_pca_barceloneta | 0 ,
                                   data = full_beach_73)
print(Sys.time())

# Regression for Bogatell
print(Sys.time())
full_beach_73_bogatell <- mlogit(formula = ind_choice ~ transitory_pca + avg_rent_2015 + amenities_pca_bogatell | 0, 
                                 data = full_beach_73)
print(Sys.time())

# Regression for NovaMarBella
print(Sys.time())
full_beach_73_novamarb <- mlogit(formula = ind_choice ~ transitory_pca + avg_rent_2015 + amenities_pca_novamarb | 0 ,
                               data = full_beach_73)
print(Sys.time())

# Regression for Min Time to beach
print(Sys.time())
full_beach_73_mintime <- mlogit(formula = ind_choice ~ transitory_pca + avg_rent_2015 + amenities_pca_mintime | 0 ,
                               data = full_beach_73)
print(Sys.time())



#####################################
# Export results to stargazer table
stargazer(full_beach_73_barceloneta, full_beach_73_bogatell, 
          full_beach_73_novamarb, full_beach_73_mintime,
          covariate.labels = c("PCA transitory",
                               "Avg Rent",
                               "PCA amenities","PCA amenities","PCA amenities","PCA amenities"),
          column.labels=c("Barceloneta", "Bogatell", 
                          "NovaMarBella", "MinTime"),
          dep.var.labels = c("","","",""),
          type = "html", out="beach_sensitivity.html")


