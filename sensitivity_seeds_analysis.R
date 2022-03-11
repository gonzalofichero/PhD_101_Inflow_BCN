# Loading libraries
library(tidyverse)


#############################################################################
# Data wrangling for plotting seeds data
indiv_barri73_sensitity_beach %>% 
  select(BARRI_COD, transitory_pca, avg_rent_2015, amenities_pca_mintime) %>% 
  mutate(transitory_pca = (-1) * transitory_pca,
         avg_rent_2015 = case_when(BARRI_COD == "47" ~ 8.14,
                                   TRUE ~ avg_rent_2015)) %>% 
  mutate(scale_rent = scale(avg_rent_2015),
         scale_rooting = scale(transitory_pca),
         scale_amenities = scale(amenities_pca_mintime)
         ) %>% 
  unique() -> full_bcn




###############################################################
# Creating dfs for each seed (by Barri)
full_bcn %>% 
  filter(BARRI_COD %in% c("05", "11", "12",
                          "14","19","26",
                          "37","38",'41',
                          "44","57","67")) -> seed31416


full_bcn %>% 
  filter(BARRI_COD %in% c("11", "22", "34",
                          "38","47","48",
                          "52","53",'54',
                          "61","67","68")) -> seed42

full_bcn %>% 
  filter(BARRI_COD %in% c("05", "11", "12",
                          "14","17","20",
                          "21","31",'40',
                          "46","49","51",
                          "55","59","61","72")) -> seed217

full_bcn %>% 
  filter(BARRI_COD %in% c("02", "06", "09",
                          "10","16","24",
                          "27","37",'47',
                          "52","59","62","64","68")) -> seed4432

full_bcn %>% 
  filter(BARRI_COD %in% c("06", "13", "15",
                          "18","20","23",
                          "24","25",'26',
                          "32","33","36",
                          "38","41","42","44","46",
                          "48","54","60","65","66",
                          "67","68","70","71")) -> seed746



###########################################################################################
# Plotting Rent for Full df and samples
plot(density(full_bcn$scale_rent), col="black", type="l", lty = 1, lwd = 2, ylim=c(0,0.5),
     xlab="Avg Rent (scaled)", main="")
lines(density(seed31416$scale_rent), col="red", type = "l")
lines(density(seed42$scale_rent), col="blue", type = "l")
lines(density(seed217$scale_rent), col="green", type = "l")
lines(density(seed4432$scale_rent), col="black", type = "l", lty = 2, lwd = 2)
lines(density(seed746$scale_rent), col="purple", type = "l")
legend("topright", legend = c("full data", "seed31416", "seed42",
                              "seed217","seed4432", "seed746"),
       col = c("black", "red", "blue", "green", "black", "purple"), 
       lty = c(1,1,1,1,2,1))


# Plotting Amenities for Full df and samples
plot(density(full_bcn$scale_amenities), col="black", type="l", lty = 1, lwd = 2, ylim=c(0,0.5),
     xlab="PCA Amenities (scaled)", main="")
lines(density(seed31416$scale_amenities), col="red", type = "l")
lines(density(seed42$scale_amenities), col="blue", type = "l")
lines(density(seed217$scale_amenities), col="green", type = "l")
lines(density(seed4432$scale_amenities), col="black", type = "l", lty = 2, lwd = 2)
lines(density(seed746$scale_amenities), col="purple", type = "l")
legend("topright", legend = c("full data", "seed31416", "seed42",
                              "seed217","seed4432", "seed746"),
       col = c("black", "red", "blue", "green", "black", "purple"), 
       lty = c(1,1,1,1,2,1))


# Plotting Rooting for Full df and samples
plot(density(full_bcn$scale_rooting), col="black", type="l", lty = 1, lwd = 2, ylim=c(0,0.8),
     xlab="PCA Rooting (scaled)", main="")
lines(density(seed31416$scale_rooting), col="red", type = "l")
lines(density(seed42$scale_rooting), col="blue", type = "l")
lines(density(seed217$scale_rooting), col="green", type = "l")
lines(density(seed4432$scale_rooting), col="black", type = "l", lty = 2, lwd = 2)
lines(density(seed746$scale_rooting), col="purple", type = "l")
legend("topright", legend = c("full data", "seed31416", "seed42",
                              "seed217","seed4432", "seed746"),
       col = c("black", "red", "blue", "green", "black", "purple"), 
       lty = c(1,1,1,1,2,1))