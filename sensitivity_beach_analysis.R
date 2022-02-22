library(tidyverse)
library(mlogit)
library(gmnl)


########################################################################
# Taking a sample of full df + creating binary feature for nation group

indiv_barri73_20220218_final %>%
  #filter(sampling <= 0.05) %>% 
  filter(!is.na(perc_ethnic)) %>% 
  mutate(binary_nation = case_when(nation == "Latino" ~ 1,
                                   TRUE ~ 0)) -> full_sample


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