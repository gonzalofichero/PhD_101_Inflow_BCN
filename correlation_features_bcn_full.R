#### Checking correlations #########

# Please, run cor_mtest.R file to create cor.mtest function necessary for analysis...
library(corrplot)


# Selecting the features that I'm using in full model
indiv_barri73_fixed %>% 
  select(sum_old, age_building, perc_left,
         perc_domi_uni_25_40, excess_uni, airbnbs, 
         avg_rent_2015, cultural_pop, bars,
         Time_bike_Barceloneta) %>% 
  unique() -> article1_correlations73


##### Correlation 1to1 for European incoming #####
p.mat.barri73 <- cor.mtest(article1_correlations73)

corrplot(cor(as.matrix(article1_correlations73)), 
         type="upper", p.mat = p.mat.barri73, sig.level = 0.01, insig = "blank")
