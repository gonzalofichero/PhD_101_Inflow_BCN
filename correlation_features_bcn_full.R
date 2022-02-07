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





######################################################
# Correlations when using PCA for control variables

# Selecting the features that I'm using in full model
indiv_barri73_fixed %>% 
  select(BARRI_COD, avg_rent_2015, cultural_pop, bars,
         Time_bike_Barceloneta) %>% 
  unique() %>% 
  left_join(control_vars_pca, by = "BARRI_COD") %>% 
  left_join(amenities_pca, by = "BARRI_COD") -> article1_correlations_pca


##### Correlation 1to1 for Full variables + PCAs #####
p.mat.pca <- cor.mtest(article1_correlations_pca %>% select(avg_rent_2015, cultural_pop, bars,
                                                            Time_bike_Barceloneta, control_pca_factor1,
                                                            control_pca_factor2,
                                                            control_pca_factor3))

corrplot(cor(as.matrix(article1_correlations_pca %>% select(avg_rent_2015, cultural_pop, bars,
                                                            Time_bike_Barceloneta, control_pca_factor1,
                                                            control_pca_factor2,
                                                            control_pca_factor3))), 
         type="upper", p.mat = p.mat.pca, sig.level = 0.01, insig = "blank")


##### Correlation 1to1 for Full PCAs #####
p.mat.pca2 <- cor.mtest(article1_correlations_pca %>% select(avg_rent_2015, amenities_pc, control_pca_factor1,
                                                            control_pca_factor2,
                                                            control_pca_factor3))

corrplot(cor(as.matrix(article1_correlations_pca %>% select(avg_rent_2015, amenities_pc, control_pca_factor1,
                                                            control_pca_factor2,
                                                            control_pca_factor3))), 
         type="upper", p.mat = p.mat.pca2, sig.level = 0.01, insig = "blank")
