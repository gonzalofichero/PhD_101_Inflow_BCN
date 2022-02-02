library(tidyverse)
library(mlogit)
library(stargazer)

setwd("C:/Users/ggarcia/Desktop/PhD GG/10 - Data/01 - Paper 1")

bcn <- read_delim("discrete_choice_dataset_bar.txt", 
                     delim = "|", col_names = TRUE)

glimpse(bcn)

# Generate individual index
bcn %>% 
  mutate(id_individual = row_number()) %>% 
  add_column(sampling = runif(nrow(.))) -> bcn

sample1 <- bcn %>% 
            filter(sampling <= 0.1) %>% 
            select(BARRI_COD, 
                   Sexe, nation, 
                   median_size_flat, perc_left, excess_uni, avg_rent_2015, mean_int_migration) %>% 
            mutate(BARRI_COD = as.factor(BARRI_COD))

reg_choice <- Rchoice(BARRI_COD ~ Sexe + nation + median_size_flat + perc_left + excess_uni + avg_rent_2015,
                      data = sample1, family = ordinal("logit"))

summary(reg_choice)


library(nnet)

reg_multi <- multinom(BARRI_COD ~ nation + perc_left + avg_rent_2015,
                      data = sample1, model = TRUE)

summary(reg_multi)



# Generate dataframe with all crossings between individual and Barri
bcn %>% expand(id_individual, BARRI_COD) -> master_df

# Now I need to join information at individual level
master_df %>% 
  left_join(bcn %>% 
              select(id_individual, Sexe, nation, BARRI_COD, sampling) %>% 
              rename(choice = BARRI_COD), 
            by = "id_individual") -> df_indiv

# Now I need to join information at Barri level
df_indiv %>% 
  left_join(bcn %>% 
              select(BARRI_COD, age_building, median_size_flat, perc_left,
                     excess_uni, avg_rent_2015, sum_old,
                     mean_int_migration, European_stock, Latino_stock) %>% 
              distinct(),
            by = "BARRI_COD"
  ) -> df_ind_barri


# Create logical feature for selection of Barri
df_ind_barri %>% 
  mutate(ind_choice = case_when(BARRI_COD == choice ~ TRUE,
                                TRUE ~ FALSE)) -> df_ind_barri

df_ind_barri %>% 
  filter(!is.na(BARRI_COD)) -> df_ind_barri

sample_eur <- df_ind_barri %>% 
            filter(nation == "European") %>% 
            filter(sampling <= 0.1)

sample_lat <- df_ind_barri %>% 
                filter(nation == "Latino") %>% 
                filter(sampling <= 0.1)

sample1 <- sample1 %>% left_join(grup_barris_table, by = "BARRI_COD")

sample <- sample1 %>%  mutate(Sexe = as.factor(Sexe),
                               nation = as.factor(nation))



##############################################
# First regression trial: no mixed logit



# Importing the data
indiv_barri73 <- read_delim("mlogit_bcn73.txt", delim="|")
indiv_barri69 <- read_delim("mlogit_bcn69.txt", delim="|")
indiv_barri68 <- read_delim("mlogit_bcn68.txt", delim="|")


# Splitting between Europeans and Latinos
euro_73 <- indiv_barri73 %>% filter(nation=="European")
latino_73 <- indiv_barri73 %>% filter(nation=="Latino")

euro_69 <- indiv_barri69 %>% filter(nation=="European")
latino_69 <- indiv_barri69 %>% filter(nation=="Latino")

euro_68 <- indiv_barri68 %>% filter(nation=="European")
latino_68 <- indiv_barri68 %>% filter(nation=="Latino")


# Generating the data format that mlogit needs
r_euro73 <- dfidx(euro_73, shape = "long",
              alt.var = "BARRI_COD",
              chid.var = "choice",
              idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)

r_lat73 <- dfidx(latino_73, shape = "long",
                 alt.var = "BARRI_COD",
                 chid.var = "choice",
                 idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)



r_euro69 <- dfidx(euro_69, shape = "long",
                  alt.var = "BARRI_COD",
                  chid.var = "choice",
                  idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)

r_lat69 <- dfidx(latino_69, shape = "long",
                 alt.var = "BARRI_COD",
                 chid.var = "choice",
                 idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)


r_euro68 <- dfidx(euro_68, shape = "long",
                  alt.var = "BARRI_COD",
                  chid.var = "choice",
                  idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)

r_lat68 <- dfidx(latino_68, shape = "long",
                 alt.var = "BARRI_COD",
                 chid.var = "choice",
                 idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)



# Let's try some logit regression

###########################################
# For 73 barris, both nation groups
logit_euro73 <- mlogit(ind_choice ~ age_building + perc_left + avg_rent_2015 + bars | 0, data = r_euro73)

summary(logit_euro73)


logit_lat73 <- mlogit(ind_choice ~ age_building + perc_left + avg_rent_2015 + bars | 0, data = r_lat73)

summary(logit_lat73)


# Stepwise process for 73 barris
r_euro73 %>% mutate(cultural = Teatres + Cinemas) -> r_euro73
r_lat73 %>% mutate(cultural = Teatres + Cinemas) -> r_lat73

# Europeans
logit_euro73a <- mlogit(ind_choice ~ sum_old + age_building + perc_left | 0, data = r_euro73)
logit_euro73b <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 | 0, data = r_euro73)
logit_euro73c <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars | 0, data = r_euro73)
logit_euro73d <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 | 0, data = r_euro73)
logit_euro73e <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni | 0, data = r_euro73)
logit_euro73f <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni + cultural | 0, data = r_euro73)



# Latinos
logit_lat73a <- mlogit(ind_choice ~ sum_old + age_building + perc_left | 0, data = r_lat73)
logit_lat73b <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 | 0, data = r_lat73)
logit_lat73c <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars | 0, data = r_lat73)
logit_lat73d <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 | 0, data = r_lat73)
logit_lat73e <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni | 0, data = r_lat73)
logit_lat73f <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni + cultural | 0, data = r_lat73)


# Results
stargazer(logit_euro73a, logit_euro73b, 
          logit_euro73c, logit_euro73d,
          logit_euro73e, logit_euro73f,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Avg Rent", "Bars per population", 
                               "Unitary Households", "University Population",
                               "Cultural Equipment"),
          column.labels=c("1", "2", "3", "4", "5", "6"),
          dep.var.labels = c("","","","", "", ""),
          type = "html", out="logit_euro73.html")


stargazer(logit_lat73a, logit_lat73b, 
          logit_lat73c, logit_lat73d,
          logit_lat73e, logit_lat73f,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Avg Rent", "Bars per population", 
                               "Unitary Households", "University Population",
                               "Cultural Equipment"),
          column.labels=c("1", "2", "3", "4", "5", "6"),
          dep.var.labels = c("","","","", "", ""),
          type = "html", out="logit_lat73.html")


###########################################
# For 69 barris, both nation groups
logit_euro69 <- mlogit(ind_choice ~ age_building + perc_left + avg_rent_2015 + bars | 0, data = r_euro69)

summary(logit_euro69)


logit_lat69 <- mlogit(ind_choice ~ age_building + perc_left + avg_rent_2015 + bars | 0, data = r_lat69)

summary(logit_lat69)


###########################################
# For 68 barris, both nation groups
logit_euro68 <- mlogit(ind_choice ~ age_building + perc_left + avg_rent_2015 + bars | 0, data = r_euro68)

summary(logit_euro68)


logit_lat68 <- mlogit(ind_choice ~ age_building + perc_left + avg_rent_2015 + bars | 0, data = r_lat68)

summary(logit_lat68)


# Stepwise process for 68 barris
r_euro68 %>% mutate(cultural = Teatres + Cinemas) -> r_euro68
r_lat68 %>% mutate(cultural = Teatres + Cinemas) -> r_lat68

# Europeans
logit_euro68a <- mlogit(ind_choice ~ sum_old + age_building + perc_left | 0, data = r_euro68)
logit_euro68b <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 | 0, data = r_euro68)
logit_euro68c <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars | 0, data = r_euro68)
logit_euro68d <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 | 0, data = r_euro68)
logit_euro68e <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni | 0, data = r_euro68)
logit_euro68f <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni + cultural | 0, data = r_euro68)



# Latinos
logit_lat68a <- mlogit(ind_choice ~ sum_old + age_building + perc_left | 0, data = r_lat68)
logit_lat68b <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 | 0, data = r_lat68)
logit_lat68c <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars | 0, data = r_lat68)
logit_lat68d <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 | 0, data = r_lat68)
logit_lat68e <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni | 0, data = r_lat68)
logit_lat68f <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni + cultural | 0, data = r_lat68)


# Results
stargazer(logit_euro68a, logit_euro68b, 
          logit_euro68c, logit_euro68d,
          logit_euro68e, logit_euro68f,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Avg Rent", "Bars per population", 
                               "Unitary Households", "University Population",
                               "Cultural Equipment"),
          column.labels=c("1", "2", "3", "4", "5", "6"),
          dep.var.labels = c("","","","", "", ""),
          type = "html", out="logit_euro68.html")


stargazer(logit_lat68a, logit_lat68b, 
          logit_lat68c, logit_lat68d,
          logit_lat68e, logit_lat68f,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Avg Rent", "Bars per population", 
                               "Unitary Households", "University Population",
                               "Cultural Equipment"),
          column.labels=c("1", "2", "3", "4", "5", "6"),
          dep.var.labels = c("","","","", "", ""),
          type = "html", out="logit_lat68.html")




##################################################################
# 73 vs 68 barris, both groups, all features models - comparison

stargazer(logit_euro68f, logit_euro73f, 
          logit_lat73f, logit_lat68f,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Avg Rent", "Bars per population", 
                               "Unitary Households", "University Population",
                               "Cultural Equipment"),
          column.labels=c("Euro-68", "Euro-73", "Lat-73", "Lat-68"),
          dep.var.labels = c("","","",""),
          type = "html", out="logit_68vs73.html")



########################################################
# All results (3 groups barris) together in 1 big table


stargazer(logit_euro73, logit_lat73, 
          #logit_euro69, logit_lat69,
          logit_euro68, logit_lat68,
          covariate.labels = c("Avg Age of Building" , "Left Wing votes (municipal elections)",
                               "Avg Rent", "Bars per population"),
          column.labels=c("Euro inf (73b)", "Latino inf (73b)",
                          #"Euro inf (69b)", "Latino inf (69b)",
                          "Euro inf (68b)", "Latino inf (68b)"
                          ),
          dep.var.labels = c("","","",""),
          type = "html", out="comparing_results_logit.html")




#################################################
# Mixed logit models for both groups, 73 barris

logit_lat73_mixed <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni + cultural | 0, 
                       data = r_lat73,
                       rpar = c(sum_old = "n", age_building = "n", perc_left = "n",
                                avg_rent_2015 = "n", bars = "n", perc_domi_uni_25_40 = "n",
                                excess_uni = "n", cultural = "n"),
                       correlation = TRUE, R = 100, halton = NA)

summary(logit_lat73_mixed)


logit_euro73_mixed <- mlogit(ind_choice ~ sum_old + age_building + perc_left + avg_rent_2015 + bars + perc_domi_uni_25_40 + excess_uni + cultural | 0, 
                            data = r_euro73,
                            rpar = c(sum_old = "n", age_building = "n", perc_left = "n",
                                     avg_rent_2015 = "n", bars = "n", perc_domi_uni_25_40 = "n",
                                     excess_uni = "n", cultural = "n"),
                            correlation = TRUE, R = 100, halton = NA)

summary(logit_euro73_mixed)



#################################################
# Histogram of independent variables: N or LN?

bcn %>% 
  filter(!is.na(BARRI_COD)) %>% 
  select(BARRI_COD, sum_old, age_building, 
         perc_left, avg_rent_2015, bars, perc_domi_uni_25_40,
         excess_uni, Teatres, Cinemas) %>% 
  group_by(BARRI_COD) %>% 
  distinct() %>% 
  mutate(cultural = Teatres + Cinemas) -> features_barris

features_barris %>% 
  ggplot(aes(x=sum_old)) + geom_histogram() +
  labs(y="count", x = "Avg Age Padron")





##########################################
# Testing hypothesis H1, H2a and H2b


# First with the 73 barris
indiv_barri73 <- read_delim("mlogit_bcn73_full.txt", 
                  delim = "|", col_names = TRUE)



#############################
# CHECK: NAs in Airbnb data

indiv_barri73 %>% 
  filter(is.na(airbnbs))  %>% 
  group_by(BARRI_COD) %>% 
  summarise(counts = n())

# Barri 56 coming with NAs, when should be 6...
# Fixing now with a not very subtle solution, then I'll check up why coming like this

indiv_barri73 %>% 
  mutate(airbnbs = case_when(BARRI_COD == "56"  ~ 6,
                             TRUE ~ airbnbs)) ->  indiv_barri73_fixed


# Splitting into Europeans and Latinos
barri73_lat <- indiv_barri73_fixed %>% filter(nation == "Latino", !is.na(perc_ethnic))
barri73_euro <- indiv_barri73_fixed %>% filter(nation == "European", !is.na(perc_ethnic))



# Running 4 regressions for both groups

## Latinos
logit_lat73_control <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs | 0, 
                            data = barri73_lat)

logit_lat73_h2a <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + avg_rent_2015 | 0, 
                              data = barri73_lat)

logit_lat73_h2b <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + cultural_pop + bars + Time_bike_Barceloneta | 0, 
                          data = barri73_lat)

logit_lat73_h2b_pca <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + amenities_pc | 0, 
                          data = barri73_lat)

logit_lat73_h3 <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs | 0 + perc_ethnic, 
                          data = barri73_lat)

logit_lat73_full <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + avg_rent_2015 + cultural_pop + bars + Time_bike_Barceloneta | 0 + perc_ethnic, 
                          data = barri73_lat)

logit_lat73_full_pca <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + avg_rent_2015 + amenities_pc | 0 + perc_ethnic, 
                           data = barri73_lat)


stargazer(logit_lat73_control, logit_lat73_h2a, 
          logit_lat73_h2b, 
          logit_lat73_h3,
          logit_lat73_full,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Unitary Households", "University Population",
                               "#Airbnb's",
                               "Avg Rent", 
                               "Cultural Equipment", "Bars per population", "Distance to beach", 
                               "%co-ethnic component by barri"
                               ),
          column.labels=c("Control", "Economic Rest.",
                          "Amenities", "Ethnic Support", "Full"),
          dep.var.labels = c("","","","",""),
          type = "html", out="logit_latino_73.html")


stargazer(logit_lat73_control, logit_lat73_h2a, 
          logit_lat73_h2b_pca, 
          logit_lat73_h3,
          logit_lat73_full_pca,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Unitary Households", "University Population",
                               "#Airbnb's",
                               "Avg Rent", 
                               "Amenities", 
                               "%co-ethnic component by barri"
          ),
          column.labels=c("Control", "Economic Rest.",
                          "Amenities", "Ethnic Support", "Full"),
          dep.var.labels = c("","","","",""),
          type = "html", out="logit_latino_73_pca.html")


## Europeans
logit_euro73_control <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs | 0, 
                              data = barri73_euro)

logit_euro73_h2a <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + avg_rent_2015 | 0, 
                          data = barri73_euro)

logit_euro73_h2b <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + cultural_pop + bars + Time_bike_Barceloneta | 0, 
                          data = barri73_euro)

logit_euro73_h2b_pca <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + amenities_pc | 0, 
                           data = barri73_euro)

logit_euro73_h3 <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs | perc_ethnic, 
                         data = barri73_euro)
# Having singularity problem with co-ethnic for Europeans...

logit_euro73_full <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + avg_rent_2015 + cultural_pop + bars + Time_bike_Barceloneta | 0, 
                           data = barri73_euro)

logit_euro73_full_pca <- mlogit(ind_choice ~ sum_old + age_building + perc_left + perc_domi_uni_25_40 + excess_uni + airbnbs + avg_rent_2015 + amenities_pc | 0, 
                            data = barri73_euro)




stargazer(logit_euro73_control, logit_euro73_h2a, 
          logit_euro73_h2b, 
          #logit_euro73_h3,
          logit_euro73_full,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Unitary Households", "University Population",
                               "#Airbnb's",
                               "Avg Rent", 
                               "Cultural Equipment", "Bars per population", "Distance to beach", 
                               "%co-ethnic component by barri"
          ),
          # column.labels=c("Control", "Economic Rest.",
          #                 "Amenities", "Ethnic Support", "Full"),
          # dep.var.labels = c("","","","",""),
          column.labels=c("Control", "Economic Rest.",
                          "Amenities", "Full"),
          dep.var.labels = c("","","",""),
          type = "html", out="logit_euro_73.html")


stargazer(logit_euro73_control, logit_euro73_h2a, 
          logit_euro73_h2b_pca, 
          #logit_euro73_h3,
          logit_euro73_full_pca,
          covariate.labels = c("Avg Age in Padron",
                               "Avg Age of Building",
                               "Left Wing votes (municipal elections)",
                               "Unitary Households", "University Population",
                               "#Airbnb's",
                               "Avg Rent", 
                               "Amenities", 
                               "%co-ethnic component by barri"
          ),
          # column.labels=c("Control", "Economic Rest.",
          #                 "Amenities", "Ethnic Support", "Full"),
          # dep.var.labels = c("","","","",""),
          column.labels=c("Control", "Economic Rest.",
                          "Amenities", "Full"),
          dep.var.labels = c("","","",""),
          type = "html", out="logit_euro_73_pca.html")
