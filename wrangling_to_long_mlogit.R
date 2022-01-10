library(tidyverse)
library(mlogit)

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


logit_euro73a <- mlogit(ind_choice ~ sum_old + mean_int_migration + age_building + perc_left | 0, data = r_euro73)






# Results
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



# All results together in 1 big table
library(stargazer)

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


