library(tidyverse)
library(mlogit)

library(Rchoice)

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



##########################
# First regression trial

# Generating the data format that mlogit needs
try_eur <- dfidx(sample_eur, shape = "long",
              alt.var = "BARRI_COD",
              chid.var = "choice",
              idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)

try_lat <- dfidx(sample_lat, shape = "long",
                 alt.var = "BARRI_COD",
                 chid.var = "choice",
                 idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)

try_both <- dfidx(sample1, shape = "long",
                 alt.var = "BARRI_COD",
                 chid.var = "choice",
                 idx = c("id_individual", "BARRI_COD"), drop.index = FALSE)


# Let´s try some mixed logit regression

mixed_reg_eur <- mlogit(ind_choice ~ age_building + perc_left + excess_uni + avg_rent_2015 + mean_int_migration + sum_old | Sexe + 0, data = try_eur)

summary(mixed_reg_eur)


mixed_reg_lat <- mlogit(ind_choice ~ age_building + perc_left + excess_uni + avg_rent_2015 + mean_int_migration + sum_old | Sexe + 0, data = try_lat)

summary(mixed_reg_lat)



mixed_reg_both <- mlogit(ind_choice ~ age_building + perc_left + excess_uni + avg_rent_2015 + mean_int_migration + sum_old | nation + 0, data = try_both)

summary(mixed_reg_both)
