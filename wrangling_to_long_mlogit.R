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

sample1 <- df_ind_barri %>% filter(sampling <= 0.1)


##########################
# First regression trial

# Generating the data format that mlogit needs
try1 <- mlogit.data(sample1, 
                    shape='long',
                    alt.var= "BARRI_COD",
                    choice = "ind_choice",
                    idx = "id_individual"
                    #,
                    #drop.index = TRUE
                    )

try2 <- dfidx(sample1, shape = "long", choice = "ind_choice",
              idx = list(c("ind_choice","id_individual")))

try2 <- dfidx(sample1, idx = c("id_individual","BARRI_COD"))

try2 <- mlogit.data(sample1, choice = "ind_choice", shape = "long", 
            alt.var = "BARRI_COD", 
            chid.var = "id_individual",
            drop.index=TRUE)


# Let´s try some mixed logit regression

mixed_reg <- mlogit(ind_choice ~ age_building + perc_left +
                      excess_uni + avg_rent_2015, try2
                    #,
                    #method = "bfgs",
                    #rpar=c(age_building = 'n', perc_left = 'n', excess_uni = 'n',
                    #       avg_rent_2015 = 'n'),
                    #R = 100, halton = NA, panel = TRUE
                    )


summary(mixed_reg)
