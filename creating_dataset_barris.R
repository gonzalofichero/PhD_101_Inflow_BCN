


############################################################
# First part: create 3 data sets
# (a) all 73 barris
# (b) 69 barris from grouping with Toni's definition
# (c) 68/9 barris from droping barris with low inflow (<10)


# Loading library for data wrangling
library(tidyverse)


setwd("C:/Users/ggarcia/Desktop/PhD GG/10 - Data/01 - Paper 1")

# Full data set
bcn <- read_delim("discrete_choice_dataset_bar.txt", 
                  delim = "|", col_names = TRUE)


# Grouping Barris with Toni's definition (for lower count cases)
grup_barris_table <- read_delim("tabla_conversion_agrupado_barrios.txt", 
                                delim = "\t", col_names = TRUE)

# Correcting wrong join in previous script "discrete_choice_model.R"
bcn %>% 
  select(-BARRI_AGRUP_TONI) %>% 
  left_join(grup_barris_table, by ="BARRI_COD") -> bcn2


##############################################
# Adding new variables for analysis: 20210121

bcn2 %>% 
  left_join(ethnic_composition, by = c("BARRI_COD", "NOM_Naix")) %>% 
  left_join(cultural_barri, by = "BARRI_COD") %>% 
  left_join(beach, by = "BARRI_COD") -> bcn3
              


# Dataset 1: 73 barris

bcn3 %>% 
  filter(!is.na(NOM)) -> bcn73


# Dataset 2: 69 barris, Toni's group_by

# Barris that don't change
bcn_original <- bcn3 %>% 
                filter(!is.na(Nom_Barri_dest)) %>% 
                  filter(NOM == BARRI_AGRUP_TONI) %>% 
                  select(Any, Sexe, BARRI_COD, nation, NOM_Naix,
                         ind_inflow, BARRI_AGRUP_TONI,
                         Poblacio, income, mesas, bars,
                         age_building, size_barri_m2,
                         perc_left, perc_indep, excess_uni, perc_domi_uni_25_40,
                         Cinemas, Teatres, avg_rent_2015, sum_old,
                         perc_ethnic, cultural_pop, Time_bike_Barceloneta,
                         dist_bike_barceloneta
                         )

# Barris that have to be grouped
bcn_toni <- bcn3 %>% 
              filter(!is.na(NOM)) %>% 
              filter(NOM != BARRI_AGRUP_TONI)


bcn_toni %>% 
  select(NOM, BARRI_AGRUP_TONI, Poblacio, 
         income, mesas, bars, age_building, median_size_flat,
         size_barri_m2, perc_left, perc_indep, excess_uni,
         perc_domi_uni_25_40, Cinemas, Teatres, avg_rent_2015,
         sum_old, perc_ethnic, cultural_pop, Time_bike_Barceloneta,
         dist_bike_barceloneta) %>% 
  #select(NOM, BARRI_AGRUP_TONI, Poblacio) %>% 
  unique() -> barris_toni
  

# No more Excel solution :D
barris_toni %>% 
  group_by(BARRI_AGRUP_TONI) %>% 
  summarize(
    income = weighted.mean(income,Poblacio),
    mesas = sum(mesas),
    bars = sum(bars),
    age_building = weighted.mean(age_building,Poblacio),
    size_barri_m2 = sum(size_barri_m2), 
    perc_left = weighted.mean(perc_left,Poblacio),
    perc_indep = weighted.mean(perc_indep,Poblacio),
    excess_uni = weighted.mean(excess_uni,Poblacio),
    perc_domi_uni_25_40 = weighted.mean(perc_domi_uni_25_40,Poblacio),
    Cinemas = sum(Cinemas),
    Teatres = sum(Teatres),
    avg_rent_2015 = weighted.mean(avg_rent_2015,Poblacio),
    sum_old = weighted.mean(sum_old,Poblacio),
    perc_ethnic = weighted.mean(perc_ethnic,Poblacio),
    cultural_pop = weighted.mean(cultural_pop,Poblacio),
    Time_bike_Barceloneta = weighted.mean(Time_bike_Barceloneta,Poblacio),
    dist_bike_barceloneta = weighted.mean(dist_bike_barceloneta,Poblacio),
    Poblacio = sum(Poblacio),
    ) %>% 
  ungroup() -> bcn_toni_grouped



# Now it's time to put everything together
bcn_toni %>% 
  select(Any, Sexe, BARRI_COD, nation, NOM_Naix,
         ind_inflow, BARRI_AGRUP_TONI) %>% 
  left_join(bcn_toni_grouped, by = "BARRI_AGRUP_TONI") -> bcn_toni_final


bcn69 <- rbind(bcn_original, bcn_toni_final)


# DEPRECATED: NO DIFFERENCE BETWEEN THIS BARRIS AND TONI'S GROUPING
# Dataset 3: 68 barris, low inflow Barris kicked out

# bcn2 %>% 
#   filter(!is.na(NOM)) %>% 
#   group_by(NOM, nation) %>% 
#   summarise(inflow = n()) %>% 
#   filter(inflow <= 10)
# 
# bcn2 %>% 
#   filter(!is.na(NOM)) %>% 
#   filter(!NOM %in% c("Baró de Viver", "la Clota", 
#                      "la Marina del Prat Vermell - AEI Zona Franca (2)",
#                      "Vallbona")) -> bcn68





####################################################################
# Second part: it's time to create mlogit datasets for each bcnXX

library(mlogit)

##########################
# bcn73 to mlogit frame

# Generate individual index
bcn73 %>% 
  mutate(id_individual = row_number()) %>% 
  add_column(sampling = runif(nrow(.))) -> bcn73

# Generate dataframe with all crossings between individual and Barri
bcn73 %>% expand(id_individual, BARRI_COD) -> master73

# Now I need to join information at individual level
master73 %>% 
  left_join(bcn73 %>% 
              select(id_individual, Sexe, NOM_Naix, nation, perc_ethnic, BARRI_COD, sampling) %>% 
              rename(choice = BARRI_COD), 
            by = "id_individual") -> indiv73

# Now I need to join information at Barri level
indiv73 %>% 
  left_join(bcn73 %>% 
              select(BARRI_COD, income, mesas, bars,
                     age_building, perc_left, perc_indep,
                     excess_uni, perc_domi_uni_25_40,
                     avg_rent_2015, sum_old,
                     cultural_pop, Time_bike_Barceloneta,
                     dist_bike_barceloneta
                     ) %>% 
              unique(),
            by = "BARRI_COD"
  ) -> indiv_barri73


# Create logical feature for selection of Barri
indiv_barri73 %>% 
  mutate(ind_choice = case_when(BARRI_COD == choice ~ TRUE,
                                TRUE ~ FALSE)) -> indiv_barri73



# ##########################
# # bcn68 to mlogit frame
# 
# # Generate individual index
# bcn68 %>% 
#   mutate(id_individual = row_number()) %>% 
#   add_column(sampling = runif(nrow(.))) -> bcn68
# 
# # Generate dataframe with all crossings between individual and Barri
# bcn68 %>% expand(id_individual, BARRI_COD) -> master68
# 
# # Now I need to join information at individual level
# master68 %>% 
#   left_join(bcn68 %>% 
#               select(id_individual, Sexe, nation, BARRI_COD, sampling) %>% 
#               rename(choice = BARRI_COD), 
#             by = "id_individual") -> indiv68
# 
# # Now I need to join information at Barri level
# indiv68 %>% 
#   left_join(bcn68 %>% 
#               select(BARRI_COD, income, mesas, bars,
#                      age_building, median_size_flat, perc_left, perc_indep,
#                      excess_uni, perc_domi_uni_25_40,
#                      Cinemas, Teatres,
#                      avg_rent_2015, sum_old,
#                      mean_int_migration, European_stock, Latino_stock) %>% 
#               distinct(),
#             by = "BARRI_COD"
#   ) -> indiv_barri68
# 
# 
# # Create logical feature for selection of Barri
# indiv_barri68 %>% 
#   mutate(ind_choice = case_when(BARRI_COD == choice ~ TRUE,
#                                 TRUE ~ FALSE)) -> indiv_barri68




##########################
# bcn69 to mlogit frame

# Generate individual index
bcn69 %>% 
  mutate(id_individual = row_number()) %>% 
  add_column(sampling = runif(nrow(.))) -> bcn69

# Generate dataframe with all crossings between individual and Barri
bcn69 %>% expand(id_individual, BARRI_COD) -> master69

# Now I need to join information at individual level
master69 %>% 
  left_join(bcn69 %>% 
              select(id_individual, Sexe, NOM_Naix, nation, perc_ethnic, BARRI_COD, sampling) %>% 
              rename(choice = BARRI_COD), 
            by = "id_individual") -> indiv69

# Now I need to join information at Barri level
indiv69 %>% 
  left_join(bcn69 %>% 
              select(BARRI_COD, income, mesas, bars,
                     age_building, perc_left, perc_indep,
                     excess_uni, perc_domi_uni_25_40,
                     avg_rent_2015, sum_old,
                     cultural_pop, Time_bike_Barceloneta,
                     dist_bike_barceloneta) %>% 
              distinct(),
            by = "BARRI_COD"
  ) -> indiv_barri69


# Create logical feature for selection of Barri
indiv_barri69 %>% 
  mutate(ind_choice = case_when(BARRI_COD == choice ~ TRUE,
                                TRUE ~ FALSE)) -> indiv_barri69




##########################################
# Time to export files to continue later

write_delim(indiv_barri73, "mlogit_bcn73_full.txt", delim = "|")
write_delim(indiv_barri69, "mlogit_bcn69_full.txt", delim = "|")
