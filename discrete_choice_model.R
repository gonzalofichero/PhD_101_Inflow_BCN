# Loading necessary libraries 
library(tidyverse)


# Loading data:

  # Inflow to BCN
  inflow <- read_delim("inflows_BCN_2011_2018.txt", 
                       delim = "\t", col_names = TRUE)
  # Grouping Barris with Toni?s definition (for lower count cases)
  grup_barris_table <- read_delim("tabla_conversion_agrupado_barrios.txt", 
                                  delim = "\t", col_names = TRUE)
  # Inflow "by defect" to see if the numbers check with Toni's
  other_inflow <- read_delim("altas_omision_BCN_2011_2018.txt", 
                                  delim = "\t", col_names = TRUE)
  
# Checking
glimpse(inflow)
glimpse(grup_barris_table)
glimpse(other_inflow)
  
  
# Grouping by into Europeans vs Latinos vs East Europeans:
inflow %>% 
    mutate(nation = case_when(as.character(NOM_Naix) %in% c("Resta Unió Europea", "Itàlia", "França", "Regne Unir", "Alemanya") ~ "European",
                              as.character(NOM_Naix) %in% c("Geòrgia", "Resta Europa", "Romania", "Rússia", "Ucraïna") ~ "European.East",
                              as.character(NOM_Naix) %in% c("Argentina", "Veneçuela", "Colòmbia", "Brasil", "Mèxic", "Xile", "Perú", "Equador", "República Dominicana", "Hondures", "Uruguai", "Bolívia", "Paraguai", "Cuba", "Resta América") ~ "Latino",
                              TRUE ~ as.character(NOM_Naix))) -> inflow

other_inflow %>% 
  mutate(nation = case_when(as.character(NOM_Naix) %in% c("Resta Unió Europea", "Itàlia", "França", "Regne Unir", "Alemanya") ~ "European",
                            as.character(NOM_Naix) %in% c("Geòrgia", "Resta Europa", "Romania", "Rússia", "Ucraïna") ~ "European.East",
                            as.character(NOM_Naix) %in% c("Argentina", "Veneçuela", "Colòmbia", "Brasil", "Mèxic", "Xile", "Perú", "Equador", "República Dominicana", "Hondures", "Uruguai", "Bolívia", "Paraguai", "Cuba", "Resta América") ~ "Latino",
                            TRUE ~ as.character(NOM_Naix))) -> other_inflow


# Adding Grouped Barris from Toni's definition to compare flows:
inflow %>% 
  rename(BARRI_COD = BARRI_dest) %>% 
  mutate(BARRI_COD = as.factor(str_pad(BARRI_COD, width=2, side="left", pad="0"))) %>% 
  left_join(grup_barris_table %>% select(BARRI_COD,BARRI_AGRUP_TONI), by = "BARRI_COD") -> inflow2

other_inflow %>% 
  rename(BARRI_COD = BARRI_dest) %>% 
  mutate(BARRI_COD = as.factor(str_pad(BARRI_COD, width=2, side="left", pad="0"))) %>% 
  left_join(grup_barris_table %>% select(BARRI_COD,BARRI_AGRUP_TONI), by = "BARRI_COD") -> other_inflow2


# Need to filter for Latinos + Europeans, 24 to 40 yos, University degree or higher, 
# for 2018
table(inflow2$Nivell_educ)
table(inflow2$Edat_q)

inflow2 %>% 
  filter(Nivell_educ == "Estudis universitaris / CFGS grau superior") %>% 
  filter(nation %in% c("European","Latino")) %>% 
  filter(Edat_q %in% c("25 - 29", "30 - 34", "35 - 39")) %>% 
  filter(Any %in% c(2016,2017,2018)) %>% 
  group_by(Any, nation) %>% 
  summarise(total = sum(Casos, na.rm=TRUE))


other_inflow2 %>% 
  filter(Nivell_educ == "Estudis universitaris / CFGS grau superior") %>% 
  filter(nation %in% c("European","Latino")) %>% 
  filter(Edat_q %in% c("25 - 29", "30 - 34", "35 - 39")) %>% 
  filter(Any %in% c(2016,2017,2018)) %>% 
  group_by(Any, nation) %>% 
  summarise(total = sum(Casos, na.rm=TRUE))


# After checking that the numbers (more or less) sum up to what I'm expecting, I can add both bases together, after filters
inflow2 %>%
  filter(Nivell_educ == "Estudis universitaris / CFGS grau superior") %>% 
  filter(nation %in% c("European","Latino")) %>% 
  filter(Edat_q %in% c("25 - 29", "30 - 34", "35 - 39")) %>% 
  filter(Any %in% c(2016,2017,2018)) %>%
  select(Any, Sexe, BARRI_COD, nation, NOM_Naix, Nom_Barri_dest, BARRI_AGRUP_TONI, Casos) %>%
  group_by(Any, Sexe, BARRI_COD, nation, NOM_Naix, Nom_Barri_dest, BARRI_AGRUP_TONI) %>% 
  summarise(inflow = sum(Casos, na.rm=TRUE)) -> inflow_filtered

other_inflow2 %>% 
  filter(Nivell_educ == "Estudis universitaris / CFGS grau superior") %>% 
  filter(nation %in% c("European","Latino")) %>% 
  filter(Edat_q %in% c("25 - 29", "30 - 34", "35 - 39")) %>% 
  filter(Any %in% c(2016,2017,2018)) %>%
  select(Any, Sexe, BARRI_COD, nation, NOM_Naix, Nom_Barri_dest, BARRI_AGRUP_TONI, Casos) %>% 
  group_by(Any, Sexe, BARRI_COD, nation, NOM_Naix, Nom_Barri_dest, BARRI_AGRUP_TONI) %>% 
  summarise(inflow = sum(Casos, na.rm=TRUE)) -> other_inflow_filtered



all_inflow <- rbind(inflow_filtered, other_inflow_filtered)

# Removing non necessary data from RAM
rm(inflow, inflow2, other_inflow, other_inflow2, inflow_filtered, other_inflow_filtered)



# For the discrete choice model, I need to have individual data, even though it's repeated.
# Let's ungroup the inflow variable in more rows:

set.seed(42)
test_ungroup <- all_inflow %>% ungroup() %>% sample_n(1000)


test_ungroup %>% 
  rowwise %>% 
  mutate( identif = paste(Any,Sexe, BARRI_COD, NOM_Naix, Nom_Barri_dest, BARRI_AGRUP_TONI),
          cases = list(rep(identif,inflow))) %>%
  unnest() -> check

# Should be expecting this amount of rows in test data.frame
test_ungroup %>% summarise(total = sum(inflow))


# Now we are going for the real thing:
all_inflow %>% 
  rowwise %>% 
  mutate( identif = paste(Any,Sexe, BARRI_COD, NOM_Naix, Nom_Barri_dest, BARRI_AGRUP_TONI),
          cases = list(rep(identif,inflow))) %>%
  unnest() -> inflow_unnested

# Correct format to columns and names
inflow_unnested %>% 
  select(-c(identif, cases)) %>% 
  rename(aggregate_inflow = inflow) %>% 
  mutate(ind_inflow = 1) -> inflow_unnested


# Checking individual cases per year:
inflow_unnested %>% group_by(Any, nation) %>% summarise(total = sum(ind_inflow))
  

# Taking the Barri's characteristics from EDSD thesis
barri_char <- read_csv("bcn_full_dataset.csv", col_names = TRUE)

# Renaming to have same names in Barri coding
barri_char %>% rename(BARRI_COD = BARRI) -> barri_char

glimpse(barri_char)

# Joining both data sets: individual choices + neighborhood characteristics
disc_choice_df <- inflow_unnested %>% 
                    left_join(barri_char, by = "BARRI_COD")

# Export to 3 different files to make sure I'm gonna be able to read it in CED
write_csv(disc_choice_df, "discrete_choice_dataset.csv")
write_delim(disc_choice_df, "discrete_choice_dataset.txt", delim = "\t")
write_delim(disc_choice_df, "discrete_choice_dataset_bar.txt", delim = "|")
