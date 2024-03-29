###################################
# CREATING ADDITIONAL VARIABLES FOR PAPER 1
# AFTER 20220117 MEETING WITH SUPERVISORS
# Need to create:
# (1) Cultural Equipment (Teatres + Cinemas)  per Population
# (2) Closeness to see
# (3) %co-ethnic for Nations in Latino/European groups
# (4) %renting places = Airbnb data



###################################################
# Creating co-ethnic variable, individual level

library(tidyverse)
library(readxl)

# From Ajuntament's data: #people by age, sex and country of birth for each barri

origin_bcn <- read_excel("PC_2016.xlsx", sheet = "Datos")
glimpse(origin_bcn)


origin_bcn_2 <- origin_bcn %>%
  # mutating countries to català
  mutate(nation2 = case_when(as.character(BORN) == 'ITALIA' ~ "Itàlia",
                             as.character(BORN) == 'FRANCIA' ~ "França",
                             as.character(BORN) == 'REINO UNIDO' ~ "Regne Unir",
                             as.character(BORN) == 'ALEMANIA' ~ "Alemanya",
                             as.character(BORN) %in% c('AUSTRIA','BELGICA','BULGARIA','CHIPRE','DINAMARCA','FINLANDIA',
                                                       'GRECIA','HUNGRIA','IRLANDA','LUXEMBURGO','MALTA',
                                                       'PAISES BAJOS','POLONIA','PORTUGAL',
                                                       'RUMANIA','SUECIA','LETONIA','ESTONIA','LITUANIA','REPUBLICA CHECA',
                                                       'REPUBLICA ESLOVACA','ESLOVENIA') ~ "Resta Unió Europea",
                             as.character(BORN) == 'ARGENTINA' ~ "Argentina",
                             as.character(BORN) == 'VENEZUELA' ~ "Veneçuela",
                             as.character(BORN) == 'COLOMBIA' ~ "Colòmbia",
                             as.character(BORN) == 'BRASIL' ~ "Brasil",
                             as.character(BORN) == 'MEXICO' ~ "Mèxic",
                             as.character(BORN) == 'CHILE' ~ "Xile",
                             as.character(BORN) == 'PERU' ~ "Perú",
                             as.character(BORN) == 'ECUADOR' ~ "Equador",
                             as.character(BORN) == 'REPUBLICA DOMINICANA' ~ "República Dominicana",
                             as.character(BORN) == 'HONDURAS' ~ "Hondures",
                             as.character(BORN) == 'URUGUAY' ~ "Uruguai",
                             as.character(BORN) == 'BOLIVIA' ~ "Bolívia",
                             as.character(BORN) == 'PARAGUAY' ~ "Paraguai",
                             as.character(BORN) == 'CUBA' ~ "Cuba",
                             as.character(BORN) %in% c('COSTA RICA','EL SALVADOR','GUATEMALA',
                                                       'NICARAGUA','PANAMA') ~ "Resta América",
                             TRUE ~ "Otros")) %>% 
  select(Barrio, nation2, Total) %>%
  group_by(Barrio, nation2) %>%
  summarize(stock_ethnic = sum(Total, na.rm = T)) %>% 
  ungroup() %>% 
  #mutate(Codi_Barri = as.factor(str_pad(Codi_Barri, width=2, side="left", pad="0"))) %>% 
  rename(BARRI_COD = Barrio)



# Now taking the population by barri, to have for each ethnic the % that represents in the barri
bcn %>% 
  filter(!is.na(BARRI_COD)) %>% 
  select(BARRI_COD, Poblacio) %>% 
  unique() -> pop_barri


# Putting everything together by barri and ethnic origin
origin_bcn_2 %>% 
  left_join(pop_barri, by = "BARRI_COD") %>% 
  mutate(perc_ethnic = stock_ethnic / Poblacio) %>% 
  select(-Poblacio) %>% 
  rename(NOM_Naix = nation2) -> ethnic_composition





#####################################################################
# Creating cultural equipment per pop (per 1000), alternative level
bcn %>% 
  filter(!is.na(BARRI_COD)) %>% 
  select(BARRI_COD, Poblacio, Teatres, Cinemas) %>% 
  unique() %>% 
  mutate(cultural_pop = (Teatres + Cinemas) / Poblacio *1000) %>% 
  select(BARRI_COD, cultural_pop) -> cultural_barri




#################################################################################
# Creating distance to beach variable (taken from Google Maps), alternative level
beach <- read_delim("beach_distance_BCN.txt", delim = "|")


beach %>% 
  # some problem when importing the file
  rename(dist_bike_barceloneta = `Dist_bike_Barceloneta `) %>% 
  mutate(dist_bike_barceloneta = as.double(dist_bike_barceloneta)) %>% 
  select(BARRI_COD, Time_bike_Barceloneta, dist_bike_barceloneta) -> beach






###################################################
# Waiting to be last file 20200218, adding:
# (1) more complete beach time feature (3 spots)
# (2) PCA 1 factor for transitory variable = Airbnb + Avg Padron (>0 less transient population)
# (3) exporting txt file

indiv_barri73_fixed %>% 
  select(-Time_bike_Barceloneta) %>% 
  select(-dist_bike_barceloneta) %>% 
  left_join(transitority_vars_pca, by = "BARRI_COD") %>% 
  left_join(beach, by = "BARRI_COD") -> indiv_barri73_20220218_final


write_delim(indiv_barri73_20220218_final, "indiv_barri73_final.txt", delim = "|")

