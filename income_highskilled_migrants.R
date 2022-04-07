# Loading packages
library(tidyverse)
library(survey)


# Setting environment
setwd("C:/Users/ggarcia/Desktop/PhD GG/10 - Data/01 - Paper 1/Encuesta BCN")


# Loading data
indiv <- read_delim("r17045_ESDB_Persones_BDD_V_1_1.csv", 
                    delim = ";",
                    locale = locale(encoding = "windows-1252"))

homes <- read_delim("r17045_ESDB_Llars_BDD_V_1_1.csv",
                    delim = ";",
                    locale = locale(encoding = "windows-1252"))

rooms <- read_delim("r17045_ESDB_Habitatges_BDD_V_1_0.csv",
                    delim = ";",
                    locale = locale(encoding = "windows-1252"))


# Check
glimpse(indiv)
glimpse(homes)


# Smaller version of Home
home2 <- homes %>% select(ID_UNIT, ID_LLAR, NUMQ,
                         HAB_LLAR, TIPUS_LLAR,
                         RESUM_RESID, ESTRUCTURA_LLAR, TIPUS_LLAR_SS,
                         TIPUS_LLAR_ECV, L9_BASE_2016, L12_E,
                         GBAR, PES_GBAR, L12_E_CAL, L12, L13)

indiv_homes <- left_join(home2, indiv, by = c("ID_LLAR","ID_UNIT"))

glimpse(indiv_homes)


# Quick checks of information

hs_migrant <- indiv_homes %>% 
                filter(P16 %in% c("Post-universitaris: màster, postgrau, doctorat", 
                                  "Universitaris: graus, diplomatura, llicenciatura"))

# Create Latino vs European category
hs_migrant %>% 
mutate(nation = case_when(P4 %in% c("Alemanya", "Croàcia", "França","Àustria","Bèlgica",
                                    "Dinamarca","Eslovàquia","Finlàndia","Grècia",
                                    "Portugal","Regne Unit","República Txeca",
                                    "Irlanda","Itàlia","Letònia","Suècia",
                                    "Països Baixos", "Polònia") ~ "European",
                          P4 %in% c("Argentina", "Bolívia", "Brasil",
                                    "Colòmbia", "Costa Rica", "Cuba",
                                    #"Hondures","Nicaragua","República Dominicana",
                                    "Equador", "Mèxic", "Perú", 
                                    "Paraguai","Uruguai", "Veneçuela", "Xile") ~ "Latino",
                          P3 == "Espanya" ~ "Spanish",
                          TRUE ~ "Otros")) -> hs_migrant



hs_migrant %>% 
  group_by(nation) %>% 
  #Filtro: (1) viven hace menos de 2 años en BCN, (2) elimino outliers de Europeos
  filter(P13A>=2015, L12<10000) %>% 
  summarise(avg_salary = weighted.mean(L12, PES_GBAR.y, na.rm = T),
            median_salary = median(L12, na.rm = T))

# Si miro promedios ponderados, Latinos ganan 15% menos q Europeos UE
# Si miro medianas, Latinos ganan 11% menos que Europeos UE


hs_migrant %>% 
  filter(!is.na(L12), P13A>=2015, L12<10000) %>% 
  ggplot(aes(x=L12, color = nation)) + geom_density()



######################################################################
# Cuánta gente no está empadronada correctamente: bias de registro?
indiv_homes %>% 
  mutate(nation = case_when(P4 %in% c("Alemanya", "Croàcia", "França","Àustria","Bèlgica",
                                      "Dinamarca","Eslovàquia","Finlàndia","Grècia",
                                      "Portugal","Regne Unit","República Txeca",
                                      "Irlanda","Itàlia","Letònia","Suècia",
                                      "Països Baixos", "Polònia") ~ "European",
                            P4 %in% c("Argentina", "Bolívia", "Brasil",
                                      "Colòmbia", "Costa Rica", "Cuba",
                                      #"Hondures","Nicaragua","República Dominicana",
                                      "Equador", "Mèxic", "Perú", 
                                      "Paraguai","Uruguai", "Veneçuela", "Xile") ~ "Latino",
                            P3 == "Espanya" ~ "Spanish",
                            TRUE ~ "Otros"),
         education = case_when(P16 %in% c("Post-universitaris: màster, postgrau, doctorat", 
                                          "Universitaris: graus, diplomatura, llicenciatura") ~ "High-skilled",
                               TRUE ~ "Low-skilled"),
         padron = case_when(P11 == "Sí" ~ "Barcelona, at current residence",
                            P12 == "A Barcelona, a un altre habitatge" ~ "Barcelona, at another residence",
                            P12 == "En un municipi de Catalunya" |  P12 == "En un municipi de la resta d'Espanya" ~ "Rest of Spain",
                            P11 == "No" & P12 == "En un municipi fora d'Espanya" ~ "Foreign residence",
                            P11 == "No" & P12 == "No contesta" ~ "No answer",
                            TRUE ~ "No answer")) -> padrones 

# Faltas en Registro por Nation
prop.table(table(padrones$padron, padrones$nation),2)


# Faltas en Registro por [Nation para High-skilled
padrones %>% filter(education == "High-skilled") -> padron_hs
prop.table(table(padron_hs$padron, padron_hs$nation),2)

# No hay cambio significativo por Nivel educativo: (1) 15% mal registro Europeos, (2) 4/5% mal registro Latinos
