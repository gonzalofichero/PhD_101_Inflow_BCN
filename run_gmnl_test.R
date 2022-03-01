library(tidyverse)
library(mlogit)
library(gmnl)

# Loading full data file
indiv73 <- read_delim("ind_barri73_fullbeach.txt", delim = "|")

# Creating sample of 17 barris
set.seed(31416)
barri_sample <- round(runif(25, min=0.01, max = 0.73) * 100,0)



########################################################################
# Taking a sample of full df + creating binary feature for nation group

indiv73 %>%
  filter(as.numeric(BARRI_COD) %in% barri_sample) %>% 
  filter(sampling <= 0.40) %>% 
  filter(!is.na(perc_ethnic)) %>% 
  mutate(binary_nation = case_when(nation == "Latino" ~ 1,
                                   TRUE ~ 0),
         binary_sex = case_when(Sexe == "Dona" ~ 1,
                                TRUE ~ 0)) -> full_sample

full_sample %>% 
  group_by(id_individual) %>% 
  summarise(selections = sum(ind_choice)) %>% 
  filter(selections == 1) -> ind_sample


full_sample %>% 
  left_join(ind_sample, by ="id_individual") %>% 
  filter(!is.na(selections)) -> full_indiv_barri_sample



########################################################################
# Creating mlogit.data file for gmnl package needs

full_sample_73 <- mlogit.data(full_indiv_barri_sample, shape = "long",
                              choice = "ind_choice")

print(Sys.time())
full_nation_73_interaction3 <- gmnl(formula = ind_choice ~ transitory_pca + avg_rent_2015 + amenities_pca_mintime - 1 | perc_ethnic + binary_sex - 1 | 0 | binary_nation - 1,
                                   data = full_sample_73,
                                   model = "mixl",
                                   ranp = c(transitory_pca = "n", avg_rent_2015 = "n", amenities_pca_mintime = "n"),
                                   mvar = list( transitory_pca = c("binary_nation"), 
                                                avg_rent_2015 = c("binary_nation"),
                                                amenities_pca_mintime = c("binary_nation")),
                                   method = "bfgs",
                                   R = 10)
save.image()
# 2horas 32minutos con 19 alternativas + 7383 individuos
# 22horas 54 minutos con 27 alternativas + 13753 individuos

# xhoras xxminutos con yy alternativas + 5608 individuos


summary(full_nation_73_interaction)
summary(full_nation_73_interaction2)
summary(full_nation_73_interaction3)
