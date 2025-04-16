################################################################################
###                     ANALYSE - LOUTRE - PROTOCOLE                         ###  
################################################################################

################################################################################
### Import packages
library(tidyverse)
library(gridExtra)
library(lubridate)

################################################################################
### Data importation
base_tregor <-
  readxl::read_xlsx(path = 'raw_data/export_suiviloutrelocal_telecharger_csv_2025_04_03_10h58m17.xlsx') %>%
  select(
    id_dataset,
    code_secteur,
    code_site,
    nom_site,
    x_l93,
    y_l93,
    date_visite,
    observateurs,
    condition_prospection,
    nom_taxon,
    nom_complet_taxon,
    techn_observation,
    statut_observation,
    nb_ep_tot,
    nb_ep_w,
    nb_ep_dnf,
    nb_ep_df
  ) %>%
  mutate(
    annee = year(date_visite),
    mois = month(date_visite),
    jour = day(date_visite),
    nb_ep_w = abs(nb_ep_w),
    statut_observation = ifelse(str_detect(statut_observation, '^Pr'), yes = 'Présent', no = 'Absent')
  ) %>% 
  filter(nom_complet_taxon =='Lutra lutra',
         !(id_dataset == 85 & code_secteur == "FR5300006"),
         code_secteur != "J401")




base_tregor <- read.table("processed_data/loutre_petit_tregor.txt", 
                   header= T,
                   stringsAsFactors = T,
                   sep ="\t")


base_lieu_de_greve <- read.table("loutre_lieu_de_greve.txt", 
                          header= T,
                          stringsAsFactors = T,
                          sep ="\t")



texte <- "DonnÃ©es Ã©tudes Loutre GMB"
stringi::stri_enc_toascii(texte)

################################################################################

sites_geo <- base_tregor %>% 
  select(nom_site:y_l93) %>% 
  sf::st_as_sf(coords = c("x_l93", "y_l93"),
               crs = sf::st_crs(2154))

mapview::mapview(sites_geo)




#################################################
# Stat descriptive

base_tregor %>% 
  filter(statut_observation =='Présent') %>% 
  ggplot(aes(x = annee)) +
    geom_bar(fill = "red") +
    labs(x = "Année", y = "Nombre d'observations de présence")
  
base_tregor %>% 
  ggplot(aes(x = annee, fill = statut_observation)) +
  geom_bar() +
  labs(x = "Année",
       y = "Nombre d'observations de présence",
       title = "Côtiers entre la baie de Morlaix et la baie de Lannion",
       fill = "Observation")

base_tregor %>% 
  filter(code_secteur == 'J25') %>% 
  ggplot(aes(x = date_visite,
             y = code_site,
             col = statut_observation)) +
  geom_point() +
  labs(x = "Année",
       y = "Site",
       title = "Côtiers entre la baie de Morlaix et la baie de Lannion",
       fill = "Observation") 

summary(base_tregor$date_visite)

















### Verification importation dataset
summary(base_tregor)

summary(base_lieu_de_greve)


### Anomalies 
which (base_tregor$nb_ep_w == -4)# rows : 439 465


### Delete remove inconsistent data

base_tregor_modif <- base_tregor %>%
  filter(!(id_dataset == 85 & code_secteur == "FR5300006")) 



################################################################################
### Convert vector to factor

convert_to_factor <- function(dataframe, n_col, col_indices) {
  # Vérification : si les indices sont valides
  if (any(col_indices > n_col)) {
    stop("Certains indices de colonnes dépassent le nombre total de colonnes.")
  }
  
  # Pour chaque colonne indiquée, on convertit en facteur
  for (i in col_indices) {
    dataframe[[i]] <- as.factor(dataframe[[i]])
  }
  
  return(dataframe)
}

base_tregor_convert <- convert_to_factor(base_tregor, 17,  c(1:12))
summary(base_tregor_convert)

base_lieu_de_greve_convert <- convert_to_factor(base_lieu_de_greve, 20,  c(1:15))
summary(base_lieu_de_greve_convert)


################################################################################
#In base_tregor$Statut_observation change Ne_Sais_Pas" to "0"

which(base_tregor_convert$statut_observation == "Ne_Sait_Pas") # 432

base_tregor_convert <- base_tregor_convert %>%
  mutate(statut_observation = fct_recode(statut_observation, "0" = "Ne_Sait_Pas"))

summary(base_tregor_convert$statut_observation)


################################################################################
### Creation data set with just Lutra_Lutra

tab_lut <- base_tregor_convert %>%
  filter(code_espece == "Lut_lut") 
 
summary(tab_lut)

# Base_tregor_convert : separate the dataset into two sector-specific datasets

tab_lut_petit_tregor <- tab_lut %>%
  filter(id_dataset == 85) 

tab_lut_test_franck <- tab_lut %>%
  filter(id_dataset == 72) 

###############################################################################
### Statistical description 



### Site Petit tregor 

#Define colors 
colors <- c("purple3", "turquoise")

ggplot(tab_lut_petit_tregor, aes(x = factor(Annee), fill = statut_observation)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = colors) +
  labs(title = "Statut des observations par année",
       x = "Année",
       y = "Nombre d'observations",
       fill = "Statut observation") +
  theme_minimal()


ggplot(tab_lut_petit_tregor, aes(x = factor(code_site), fill = statut_observation)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = colors) +
  labs(title = "Statut des observations par année",
       x = "Année",
       y = "Nombre d'observations",
       fill = "Statut observation") +
  theme_minimal()



# Delete NA rows

data_clean <- na.omit(tab_lut_petit_tregor)





