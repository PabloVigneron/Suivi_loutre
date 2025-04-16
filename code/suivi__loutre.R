################################################################################
###                     ANALYSE - LOUTRE - PROTOCOLE                         ###  
################################################################################

################################################################################
### Import packages
library(tidyverse)
library(gridExtra)

################################################################################
### Data importation

base_tregor <- read.table("processed_data/loutre_petit_tregor.txt", 
                   header= T,
                   stringsAsFactors = T,
                   sep ="\t")


base_lieu_de_greve <- read.table("loutre_lieu_de_greve.txt", 
                          header= T,
                          stringsAsFactors = T,
                          sep ="\t")





################################################################################
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





