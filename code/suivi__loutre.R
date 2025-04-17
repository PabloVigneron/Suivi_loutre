################################################################################
###                     ANALYSE - LOUTRE - PROTOCOLE                         ###  
################################################################################

################################################################################
### Import packages
library(tidyverse)
library(gridExtra)
library(lubridate)
library(sf)
library(mapview)
library(maptiles)
library(tidyterra)

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


texte <- "DonnÃ©es Ã©tudes Loutre GMB"
stringi::stri_enc_toascii(texte)

################################################################################
# Create a map with all sites 

sites_geo <- base_tregor %>% 
  select(nom_site:y_l93) %>% 
  sf::st_as_sf(coords = c("x_l93", "y_l93"),
               crs = sf::st_crs(2154))

mapview::mapview(sites_geo)

################################################################################
# Create a map with all presence and absence of the otters between years


sites_j25 <- sites_geo %>%
  filter(code_secteur == 'J25')

bbox_j25 <- sf::st_bbox(sites_j25)

basemap <- get_tiles(sites_j25,
                     provider = "OpenStreetMap",
                     crop = TRUE,
                     zoom = 12)
ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(
    data = sites_j25,
    aes(color = statut_observation),
    size = 1.5,
    alpha = 0.8
  ) +
  facet_wrap( ~ annee) +
  scale_color_manual(values = c("Présent" = "green4", "Absent" = "red")) +
  theme_gray() +
  theme(
    panel.border = element_rect(
      color = "transparent",
      fill = NA,
      linewidth = 0.8
    ),
    strip.background = element_rect(fill = "grey80", color = "transparent"),
    legend.background = element_rect(fill = "grey80"),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Sites de prospection, Petit Tregor", color = "Statut d'observation")

################################################################################
# Ajouter les sites non prospecté dans le graphique 


# Étape 1 : filtrer dès le début le secteur J25
base_j25 <- base_tregor %>%
  filter(code_secteur == "J25")

# Étape 2 : récupérer les coordonnées uniques des sites de J25
sites_coord <- base_j25 %>%
  select(code_site, x_l93, y_l93) %>%
  distinct()

# Étape 3 : créer toutes les combinaisons site × année pour J25
sites_all_years <- expand_grid(
  code_site = unique(base_j25$code_site),
  annee = unique(base_j25$annee)
)

# Étape 4 : joindre les données d'observation + coordonnées
sites_status_full <- sites_all_years %>%
  left_join(base_j25 %>% 
              select(code_site, annee, statut_observation),
            by = c("code_site", "annee")) %>%
  left_join(sites_coord, by = "code_site") %>%
  mutate(
    statut_final = case_when(
      statut_observation == "Présent" ~ "Présent",
      statut_observation == "Absent" ~ "Absent",
      TRUE ~ "Non prospecté"
    )
  )

# Étape 5 : convertir en sf
sites_j25_sf <- sites_status_full %>%
  st_as_sf(coords = c("x_l93", "y_l93"), crs = 2154)

# Étape 6 : générer le fond de carte centré sur les sites
basemap <- get_tiles(sites_j25_sf, provider = "OpenStreetMap", crop = TRUE, zoom = 12)

# Étape 7 : générer la carte
ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = sites_j25_sf, aes(color = statut_final), size = 1.5, alpha = 0.9) +
  facet_wrap(~ annee) +
  scale_color_manual(
    values = c("Présent" = "green4", "Absent" = "red", "Non prospecté" = "grey20")
  ) +
  labs(
    title = "Sites de prospection, Petit Trégor - Secteur J25",
    color = "Statut d'observation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "grey90"),
    legend.background = element_rect(fill = "grey95", color = "black")
  )


#################################################
# Stat descriptive

base_tregor %>% 
  filter(statut_observation =='Présent') %>% 
  ggplot(aes(x = annee)) +
    geom_bar(fill = "black") +
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



################################################################################
### FONCTION  : 
# Convert vector to factor

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


