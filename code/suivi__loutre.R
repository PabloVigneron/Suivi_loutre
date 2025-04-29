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
library(RVAideMemoire)
library(emmeans)
library(car)

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
    statut_observation = ifelse(str_detect(statut_observation, '^Pr'), yes = 'Présent', no = 'Absent'), 
  ) %>% 
  filter(nom_complet_taxon =='Lutra lutra',
         !(id_dataset == 85 & code_secteur == "FR5300006"),
         code_secteur != "J401")

texte <- "DonnÃ©es Ã©tudes Loutre GMB"
stringi::stri_enc_toascii(texte)

################################################################################






################################################################################

head (base_tregor)%>%
  kable(format = "latex")

################################################################################
# Create a map with all sites 
sites_geo <- base_tregor %>% 
  select(nom_site:y_l93,
         statut_observation,
         code_secteur,
         annee) %>% 
  sf::st_as_sf(coords = c("x_l93", "y_l93"),
               crs = sf::st_crs(2154))

mapview::mapview(sites_geo, 
                 zcol = "code_secteur",
                 col.regions = c("FR5300006" = "indianred1", 
                                 "J25" = "darkseagreen3"))

################################################################################
# Create a map with all presence and absence of the otters between years

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


################################################################################
### Stat descriptive


################################################################################
# Nombre d'observations de présence de la loutre en fonction des années

base_tregor %>%
  filter(statut_observation == 'Présent') %>%
  ggplot(aes(x = annee)) +
  geom_bar(fill = "black") +
  labs(x = "Année", 
       y = "Nombre d'observations de présence de la loutre", 
       title = "Suivi des sites de prospection - Secteur Petit Tregor") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA)
  )

################################################################################
# Nombre d'observations de présence et absence de la loutre en fonction des années

base_tregor %>%
  ggplot(aes(x = annee, fill = statut_observation)) +
  geom_bar() +
  scale_fill_manual(values = c(
    "Présent" = "darkseagreen3",
    "Absent" = "indianred1"
  )) +
  labs(x = "Année",
       y = "Nombre d'observations de présence  et absence de loutre",
       title = "Suivi des sites de prospection - Secteur Petit Tregor",
       fill = "Observation") +
  theme (
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.background = element_rect(fill = "grey95", color = "black")
  )

################################################################################
# Statut d'observation des sites prospectée en fonction des années 

base_j25 <- base_j25 %>%
  mutate(annee = year(date_visite))

all_combos <- expand_grid(code_site = unique(base_j25$code_site),
                          annee = unique(base_j25$annee))

obs_complete <- all_combos %>%
  left_join(base_j25 %>% select(code_site, annee, statut_observation),
            by = c("code_site", "annee")) %>%
  mutate(
    statut_final = case_when(
      statut_observation == "Présent" ~ "Présent",
      statut_observation == "Absent" ~ "Absent",
      TRUE ~ "Non prospecté"
    )
  )

ggplot(obs_complete, aes(x = annee, y = code_site, fill = statut_final)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "Présent" = "darkseagreen3",
      "Absent" = "indianred1",
      "Non prospecté" = "grey80"
    )
  ) +
  labs(x = "Année",
       y = "Site",
       title = "Suivi des sites de prospection - Secteur Petit Tregor (J25)",
       fill = "Statut d'observation") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.background = element_rect(fill = "grey95", color = "black")
  )
##################################################################################










################################################################################
### Est ce que les conditions de prospection influe sur la présence de l'espèce ?



################################################################################
### Nombre d'épreintes total sites en fonction des années ?



################################################################################
### GLM - Le statut d'observation depend de l'année 

base_j25 <- base_j25 %>% 
  mutate(statut_presence = ifelse(as.character(statut_observation) == "Présent", 1, 0))

## Definition des types de variables
# Variable réponse : Statut présence  
# Variable explicative : 
# - Annee : facteur ordinale fixe


## Poser le modèle 
# Binaire: distribution Binomiale
# Fonction de lien : logit 

mod1 <- glm(statut_presence~annee, family=binomial(link = "logit"), data = base_j25)
mod1.1 <- glm(statut_presence~annee,quasibinomial(link = "logit"), data = base_j25)

plotresid(mod1)
plotresid(mod1.1)

# On regarde la courbe rouge : c'est ok 
summary(mod1)
summary(mod1.1)

#Residual deviance: 505.20  on 412  degrees of freedown

heteroscedasticite <- function(deviance_residuelle, dl_residuelle){
  deviance_residuelle /dl_residuelle
}

heteroscedasticite(505.2, 412)

#[1] 1.226214= > Seuil de 1, 5, remarque légère surdispersion des résidus



###ANALYSE 
Anova (mod1)
Anova (mod1.1)

###############################################################################
### GLM - Le nombre de marquage depend de l'année

## Definition des types de variables
# Variable réponse : nb_ep_tot : Quantitative discrète 
# Variable explicative : 
# - Annee : facteur ordinale fixe


## Poser le modèle 
# Comptage : distribution Poisson 
# Fonction de lien : ln

mod2 <- glm(nb_ep_tot~annee, family=poisson(link = "log"), data = base_j25)
mod2.1 <- glm(nb_ep_tot~annee, family=poisson(link = "identity"), data = base_j25)

plotresid(mod2)
plotresid(mod2.1)

# On regarde la courbe rouge : c'est ok 
summary(mod2)
summary(mod2.1)
#Residual deviance: 669.99  on 412  degrees of freedom

heteroscedasticite <- function(deviance_residuelle, dl_residuelle){
  deviance_residuelle /dl_residuelle
}

heteroscedasticite(669.99, 412)

#[1] 1.626189= > Seuil de 1, 5,  surdispersion des résidus

###ANALYSE 
Anova (mod2)





summary(base_j25)

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

heteroscedasticite <- function(deviance_residuelle, dl_residuelle){
  deviance_residuelle /dl_residuelle
}

################################################################################


