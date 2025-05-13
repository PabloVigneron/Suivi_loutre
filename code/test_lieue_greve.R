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
greve_points <-
  readxl::read_xls(path = 'raw_data/Suivi loutre Lieue de Greve octobre 24.xls', range = 'A2:H66') %>% 
  select(-`Présence de marquage`)


  
  
base_greve_spring <- base_greve %>%
  dplyr::filter(passage == 1)

sites_coord <- base_greve_spring %>%
  dplyr::select(code_site, x, y) %>%
  distinct()


sites_all_years <- expand_grid(
  code_site = unique(base_greve_spring$code_site),
  annee = unique(base_greve_spring$annee)
)

sites_status_full <- sites_all_years %>%
  left_join(base_greve_spring %>% 
              dplyr::select(code_site, annee, statut_observation),
            by = c("code_site", "annee")) %>%
  left_join(sites_coord, by = "code_site") %>%
  mutate(
    statut_final = case_when(
      statut_observation == "1" ~ "Présent",
      statut_observation == "0" ~ "Absent",
      TRUE ~ "Non prospecté"
    )
  )

sites_sf <- sites_status_full %>%
  st_as_sf(coords = c("x", "y"), crs = 2154)

basemap <- get_tiles(sites_sf, provider = "CartoDB.Positron", crop = TRUE, zoom = 12)

ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = sites_sf, aes(color = statut_final), size = 1.5, alpha = 0.9) +
  facet_wrap(~ annee) +
  scale_color_manual(
    values = c("Présent" = "darkseagreen3", "Absent" = "indianred1", "Non prospecté" = "grey40")
  ) +
  labs(
    title = "Sites de prospection, Petit Trégor - Secteur Petit Tregor (J25)",
    color = "Statut d'observation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "grey90"),
    legend.background = element_rect(fill = "grey95", color = "black")
  )


##################
tregor_geonat_correct <- base_j25 %>%
  dplyr::mutate(statut_presence = as.factor(statut_presence),
                annee = as.factor(annee)) %>%
  dplyr::select(code_site, annee, statut_presence) %>%
  #dplyr::distinct(code_site, annee, .keep_all = TRUE) %>%
  tidyr::pivot_wider(names_from = "annee",
                     values_from = "statut_presence",
                     values_fill = NA) %>%
  dplyr::relocate(any_of(
    c(
      "code_site",
      "2011",
      "2012",
      "2014",
      "2016",
      "2017",
      "2018",
      "2019",
      "2023"
    )
  )) 

## Tableau de comparaison 


################################
# Assurer que tout est en format character pour comparaison tolérante
bts_chr <- tregor_bts_correct %>%
  mutate(across(everything(), as.character))

geonat_chr <- tregor_geonat_correct %>%
  mutate(across(everything(), as.character))

# Rassembler les deux tables en format long
bts_long <- bts_chr %>%
  pivot_longer(cols = -code_site, names_to = "annee", values_to = "valeur_bts")

geonat_long <- geonat_chr 
#pivot_longer(cols = -code_site, names_to = "annee", values_to = "valeur_geonat")

# Fusionner les deux tables longues
comparison_long <- bts_long %>%
  full_join(geonat_long, by = c("code_site", "annee")) %>%
  filter(valeur_bts != valeur_geonat)

# Affichage des différences
comparison_long
#################################################