# nettoyage de l'espace de travail et chargement des packages
rm(list=ls())
library(tidyverse)

# chargement des données et des fonctions
load(file = "processed_data/base_j25_year_numeric.rda")
source(file = "R/simulation.R")

# préparation du df
df <- base_j25_year_numeric %>% 
  mutate(code_site = as.factor(code_site),
         statut_presence = as.integer(statut_presence) - 1,
         date_annee = lubridate::yday(date_visite))

#####################################
# EFFACEMENT DU SIGNAL INTERANNUEL
# on permute les années pour effacer tout signal de tendance interannuelle
# l'année permutée est la variable annee_perm
df_rand <- df %>% 
  group_by(code_site) %>% 
  mutate(annee_perm = gtools::permute(annee)) %>% 
  ungroup() %>% 
  mutate(annee_index = annee_perm - min(annee_perm))

# vérification. On ne doit pas avoir de pente année significative (sauf une fois sur 20)
mod <- glmer(statut_presence~annee_perm+(1|code_site), family=binomial(link = "logit"), data = df_rand)
summary(mod)

#################################
# AJOUT DU SIGNAL CONNU
df_trend1 <- df_rand %>% 
  ajouter_presences_annuelles(n_pres_suppl_par_an = 0.1)

# vérifications
table(df_trend1$annee)
table(df_trend1$annee_perm[df_trend1$statut_presence == 1])
table(df_trend1$annee_perm[df_trend1$statut_presence_sim == 1])

#################################
# TEST : le modèle permet-il de détercter la tendance ?
mod_tend1 <- glmer(statut_presence_sim ~
                     annee_perm +
                     (1|code_site),
                   family=binomial(link = "logit"),
                   data = df_trend1)
summary(mod_tend1)

# on essaye avec un peu plus de tendance
df_trend1 <- df_rand %>% 
  ajouter_presences_annuelles(n_pres_suppl_par_an = 0.2)

mod_trend1 <- glmer(statut_presence_sim ~
                     annee_perm +
                     (1|code_site),
                   family=binomial(link = "logit"),
                   data = df_trend1)
summary(mod_trend1)

table(df_trend1$annee_perm[df_trend1$statut_presence == 1])
table(df_trend1$annee_perm[df_trend1$statut_presence_sim == 1])

# on essaye avec un peu plus de tendance
df_trend1 <- df_rand %>% 
  ajouter_presences_annuelles(n_pres_suppl_par_an = 0.4)

mod_trend1 <- glmer(statut_presence_sim ~
                      annee_perm +
                      (1|code_site),
                    family=binomial(link = "logit"),
                    data = df_trend1)
summary(mod_trend1)
test <- get_model_coef(modele = mod_trend1)

table(df_trend1$annee_perm[df_trend1$statut_presence == 1])
table(df_trend1$annee_perm[df_trend1$statut_presence_sim == 1])

################################################
test2 <- tester_tendance(df = df,
                         n_pres_suppl_par_an = 0.2,
                         n_permutations = 100)

test2 %>% 
  ggplot(aes(x = coef_annee,
             fill = (pvalue_annee < 0.05))) +
  geom_histogram(alpha = 0.5, bins = 15) +
  facet_wrap(~(pvalue_annee < 0.05)) +
  geom_vline(xintercept = 0, col = "red")

             