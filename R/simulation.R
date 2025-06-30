#####################################
#' Ajoute aléatoirement des présences (=1) à la place de certaines absences (=0) dans un vecteur.
#'
#' @param vecteur Le vecteur d'entrée comprenanc des 0 et des 1.
#' @param n Numérique. Nombre de présences à rajouter. Peut être décimal.
#'
#' @return Un vecteur de même longueur qu'en entrée, mais avec plus de 1.
#' @export
#'
#' @examples
#' \dontrun{
#' vec <- c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
#' vec
#' ajouter_presences(vec, n=0)
#' ajouter_presences(vec, n=1)
#' ajouter_presences(vec, n=2)
#' }


ajouter_presences <- function(vecteur, n = 1) {
  index <- sample(1:length(vecteur[vecteur == 0]), n)
  vecteur[vecteur == 0][index] <- 1
  vecteur
}

##########################################
#' Ajoute des présences à un rythme annuel sur un tableau de présence / absence.
#' ATTENTION Cette fonction requiert des les noms de variables soient ceux du jeu de données
#' loutres prétraité.
#'
#' @param df Dataframe contenant les données.
#' @param n_pres_suppl_par_an Numérique. Nombre de présences à rajouter par année. Peut être décimal.
#'
#' @return Le dataframe d'origine auquel est ajouté une variable 'statut_presence_sim', ainsi qu'une 
#'   variable 'presence_ajoutee'.
#' @export
#'
#' @examples
ajouter_presences_annuelles <-
  function(df, n_pres_suppl_par_an = 0) {
    df2 <- df %>%
      group_by(annee_perm) %>%
      mutate(statut_presence_sim = ajouter_presences(statut_presence,
                                                     n = min(annee_perm_index) * n_pres_suppl_par_an)) %>%
      ungroup()
    
    statut_presence_rand <- df %>% 
      pull(statut_presence) 
    
    df2 <- df2 %>% 
      cbind(statut_presence_rand) %>% 
      mutate(presence_ajoutee = (statut_presence_rand != statut_presence_sim))
    
    df2
    
  }


############################################
#' Title
#'
#' @param df Dataframe contenant les données.
#' @param n_pres_suppl_par_an Numérique. Nombre de présences à rajouter par année. Peut être décimal.
#' @param n_permutations Entier. Nombre de permutations à effectuer (fixé à 10 par défaut)
#'
#' @return Un dataframe résumant l'effet 'année' des modèles construits à chaque permutation.
#' @export
#'
#' @examples
tester_tendance <- function(df, n_pres_suppl_par_an = 0, n_permutations = 10) {
  
  # test pour un modèle après permutation et injection de la tendance
  test_1_modele <- function(df, n_pres_suppl_par_an, index = 1) {
    
    df_permute <- df %>% 
      group_by(code_site) %>% 
      mutate(annee_perm = gtools::permute(annee)) %>% 
      ungroup() %>% 
      mutate(annee_perm_index = annee_perm - min(annee_perm))
    
    # AJOUT DU SIGNAL CONNU
    df_trend <- df_permute %>% 
      ajouter_presences_annuelles(n_pres_suppl_par_an = n_pres_suppl_par_an)
    
    # Construction du modèle
    mod <- glmer(statut_presence_sim ~
                   annee_perm +
                   (1|code_site),
                 family=binomial(link = "logit"),
                 data = df_trend)
    
    # assemblage des résultats
    coef_annee <- coef(summary(mod))[2,1]
    pvalue_annee <- coef(summary(mod))[2,4]
    resultat <- data.frame(index, n_pres_suppl_par_an, coef_annee, pvalue_annee)
    
    resultat
  }
  
  # boucle sur les permutations dont les résultats sont assemblés dans un dataframe
  map_df(.x = 1:n_permutations,
         .f = test_1_modele,
         df = df,
         n_pres_suppl_par_an = n_pres_suppl_par_an) %>% 
    mutate(tendance = case_when(
      pvalue_annee > 0.05 ~ "NS",
      pvalue_annee < 0.05 & sign(coef_annee) == 1 ~ "Augmentation",
      pvalue_annee < 0.05 & sign(coef_annee) == -1 ~ "Diminution",
      TRUE ~ NA
      
    ))
  
}


###############################
get_pc_sig <- function(df, n_pres_suppl_par_an = 0, n_permutations = 10) {
  
  test <- tester_tendance(df = df,
                          n_pres_suppl_par_an = n_pres_suppl_par_an,
                          n_permutations = n_permutations)

  pc_detec_aug <- nrow(test[test$tendance == "Augmentation",]) / nrow(test)
  
}


#############################
my_histo <- function(test_df) {
  test_df %>%
    ggplot(aes(x = coef_annee,
               fill = tendance)) +
    geom_histogram(alpha = 0.5, bins = 15) +
    geom_vline(xintercept = 0,
               col = "blue",
               lty = "dotted") +
    scale_fill_manual(values = c("green", "red", "grey50")) +
    labs(x = "Pente année",
         y = "Fréquence",
         fill = "Tendance")
  
}

##############################
my_point_plot <- function(df) {
  ggplot(df %>% 
           filter(presence_ajoutee),
         aes(x = annee_perm,
             y = code_site,
             col = as.factor(statut_presence_sim))) +
    geom_point(size = 3) +
    geom_point(data = df,
               aes(col = as.factor(statut_presence_sim))) +
    scale_color_manual(values =  c("purple", "darkgreen")) +
    labs(x = "Année (après permutation)",
         y = "Site",
         col = "Présence",
         size = "Présence ajoutée")
  
}


##########################################
#' Ajoute des présences à un rythme annuel sur un tableau de présence / absence.
#' ATTENTION Cette fonction requiert des les noms de variables soient ceux du jeu de données
#' loutres prétraité.
#'
#' @param df Dataframe contenant les données.
#' @param n_pres_suppl_par_season Numérique. Nombre de présences à rajouter par année. Peut être décimal.
#'
#' @return Le dataframe d'origine auquel est ajouté une variable 'statut_presence_sim', ainsi qu'une 
#'   variable 'presence_ajoutee'.
#' @export
#'
#' @examples
ajouter_presences_season <-
  function(df, n_pres_suppl_par_season = 0) {
    df2 <- df %>%
      group_by(time_perm) %>%
      mutate(statut_presence_sim = ajouter_presences(statut_presence,
                                                     n = min(time_perm_index) * n_pres_suppl_par_season)) %>%
      ungroup()
    
    statut_presence_rand <- df %>% 
      pull(statut_presence) 
    
    df2 <- df2 %>% 
      cbind(statut_presence_rand) %>% 
      mutate(presence_ajoutee = (statut_presence_rand != statut_presence_sim))
    
    df2
    
  }

###########################################
#' Title
#'
#' @param df Dataframe contenant les données.
#' @param n_pres_suppl_par_season Numérique. Nombre de présences à rajouter par année. Peut être décimal.
#' @param n_permutations Entier. Nombre de permutations à effectuer (fixé à 10 par défaut)
#'
#' @return Un dataframe résumant l'effet 'année' des modèles construits à chaque permutation.
#' @export
#'
#' @examples
tester_tendance_season <- function(df, n_pres_suppl_par_season = 0, n_permutations = 10) {
  
  # test pour un modèle après permutation et injection de la tendance
  test_1_modele <- function(df, n_pres_suppl_par_season, index = 1) {
    
    df <- greve_year_numeric_season %>%
      mutate(time_index = (annee - min(annee)) * 2 + as.numeric(factor(passage)) - 1)
    
    df_permute <- df %>% 
      group_by(code_site) %>% 
      mutate(time_perm = gtools::permute(time_index)) %>% 
      ungroup() %>% 
      mutate(time_perm_index = time_perm - min(time_perm))
    
    # AJOUT DU SIGNAL CONNU
    df_trend <- df_permute %>% 
      ajouter_presences_season(n_pres_suppl_par_season = n_pres_suppl_par_season)
    
    # Construction du modèle
    mod <- glmer(statut_presence_sim ~
                   time_perm +
                   (1|code_site),
                 family=binomial(link = "logit"),
                 data = df_trend)
    
    # assemblage des résultats
    coef_time <- coef(summary(mod))[2,1]
    pvalue_time <- coef(summary(mod))[2,4]
    resultat <- data.frame(index, n_pres_suppl_par_season, coef_time, pvalue_time)
    
    resultat
  }
  
  # boucle sur les permutations dont les résultats sont assemblés dans un dataframe
  map_df(.x = 1:n_permutations,
         .f = test_1_modele,
         df = df,
         n_pres_suppl_par_season = n_pres_suppl_par_season) %>% 
    mutate(tendance = case_when(
      pvalue_time > 0.05 ~ "NS",
      pvalue_time < 0.05 & sign(coef_time) == 1 ~ "Augmentation",
      pvalue_time < 0.05 & sign(coef_time) == -1 ~ "Diminution",
      TRUE ~ NA
      
    ))
  
}

##############################
my_point_plot_season <- function(df) {
  ggplot(df %>% 
           filter(presence_ajoutee),
         aes(x = time_perm,
             y = code_site,
             col = as.factor(statut_presence_sim))) +
    geom_point(size = 3) +
    geom_point(data = df,
               aes(col = as.factor(statut_presence_sim))) +
    scale_color_manual(values =  c("purple", "darkgreen")) +
    labs(x = "Temps (après permutation)",
         y = "Site",
         col = "Présence",
         size = "Présence ajoutée")
  
}
###############################
get_pc_sig_season <- function(df, n_pres_suppl_par_season = 0, n_permutations = 10) {
  
  test <- tester_tendance_season(df = df,
                          n_pres_suppl_par_season = n_pres_suppl_par_season,
                          n_permutations = n_permutations)
  
  pc_detec_aug <- nrow(test[test$tendance == "Augmentation",]) / nrow(test)
  
}


#############################
my_histo_season <- function(test_df, titre) {
  test_df %>%
    ggplot(aes(x = coef_time,
               fill = tendance)) +
    geom_histogram(alpha = 0.5, bins = 15) +
    geom_vline(xintercept = 0,
               col = "blue",
               lty = "dotted") +
    scale_fill_manual(values = c("green", "red", "grey50")) +
    labs(x = "Pente saison",
         y = "Fréquence",
         fill = "Tendance",
         title = {{titre}}
         )
  
}
