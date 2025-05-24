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
#' @return Le dataframe d'origine auquel est ajouté une variable 'statut_presence_sim'
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
         n_pres_suppl_par_an = n_pres_suppl_par_an)
  
}

