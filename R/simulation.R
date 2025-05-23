#################################
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_model_coef <- function(df) {
  mod <- glmer(statut_presence ~ annee_perm +
                 (1|code_site),
               family=binomial(link = "logit"), data = df)
  
  coef_annee <- coef(summary(mod))[2,1]
  pvalue_annee <- coef(summary(mod))[2,4]
  
  resultat <- data.frame(coef_annee, pvalue_annee)
  
  
}


mod2 <- get_model_coef(df = df_rand)


#####################################
ajouter_presences <- function(vecteur, n = 1) {
  index <- sample(1:length(vecteur[vecteur == 0]), n)
  vecteur[vecteur == 0][index] <- 1
  vecteur
}

vec <- c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

vec
ajouter_presences(vec, n=0)
ajouter_presences(vec, n=1)
ajouter_presences(vec, n=2)

df2 <- df %>% 
  ungroup() %>% 
  mutate(statut_presence2 = ajouter_presences(statut_presence, n = 3))

table(df$statut_presence)
table(df2$statut_presence2)


##########################################
ajouter_presences_annuelles <-
  function(df, n_pres_suppl_par_an = 1) {
    df2 <- df %>%
      group_by(annee_perm) %>%
      mutate(statut_presence_sim = ajouter_presences(statut_presence,
                                                     n = min(annee_index) * n_pres_suppl_par_an)) %>%
      ungroup()
    
    df2
    
  }
