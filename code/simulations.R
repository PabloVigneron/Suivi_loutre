I(X1^2)
mod3.3 <- glmer(statut_presence~
                  scale(annee)+
              #    scale(I(annee^2))+
                  (1|code_site),
                family=binomial(link = "cauchit"), data = base_j25_year_numeric)



df <- cbind(base_j25_year_numeric,
            predict(mod3.3))

y=predict(mod3.3,se=TRUE)

plot(predict(mod3.3),residuals(mod3.3),col=c("blue","red")[1+as.numeric(base_j25_year_numeric$statut_presence)])

abline(h=0,lty=2,col="grey")

lines(lowess(predict(mod3.3),residuals(mod3.3)),col="black",lwd=2)
segments(predict(mod3.3),y$fit+2*y$se.fit,predict(mod3.3),y$fit-2*y$se.fit,col="green")


plot(scale(base_j25_year_numeric$annee),residuals(mod3.3),col=c("blue","red"))
lines(lowess(X2,residuals(reg)),col="black",lwd=2)
> lines(lowess(X2[Y==0],residuals(reg)[Y==0]),col="blue")
> lines(lowess(X2[Y==1],residuals(reg)[Y==1]),col="red")
> abline(h=0,lty=2,col="grey")
####################
mod3.0 <- glmer(statut_presence~annee+(1|code_site), family=binomial(link = "logit"), data = base_j25_year_numeric)

df <- base_j25_year_numeric %>% 
  mutate(code_site = as.factor(code_site),
         statut_presence = as.integer(statut_presence) - 1,
         date_annee = lubridate::yday(date_visite))

mod3.0c <- mgcv::gam(statut_presence ~ annee +
                       s(date_annee, bs = "ad") +
                    #  date_annee + 
                       s(code_site, bs = 're'), 
                    data = df,
                    method = 'REML')

summary(mod3.0c)
plot(mod3.0c)

df2 <- df %>% 
  cbind(predict(mod3.0c))

sites_tjs_abs <- df2 %>% 
  group_by(code_site) %>% 
  summarise(nb_obs = n(),
            nb_pres = sum(statut_presence)) %>% 
  ungroup() %>% 
  mutate(taux_pres = nb_pres / nb_obs) %>% 
  filter(taux_pres < 0.1) %>% 
  pull(code_site)
  
df3 <- df %>% 
  filter(!(code_site %in% sites_tjs_abs))

mod3.0c <- mgcv::gam(statut_presence ~ annee +
                       # s(date_annee, bs = "ad") +
                         date_annee + 
                       s(code_site, bs = 're'), 
                     data = df3,
                     method = 'REML')

summary(mod3.0c)
plot(mod3.0c)

#########################################################

df_rand <- df %>% 
  group_by(code_site) %>% 
  mutate(annee_perm = gtools::permute(annee))


mod4.0 <- glmer(statut_presence~annee_perm+(1|code_site), family=binomial(link = "logit"), data = df_rand)
coef_annee <- coef(summary(mod4.0))[2,1]
pvalue_annee <- coef(summary(mod4.0))[2,4]

#################################
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
ajouter_presence <- function(df, n_pres_par_an = 1) {
  
  df2 <- df %>% 
    filter(statut_presence == 0) %>% 
    group_by(annee) %>% 
      mutate(statut_presence2 = )
    
    
    
  absences_index <- df$statut_presence != 1
  absences_index[FALSE]
  df2 <- df %>% 
    
    
  
}
