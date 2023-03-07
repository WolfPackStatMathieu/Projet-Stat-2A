library(survival)
#install.packages("roxygen2")
library(roxygen2)
source("bernoulli.R")
source("weibull.R")
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @examples
############################# Premiere mod?lisation du mod?le de survie #############
############################## avec une loi exponentielle et t
set.seed(133)
simul_exp<-function(n,lambda){
  ###g?n?rer un ?chantillon de taille n suivant une loi exponentielle de param?tre lambda.
 return(rexp(n,lambda))
}
simul_survie<-function(n,lambda,t_star){
  #### Calculer la probabilite que la toxicite apparaisse de 0 ? t_star.
  ### Se base sur la simulation de temps via la fonction simul_exp.
  ### KAPLAN-MEIER.
  donnees<-simul_exp(n,lambda)
  donnees_censure_tstar<-ifelse(donnees<t_star,donnees,t_star)
  donnees_indicatrice_observee<-ifelse(donnees<t_star,1,0)
  donnees_ensemble<-cbind.data.frame(donnees_censure_tstar,donnees_indicatrice_observee)
  colnames(donnees_ensemble)<-c("tox_time","isobserved")
  surv_object<-Surv(donnees_ensemble$tox_time,event=donnees_ensemble$isobserved)
  fit <- survfit(surv_object ~1, data = donnees_ensemble)
  # on cherche a recuperer les donnees au temps T=6
  #afin de pouvoir tracer la droite Toxicite =f(dose)
  quantile <-quantile(fit)
  quantile$quantile
  centiles <- quantile(fit, 1:100/100)
  cent <-centiles$quantile
  m<-1
  individu <- cent[m]
  # on touche la proportion de tstar au premier NA
  while (is.na(individu)==FALSE) {
    individu <- cent[m]
    m<- m+1
  }
  transformation <- m-1
  transformation <- transformation / 100
  return(transformation)
}

n<-100
test_surv<-simul_survie(n,0.2,6)
fonction_biais_survie<-function(n,lambda,t_star){
  #### Calcul du biais de la probabilite de toxicite estimee par la fonction simul_survie.
  ### Comparaison avec la fonction de repartition d'une exp(lambda) en t_star.
  estimateur<-simul_survie(n,lambda,t_star)
  valeur_theorique<-pexp(t_star,rate=lambda)
  return(estimateur-valeur_theorique)
}
lambda_test<-1/3
n<-100
t_star<-6
#mediane<-qexp(0.9,rate=lambda_test)
test_biais_surv<-fonction_biais_survie(n,lambda_test,t_star)

Simuler_Nfois_n_echantillons<-function(N,n,lambda,t_star){
  vecteur_biais<-rep(NA,N)
  vecteur_taille<-rep(n,N)
  vecteur_biais<-sapply(vecteur_taille,fonction_biais_survie,lambda=lambda,t_star=t_star)
  return(vecteur_biais)
}
N<-100
test_simul_total<-Simuler_Nfois_n_echantillons(N,n,lambda_test,t_star)
boxplot(test_simul_total,main="Distribution du biais pour le mod?le de survie",col="Sky blue")
Simuler_biais_un_n_ech<-function(n,lambda,t_star,p,k){
  vecteur_censure<-simul_bernoulli(n,p)
  vecteur_temp<-rep(NA,n)
  biais_cure<-mean(vecteur_censure)-p
  df<-cbind.data.frame(vecteur_censure,vecteur_temp)
  colnames(df)<-c("censure","temps")
  id_censures<-which(df$censure==1)
  id_obs<-which(df$censure==0)
  df[id_censures,2]<-t_star
  df[id_obs,2]<-simul_weibull(length(id_obs),lambda,k)
  df$temps<-ifelse(df$temps<t_star,df$temps,t_star)
  colnames(df)<-c("isobserved","tox_time")
  df$isobserved<-ifelse(df$tox_time<t_star,1,0)
  surv_object<-Surv(df$tox_time,event=df$isobserved)
  fit <- survfit(surv_object ~1, data = df)
  # on cherche a recuperer les donnees au temps T=6
  #afin de pouvoir tracer la droite Toxicite =f(dose)
  quantile <-quantile(fit)
  quantile$quantile
  centiles <- quantile(fit, 1:100/100)
  cent <-centiles$quantile
  m<-1
  individu <- cent[m]
  # on touche la proportion de tstar au premier NA
  while (is.na(individu)==FALSE) {
    individu <- cent[m]
    m<- m+1
  }
  transformation <- m-1
  estimateur_survie <- transformation / 100
  biais_survie<-estimateur_survie-p
  liste_biais<-list(biais_cure,biais_survie)
  names(liste_biais)<-c("Biais guérison","Biais survie")
  return(liste_biais)
}
Simuler_biais_taillen<-function(K,n,lambda,t_star,p,k){
  resultat<-list(rep(NA,K))
  for (j in c(1:K)){
    resultat[[j]]<-unlist(Simuler_biais_un_n_ech(n,lambda,t_star,p,k))
  }
  df_biases<-cbind.data.frame(resultat)
  colnames(df_biases)<-c("Biais guerison","Bias")
  return(resultat)
}
test_retour<-Simuler_biais_un_n_ech(n=10,lambda=3,t_star=6,0.33,2)
test_several_times<-Simuler_biais_taillen(n,lambda=3,t_star=6,p=0.33,k=2,K=5)
N<-100

