library(survival)
#install.packages("roxygen2")
library(roxygen2)

#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @examples
############################# Premiere modélisation du modèle de survie #############
############################## avec une loi exponentielle et t
set.seed(133)
simul_exp<-function(n,lambda){
  ###générer un échantillon de taille n suivant une loi exponentielle de paramètre lambda.
 return(rexp(n,lambda)) 
}
simul_survie<-function(n,lambda,t_star){
  #### Calculer la probabilite que la toxicite apparaisse de 0 à t_star. 
  ### Se base sur la simulation de temps via la fonction simul_exp. 
  ### KAPLAN-MEIER. 
  donnees<-simul_exp(n,lambda)
  donnees_censure_tstar<-ifelse(donnees<t_star,donnees,t_star)
  donnees_indicatrice_observee<-ifelse(donnees<t_star,1,0)
  donnees_ensemble<-cbind.data.frame(donnees_censure_tstar,donnees_indicatrice_observee)
  colnames(donnees_ensemble)<-c("tox_time","isobserved")
  surv_object<-Surv(donnees_ensemble$tox_time,event=donnees_ensemble$isobserved)
  fit <- survfit(surv_object ~1, data = donnees_ensemble)
  summary(fit)
  # on cherche a recuperer les donnees au temps T=6
  #afin de pouvoir tracer la droite Toxicite =f(dose)
  quantile <-quantile(fit)
  quantile$quantile
  centiles <- quantile(fit, 1:100/100)
  cent <-centiles$quantile
  m<-1
  individu <- cent[m]
  while (is.na(individu)==FALSE) {
    individu <- cent[m]
    m<- m+1
  }
  transformation <- m-1
  transformation <- transformation / 100
  return(transformation)
}

n<-100
test_surv<-simul_survie(n,0.5,6)
fonction_biais_survie<-function(n,lambda,t_star){
  #### Calcul du biais de la probabilite de toxicite estimee par la fonction simul_survie. 
  ### Comparaison avec la fonction de repartition d'une exp(lambda) en t_star. 
  estimateur<-simul_survie(n,lambda,t_star)
  valeur_theorique<-pexp(t_star,rate=lambda)
  return(abs(valeur_theorique-estimateur))
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
boxplot(test_simul_total)
