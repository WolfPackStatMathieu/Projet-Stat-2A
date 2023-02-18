simul_weibull<-function(n,lambda,k){
  return(rweibull(n,shape=k,scale=lambda))
}
simul_survie_weibull<-function(n,lambda,k,t_star){
  donnees<-simul_weibull(n,lambda,k)
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
  # on touche la proportion de tstar au premier NA
  while (is.na(individu)==FALSE) {
    individu <- cent[m]
    m<- m+1
  }
  transformation <- m-1
  transformation <- transformation / 100
  return(transformation)
}
fonction_biais_survie_weibull<-function(n,lambda,k,t_star){
  #### Calcul du biais de la probabilite de toxicite estimee par la fonction simul_survie.
  ### Comparaison avec la fonction de repartition d'une exp(lambda) en t_star.
  estimateur<-simul_survie_weibull(n,lambda,k,t_star)
  valeur_theorique<-pweibull(t_star,scale=lambda,shape=k)
  return(abs(valeur_theorique-estimateur))
}
lambda_test<-1/3
k<-3
n<-100
t_star<-6
#mediane<-qexp(0.9,rate=lambda_test)
test_biais_weibull<-fonction_biais_survie_weibull(n,lambda=lambda_test,k,t_star)
Simuler_Nfois_n_weibull<-function(N,n,lambda,k,t_star){
  vecteur_biais<-rep(NA,N)
  vecteur_taille<-rep(n,N)
  vecteur_biais<-sapply(vecteur_taille,fonction_biais_survie_weibull,lambda=lambda,k=k,t_star=t_star)
  return(vecteur_biais)
}
vecteur<-Simuler_Nfois_n_weibull(N,n,lambda=lambda_test,k,t_star)
