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
N<-20
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

function_influence_rate<-function(n,lim_moins,lim_plus,lambda,t_star,number_k){
  vector_k<-as.vector(seq.int(lim_moins,lim_plus,length.out = number_k))
  curve_k<-sapply(vector_k,fonction_biais_survie_weibull,lambda=lambda,t_star=t_star,n=n)
  return(curve_k)
}
number_trials<-10
l_plus<-5
l_moins<-0.1
vecteur_k_bias<-function_influence_rate(n,lim_moin=l_moins,lim_plus=l_plus,
                                        lambda=lambda_test,number_k=number_trials,t_star=t_star)
plot(vecteur_k_bias,main="Evolution of the bias",ylab="Value of the bias")
###note : the bias decreased when k increased. The reason is that the hazard function becomes 
## IFR so the probability that the event happens with time. It is thus more likely
## that we don't have NA values. 