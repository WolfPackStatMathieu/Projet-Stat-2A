library(survival)
#install.packages("roxygen2")
library(roxygen2)
source("bernoulli.R")
source("weibull.R")
#' @examples
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
############################# Premiere mod?lisation du mod?le de survie #############
############################## avec une loi exponentielle et t
simul_exp<-function(n,lambda){
  ###g?n?rer un ?chantillon de taille n suivant une loi exponentielle de param?tre lambda.
 return(rexp(n,lambda))
}





Simuler_biais_un_n_ech<-function(n,lambda,t_star,p,k){
  vecteur_censure<-simul_bernoulli(n,p)
  vecteur_temp<-rep(NA,n)
  estimateur_cure<-mean(vecteur_censure)
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
  liste_biais<-list(estimateur_cure,estimateur_survie)
  names(liste_biais)<-c("Modele_guerison","Modele_survie")
  return(liste_biais)
}
Calcul_estim_depuis_df<-function(df,nom_col_obs,nom_coltemps){
  surv_object<-Surv(df[,nom_coltemps],event=df[,nom_col_obs])
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
  return(estimateur_survie)
}

Simuler_biais_taillen<-function(K,n,lambda,t_star,p,k){
  df_biases<-as.data.frame(t(cbind.data.frame(sapply(rep(n,K),Simuler_biais_un_n_ech,lambda=lambda,t_star=t_star,p=p,k=k))))
  df_biases$Modele_guerison<-as.numeric(df_biases$Modele_guerison)
  df_biases$Modele_survie<-as.numeric(df_biases$Modele_survie)
  return(df_biases)
}
test_retour<-Simuler_biais_un_n_ech(n=10,lambda=0.5,t_star=6,0.33,2)
test_several_times<-Simuler_biais_taillen(n=10,lambda=0.5,t_star=6,p=0.33,k=2,K=10)
N<-100
Calcul_biais_moyen_taillen<-function(K,n,lambda,t_star,p,k){
  data<-Simuler_biais_taillen(K,n,lambda,t_star,p,k)
  result<-rep(NA,2)
  result<-colMeans(data)
  result[1]<-result[1]-p
  result[2]<-result[2]-p
  return(result)
}
test_biais_moy<-Calcul_biais_moyen_taillen(n=10,lambda=0.5,t_star=6,p=0.33,k=2,K=10)
