source("generation_echantillon/generation_echantillon.R")
fonction_KM<-function(df){
  surv_object<-Surv(df$tox_time,event=df$is_observed)
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
  return(c(estimateur_survie,calcul_fonction_surv))
}

##### TEST ####
df<-Generation_un_ech(n=50,lambda=0.5,p=0.5,k=1,t_star=6)
estim_KM<-fonction_KM(df)
