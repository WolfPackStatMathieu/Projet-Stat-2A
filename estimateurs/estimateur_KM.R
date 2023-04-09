source("generation_echantillon/generation_echantillon.R")
fonction_KM<-function(df,t_star){
  df<-df[,c("tox_time","is_observed")]
  surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
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
  return(1-estimateur_survie)
}

##### TEST ####
df<-Generation_un_ech(n=10,lambda=0.5,p=0.33,k=1,t_star=6)
str(df)
estim_KM<-fonction_KM(df)
estim_KM
