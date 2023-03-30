source("generation_echantillon/generation_echantillon.R")
source("utils.R")
fonction_KM<-function(df,t_star){
  df<-df[,c("tox_time","is_observed")]
  indice_cens<-which(df$is_observed==0)
  if(length(indice_cens)==0){return(1)}
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
  return(estimateur_survie)
}

##### TEST : + verification pour remplacer par tp.surv ####
df<-Generation_un_ech(n=1000,lambda=0.5,p=0.33,k=1,t_star=6)
estim_KM<-fonction_KM(df)

obj_surv<-Surv(as.numeric(df$tox_time),event=df$is_observed)
obj <- survfit(Surv(as.numeric(df$tox_time),event=df$is_observed) ~1, data = df)
estim_KM_new <- tp.surv(obj, 6) [3]
estim_KM - (1-estim_KM_new[[1]]) # pas mal!, on n'avait pas trop mal travaillé
