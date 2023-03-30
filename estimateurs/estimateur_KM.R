source("generation_echantillon/generation_echantillon.R")
source("utils.R")
fonction_KM<-function(df,t_star){
  df<-df[,c("tox_time","is_observed")]
  indice_cens<-which(df$is_observed==0)
  if(length(indice_cens)==0){return(1)}
  surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  fit <- survfit(surv_object ~1, data = df)
  estimateur_survie<-1-tp.surv(fit,6)[3][[1]]
  return(estimateur_survie)
}

##### TEST : + verification pour remplacer par tp.surv ####
# df<-Generation_un_ech(n=1000,lambda=0.5,p=0.33,k=1,t_star=6)
# estim_KM<-fonction_KM(df)
# 
# obj_surv<-Surv(as.numeric(df$tox_time),event=df$is_observed)
# obj <- survfit(Surv(as.numeric(df$tox_time),event=df$is_observed) ~1, data = df)
# estim_KM_new <- tp.surv(obj, 6) [3]
# estim_KM - (1-estim_KM_new[[1]]) # pas mal!, on n'avait pas trop mal travaill?
