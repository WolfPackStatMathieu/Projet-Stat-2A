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

