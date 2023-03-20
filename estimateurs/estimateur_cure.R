source("estimateurs/mod_bernoulli.R")
######################## IMPORT #####
library(survival)
library(flexsurvcure)


####### Fonction ######
fonction_cure<-function(df,t_star){
  surv_object<-Surv(df$tox_time,event=df$is_observed)
  ## group correspond au groupe des personnes à risque ou non. 
  df$sensible<-as.numeric(df$sensible)
  result<-flexsurvcure(surv_object ~1, data = df, link="logistic", dist="weibullPH", mixture=T)
  shape_model<-result[["coefficients"]][["shape"]]
  probabilite_etre_sensible<-mean(df$sensible)
  probabilite_survenue_sensible<-(1-result$dfns$p(t_star,theta=probabilite_etre_sensible,shape=shape_model))
  #1 modele de melange#
  # pour les sensibles#
  probabilitegroup1<-probabilite_etre_sensible*probabilite_survenue_sensible
  probabilitegroup2<-1-probabilite_etre_sensible
  probabilite_non_DLT<-probabilitegroup1+probabilitegroup2
  probabilite_DLT<-(1-probabilite_non_DLT)
  return(probabilite_DLT)
}

###### Test###
df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)
str(df)
appel_cure<-fonction_cure(df,t_star=6)
