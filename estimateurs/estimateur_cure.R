source("estimateurs/mod_bernoulli.R")
######################## IMPORT #####
library(survival)
library(flexsurvcure)


####### Fonction ######
fonction_cure<-function(df,t_star){
  surv_object<-Surv(df$tox_time,event=df$is_observed)
  ## group correspond au groupe des personnes à risque ou non. 
  df$sensible<-as.numeric(df$sensible)
  probabilite_etre_sensible<-mean(df$sensible)
  df<-df[,c("tox_time","is_observed")]
  result<-flexsurvcure(surv_object ~1, data = df, link="logistic", dist="weibullPH", mixture=T)
  shape_model<-result[["coefficients"]][["shape"]]
  scale<-result[["coefficients"]][["scale"]]
  ### la valeur de scale peut être négative. Le scale correspond aux covariables. 
  ## On retrouve le lambda en calculant exp(z(t)*beta)
  probabilite_survenue_sensible<-exp(-exp(scale)*(t_star)^(shape))
  #1 modele de melange#
  # pour les sensibles#
  probabilitegroup1<-probabilite_etre_sensible*probabilite_survenue_sensible
  probabilitegroup2<-1-probabilite_etre_sensible
  probabilite_DLT<-1-(probabilitegroup1+probabilitegroup2)
  return(probabilite_DLT)
}

###### Test###
df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)
appel_cure<-fonction_cure(df,t_star=6)
print(appel_cure)
mean(df$is_observed)
flexsurvcure(Surv(rectime,censrec)~group, data=bc, dist="weibull", anc=list(scale=~group))
print(bc)
