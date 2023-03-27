
######################## IMPORT #####
library(survival)
library(flexsurvcure)


####### Fonction ######
fonction_cure<-function(df,t_star){
  # retourne la probabilite de ne pas avoir fait de DLT a T_star
  
  # on cree un surv_object a partir du dataframe
  surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  # on estime la probabilite d avoir fait une DTL avant t_star avec la fonction flexsurvecure
  result<-flexsurvcure(surv_object ~1, data = df, link="logistic", dist="weibullPH", mixture=T) 
  # on recupere l estimation en t_star
  Predicted_survival_prob<- summary(result, t=t_star, type="survival", tidy=T)$est
  # on retourne le complementaire pour obtenir ce qu on veut
  return(1-Predicted_survival_prob)
}

###### Test###
#df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)


# on cree un surv_object a partir du dataframe
#surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
# on estime la probabilite d avoir fait une DTL avant t_star avec la fonction flexsurvecure
#result<-flexsurvcure(surv_object ~1, data = df, link="logistic", dist="weibullPH", mixture=T) 
# on recupere l estimation en t_star
#appel_cure<-fonction_cure(df,t_star=6)
#mean(df$is_observed)

#surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)

