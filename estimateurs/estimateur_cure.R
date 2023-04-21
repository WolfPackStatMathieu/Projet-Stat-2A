
######################## IMPORT #####
library(survival)
library(flexsurvcure)
library(npcure)

####### Fonction ######
fonction_cure<-function(df,t_star){
  # retourne la probabilite de ne pas avoir fait de DLT a T_star
  indice_observed<-which(df$is_observed==1)
  indice_censored<-which(df$is_observed==0)
  df$covar<-rep(1,nrow(df))
  prob_gueri<-probcure(x=covar,t=tox_time,dataset = df,d=is_observed,x0=1,h=c(1,1.5,2),local=FALSE,
                       bootpars = controlpars(B = 1999))
  estimateur_tox<-1-prob_gueri[["q"]]$h1
  return(estimateur_tox)
}

estimateur_cure_mult<-function(df,t_star,nb_doses){
  data_return<-rep(NA,nb_doses)
  somme<-as.data.frame(sapply(c(1:nb_doses),function(x,df)return(c(x,sum(df[which(df$label_dose==x),"observed"]))),df=df))
  return(somme)
}
###### Test###
#df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)


# on cree un surv_object a partir du dataframe
#surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
# on estime la probabilite d avoir fait une DTL avant t_star avec la fonction flexsurvecure
result<-flexsurvcure(Surv(rectime,censrec)~1, data=bc, dist="weibullPH")
# on recupere l estimation en t_star
#appel_cure<-fonction_cure(df,t_star=6)
#mean(df$is_observed)

#surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)

