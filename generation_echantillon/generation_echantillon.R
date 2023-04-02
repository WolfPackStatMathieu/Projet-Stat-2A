library(survival)
require(flexsurvcure)
source("weibull.R")
Generation_un_ech<-function(n,lambda,t_star,p,k){
  vecteur_censure<-rbinom(n,1,p)
  vecteur_temp<-rep(NA,n) # cree un vecteur des temps associ?s
  # l'estimateur du modele de Bernoulli est la moyenne des 1 du vecteur_censure
  # on cree un dataframe qui acolle la DLT au vecteur_temps
  df<-cbind.data.frame(vecteur_censure,vecteur_temp)
  #on les renomme
  colnames(df)<-c("sensible","temps")
  #recuperation des numeros de lignes des individus censures
  id_non_sensibles<-which(df$sensible==0)
  #recuperation des numeros de lignes des individus a risque de DLT
  id_sensibles<-which(df$sensible==1)
  #tous les individu censure se voient attribues comme temps la limite de la 
  #fenetre d observation
  df[id_non_sensibles,2]<-t_star+1
  # les autres individus se voient attribuer un temps simule a partir d une 
  # loi de Weibull (qui peut etre une loi exponentielle si k=1)
  df[id_sensibles,2]<-simul_weibull(length(id_sensibles),lambda,k)
  # bien sur, si le temps observe est superieur a la fenetre d observation, alors
  # on le remplace par la fin de fenetre d observation
  # on renomme les colonnes pour une meilleure interpretation
  # on remplit la colonne isobserved avec des 1 si on observe une toxicite avant
  #la fin de la fenetre d observation, sinon on met des 0
  colnames(df)<-c("sensible","tox_time")
  df$is_observed<-ifelse(df$tox_time<t_star,1,0)
  return(df)
}



############# TEST #####
# generat<-Generation_un_ech(n=100,lambda=0.5,t_star=6,p=0.33,k=1)

