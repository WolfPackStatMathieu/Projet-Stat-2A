######################## Le modèle prend désormais en compte les indicatrices d'évènement tt avec deux tt=1 ou tt=2.######
##### imports #####
source("generation_echantillon/fonctions_simulations_competition.R")
library(cmprsk)
####################Pour generer les temps, il faut d'abord associer à chaque patient un temps mais aussi une cause d'arrêt. #####

#######1) il faut générer les paramètres des deux lois Weibull associées. #####

#######2) générer des temps pour chaque individu. Comme on veut distinguer entre les différentes causes, ###
######on crée une variable dd qui prendre comme valeur 1 ou 2 selon la cause de la faillite. ######

######3) on calcule le profil de chaque patient. Chaque patient a deux profils. 
##### Le premier correspond à la probabilité qu'il connaisse la faillite avant la fin de la fenetre d'observations.#####
##### Le second correspond à l'origine de la faillite. #####

#####Dans les deux cas, ces valeurs sont associées à des réalisations de la loi uniforme sur 0,1. #####
#####On associe notamment la valeur 1 pour (u2) à une certaine cause. #####
######4) Pour déterminer la cause de la faillite, on calcule la rapport des risque des risques instantanés. #####
######L'idée est que l'on utilisera les réalisations u1 et u2 comme des résultats de la fonction de répartition ####
###### pour d'un côté obtenir un quantile d'ordre u1 pour la loi weibull et de l'autre pour obtenir####
#### un quantile d'ordre u2 pour la loi Bernoulli (0 et 1 selon la cause de la faillite).####

#### type1 correspondra à "decreasing", "constant" et "increasing". 
#### même chose pour le type2. 
generation_comp<-function(p_cause1,p_cause2,t_star,nombre_obs,type1,type2){
  alpha1<-get_alpha(p_cause1,obswin=t_star,typ="weibull",typ_wb=type1)
  alpha2<-get_alpha(p_cause2,obswin=t_star,typ="weibull",typ_wb=type2)
  liste_dataset<-get_dataset0(n=nombre_obs,alpha1,alpha2,tstar=t_star,graine=133,K=1,type="weibull")
  data<-liste_dataset$data_complete
  data<-as.data.frame(data)
  data$is_observed<-ifelse(data$status==0,0,1)
  data_estim<-data[,c("status","time","is_observed")]
  colnames(data_estim)<-c("status","tox_time","is_observed")
  return(data_estim)
}
##### il faut mettre deux p pour chaque dose.#####
p_cause1<-0.33
p_cause2<-0.45
t_star<-6
nombre_obs<-10
type1<-"decreasing"
type2<-"decreasing"
test<-generation_comp(p_cause1,p_cause2,t_star,nombre_obs,type1,type2)
