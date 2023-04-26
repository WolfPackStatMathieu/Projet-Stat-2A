######################## Le mod?le prend d?sormais en compte les indicatrices d'?v?nement tt avec deux tt=1 ou tt=2.######
##### imports #####
source("generation_echantillon/fonctions_simulations_competition.R")
library(cmprsk)
####################Pour generer les temps, il faut d'abord associer ? chaque patient un temps mais aussi une cause d'arr?t. #####

#######1) il faut g?n?rer les param?tres des deux lois Weibull associ?es. #####

#######2) g?n?rer des temps pour chaque individu. Comme on veut distinguer entre les diff?rentes causes, ###
######on cr?e une variable dd qui prendre comme valeur 1 ou 2 selon la cause de la faillite. ######

######3) on calcule le profil de chaque patient. Chaque patient a deux profils. 
##### Le premier correspond ? la probabilit? qu'il connaisse la faillite avant la fin de la fenetre d'observations.#####
##### Le second correspond ? l'origine de la faillite. #####

#####Dans les deux cas, ces valeurs sont associ?es ? des r?alisations de la loi uniforme sur 0,1. #####
#####On associe notamment la valeur 1 pour (u2) ? une certaine cause. #####
######4) Pour d?terminer la cause de la faillite, on calcule la rapport des risque des risques instantan?s. #####
######L'id?e est que l'on utilisera les r?alisations u1 et u2 comme des r?sultats de la fonction de r?partition ####
###### pour d'un c?t? obtenir un quantile d'ordre u1 pour la loi weibull et de l'autre pour obtenir####
#### un quantile d'ordre u2 pour la loi Bernoulli (0 et 1 selon la cause de la faillite).####

#### type1 correspondra ? "decreasing", "constant" et "increasing". 
#### m?me chose pour le type2. 
generation_comp<-function(p_cause1,p_cause2,t_star,nombre_obs,graine,type1,type2){
  alpha1<-get_alpha(p_cause1,obswin=t_star,typ="weibull",typ_wb=type1)
  alpha2<-get_alpha(p_cause2,obswin=t_star,typ="weibull",typ_wb=type2)
  liste_dataset<-get_dataset0(n=nombre_obs,alpha1,alpha2,tstar=t_star,graine=graine,K=1,type="weibull")
  data<-liste_dataset$data_complete
  data<-as.data.frame(data)
  data$is_observed<-ifelse(data$status==0,0,1)
  data_estim<-data[,c("status","time","is_observed")]
  colnames(data_estim)<-c("status","tox_time","is_observed")
  return(data_estim)
}

generation_comp(p_cause1 = 0.3, p_cause2 = 0.7, t_star = 6, nombre_obs = 10, graine = 133,type1 = 0.3, type2 = 3)
#get_dataset0_Ntimes<- function(n=60, alpha1, alpha2,  tstar, K=5, graine=1234 , type='exponential',N) {
#  liste_realisation<-list(c(1:N))
#  set.seed(graine)
#  for (i in c(1:N)){


 # if(type=='exponential'){
 #   alpha1 <- rep(alpha1, each=n)
  #  alpha2 <- rep(alpha2, each=n)
  #  
  #  u1 <- rep(runif(n), K) # individual profile for time to any event for each patient (n distinct values), each repeated for each dose level. total length=n*K
  #  u2 <- rep(runif(n), K) # individual profile for type of event for each patient (n distinct values), each repeated for each dose level. total length=n*K
    
   # res <- as.data.frame(get_expo(n, K, alpha1, alpha2, u1, u2))
  #}
  #if(type=="weibull"){
   # u1 <- rep(runif(n), K)
    #u2 <- rep(runif(n), K)
    
  #  res <- as.data.frame(get_weibull(n*K, a1=rep(alpha1[,1], each=n), b1=rep(alpha1[,2], each=n), a2=rep(alpha2[,1], each=n), b2=rep(alpha2[,2],each=n), u1, u2))
    
#  }
  
  #dd <- ifelse(res$tt > tstar, 0, res$dd) #administrative censoring
  #tt <- ifelse(res$tt > tstar, tstar+1, res$tt) #administrative censoring
  #baz <- data.frame(id=1:(n*K), u1=res$u1, u2=res$u2, tt=ceiling(tt*7)/7, dd=dd, status=dd, tstar=tstar)
  #baz$id <- rep(1:n, K)
  #baz$dose <- rep(1:K, each=n)
  #baz <- baz[order(baz$id), c('id','u1' ,'u2', 'tt',"status",'dose')] # ordered by patient
  #colnames(baz) <- c('patient','u1' ,'u2', 'time',"status",'dose')
  #baz <- as.matrix(baz)
  #T_entrance_basic = c(0,rep(tstar,n-1))# time of inclusion (on the trial time scale), uniform, 1 patient per window
  #T_entrance_basic = cumsum(T_entrance_basic)
  #T_entrance_fixrapid = c(0, rep(tstar/4 ,n-1))# time of inclusion (on the trial time scale), uniform, 4 patients per window
  #T_entrance_fixrapid = cumsum(T_entrance_fixrapid)
#  T_entrance_fixslow= c(0, rep(tstar/2 ,n-1))# time of inclusion (on the trial time scale), uniform, 2 patients per window
 # T_entrance_fixslow = cumsum(T_entrance_fixslow)
#  T_entrance <- data.frame(T_entrance_basic=T_entrance_basic,
 #                          T_entrance_fixslow=T_entrance_fixslow,
  #                         T_entrance_fixrapid=T_entrance_fixrapid
                           
  #)
  #liste_realisation[[i]]<-list(data_complete=baz, T_entrance=T_entrance) 
  #}
  #return(liste_realisation)
#} 
#generation_comp_Ktimes<-function(p_cause1,p_cause2,t_star,nombre_obs,graine,type1,type2,N){
#  alpha1<-get_alpha(p_cause1,obswin=t_star,typ="weibull",typ_wb=type1)
#  alpha2<-get_alpha(p_cause2,obswin=t_star,typ="weibull",typ_wb=type2)
#  result<-get_dataset0_Ntimes(n=nombre_obs,alpha1,alpha2,tstar=t_star,graine=graine,K=1,type="weibull",N=N)
 # liste_data<-list(c(1:N))
#  for (j in c(1:length(result))){
 #   data<-result[[j]]$data_complete
  #  data<-as.data.frame(data)
   # data$is_observed<-ifelse(data$status==0,0,1)
  #  data_estim<-data[,c("status","time","is_observed")]
  #  colnames(data_estim)<-c("status","tox_time","is_observed")
  #  liste_data[[j]]<-data_estim
  #}
  #return(liste_data)
#}

get_alpha(0.3, 6, typ="weibull", typ_wb = "increasing")
