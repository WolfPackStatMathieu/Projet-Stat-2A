################## IMPORT #######

source("generation_echantillon/generation_ech_comp.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_cure.R")
source("estimateurs/estimateur_KM.R")
library(survival)
########### fonction un n_echantillon#####
fonction_estim_comp_once<-function(p_cause1,p_cause2,n,type1,type2,t_star){
  data<-generation_comp(p_cause1 = p_cause1,p_cause2=p_cause2,t_star=t_star,nombre_obs = n,type1=type1,type2=type2)
  indices_non_obs<-which(data$status==0)
  if(length(indices_non_obs)==n){
    sous_liste<-list(rep(1,3))
    names(sous_liste)<-c("Survie","Bernoulli","Guerison")
    return(sous_liste)
  }
  estimateursurv<-fonction_KM(df=data,t_star)
  estimateurbern<-fonction_Bern(df=data)
  estimateurcure<-fonction_cure(df=data,t_star)
  sous_liste<-list(estimateursurv,estimateurbern,estimateurcure)
  names(sous_liste)<-c("Survie","Bernoulli","Guerison")
  return(sous_liste)
}
p_cause1<-0.35
p_cause2<-0.40
n<-10
type1<-"decreasing"
type2<-"decreasing"
t_star<-6
test_estim_comp<-fonction_estim_comp_once(p_cause1,p_cause2 = p_cause2,n=n,type1,type2,t_star=t_star)
set.seed(133)
Simuler_estim_mult_times<-function(K,p_cause1,p_cause2,n,type1,type2,t_star){
  result<-as.data.frame(t(cbind(sapply(rep(n,K),fonction_estim_comp_once,p_cause1=p_cause1,p_cause2=p_cause2,type1=type1,type2=type2,t_star=t_star))))
  return(colMeans(sapply(result,as.numeric)))
}
test<-Simuler_estim_mult_times(K=10,p_cause1=p_cause1,p_cause2=p_cause2,n=n,type1=type1,type2=type2,t_star=6)
