################## IMPORT #######

source("generation_echantillon/generation_ech_comp.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_cure.R")
source("estimateurs/estimateur_KM.R")

########### fonction un n_echantillon#####
fonction_estim_comp_once<-function(p_cause1,p_cause2,n,type1,type2,t_star){
  data<-generation_comp(p_cause1 = p_cause1,p_cause2=p_cause2,t_star=t_star,nombre_obs = n,type1=type1,type2=type2)
  indices_non_obs<-which(data$status==0)
  if(length(indices_non_obs)==n){
    sous_liste<-rep(0,3)
    names(sous_liste)<-c("Survie","Bernoulli","Guerison")
    return(list(sous_liste,sous_liste))
  }
  liste_estimateur<-list(rep(NA,2))
  for (i in c(1,2)){
      data_causei<-data[which(data$status==i),c("tox_time","is_observed")]
      estimateursurv<-fonction_KM(df=data_causei,t_star)
      estimateurbern<-fonction_Bern(df=data_causei)
      estimateurcure<-fonction_cure(df=data_causei,t_star)
      sous_liste<-list(estimateursurv,estimateurbern,estimateurcure)
      names(sous_liste)<-c("Survie","Bernoulli","Guerison")
      liste_estimateur[[i]]<-sous_liste
  }
  return(liste_estimateur)
}
p_cause1<-0.05
p_cause2<-0.020
n<-10
type1<-"decreasing"
type2<-"decreasing"
t_star<-6
test_estim_comp<-fonction_estim_comp_once(p_cause1,p_cause2 = p_cause2,n=n,type1,type2,t_star=t_star)
