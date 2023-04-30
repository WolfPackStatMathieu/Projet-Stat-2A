###### IMPORTS #####
source("estimateurs/estimateur_cure.R")
source("generation_echantillon/generation_echantillon.R")
require(ggplot2)
require(cuRe)
require(flexsurvcure)
require(survival)
##### fonctions####
fnct_compar_cuRe_flex<-function(n,lambda,alpha,p,K,t_star){
  #replicate pour effectuer K fois une fonction. 
  #dans notre cas, cela va renvoyer K estimateurs de cure. 
  #simplify=FALSE car on peut sinon perdre les noms des colonnes. 
  result<-replicate(K,Generation_un_ech(n=n,lambda=lambda,p=p,k=alpha,t_star=t_star),simplify=FALSE)
  result_df<-lapply(result,as.data.frame)
  print("ici")
  result_cuRe<-sapply(result_df,fonction_cure,t_star=t_star)
  result_flex<-rep(NA,K)
  for(j in c(1:K)){
    data<-as.data.frame(result_df[j])
    opt<-flexsurvcure(Surv(tox_time,event=is_observed)~1,data=data,
                                        link="logistic", dist="weibullPH", mixture=T)
    Prob_cure<-plogis(coef(opt)[1])
    estimateur_tox<-1-Prob_cure
    result_flex[j]<-estimateur_tox
  }
  compar_data<-cbind.data.frame(result_cuRe,result_flex)
  colnames(compar_data)<-c("cuRe","flexsurv")
  return(compar_data)
}

########test#####
alpha<-2
cible<-0.98
valeur_lambda<-exp(log(t_star^(-1*alpha)*-log(1-cible))/alpha)
set.seed(133)
t_star<-6
p<-0.33
test<-fnct_compar_cuRe_flex(n=100,lambda=valeur_lambda,p=p,alpha=alpha,t_star=t_star,K=1000)
plot(density(test$cuRe))
plot(density(test$flexsurv))
