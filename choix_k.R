source("surv.R")
source("bernoulli.R")
source("generation_mean.R")
function_choice_k<-function(Coverage,target_MCerror){
  return(Coverage*(100-Coverage)/(target_MCerror)^2)
}
function_ergodic_mean<-function(n,p){
  values_pi<-rbern(n,p)
  values_mean_ergodique<-cumsum(values_pi)/(1:n)
  return(values_mean_ergodique)
}
function_print_p_theoretical<-function(n,p){
  values_ergodic<-function_ergodic_mean(n,p)
  data_size_ergodic<-cbind.data.frame(values_ergodic,c(1:n))
  colnames(data_size_ergodic)<-c("Ergodic","Size")
  result<-{plot(x=data_size_ergodic$Size,y=data_size_ergodic$Ergodic,
                main="Value of the ergodic mean",type="l",xlab="Size",ylab="Ergodic mean")
  abline(h=p,col="blue")}
  return(result)
}
### le but est de calculer l'erreur de Monte Carlo du "coverage" soit de la 
### probabilite que theta soit present dans l'intervalle entre theta-inf et 
### theta-sup. 
### On somme ces probabilites pour chaque echantillon creee. 
### creation des estimateurs. 
fonction_calc_coverage<-function(n,p,nsim,t_star,lambda,k){
  estimateurs<-Simuler_biais_taillen(nsim,n,lambda,t_star,p,k)
  estim_survie<-estimateurs$Modele_survie
  estim_cure<-estimateurs$Modele_guerison
  Estim_coverage<-sum(as.numeric((estim_survie>p & p>estim_cure)|| (estim_cure>p & p>estim_survie))/nsim)
  return(Estim_coverage)
}

function_calc_nsim<-function(n,p,cible_erreur_MC,lambda,t_star,k){
  nsim<-sample(c(1:10),1)
  coverage<-fonction_calc_coverage(n=n,nsim,t_star=t_star,lambda,p=p,k=k)
  print(coverage)
  nsim_estim<-(100*coverage*(100-100*coverage))/((cible_erreur_MC)^(2))
  return(nsim_estim)
}


##################### TEST ###############
n<-100
p<-0.33
result_print<-function_print_p_theoretical(n,p)
n<-10
nsim<-4
p<-0.33
coverage<-0.95
test_MCerror<-function_calc_MCSE(n,p,nsim,coverage)
lambda<-0.7
k_test<-3
nsim_estimated<-function_calc_nsim(n,p=p,lambda=lambda,cible_erreur_MC =0.5,t_star=6,k=k_test)

