source("surv.R")
source("bernoulli.R")
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
n<-10000
p<-0.33
result_print<-function_print_p_theoretical(n,p)
### le but est de calculer l'erreur de Monte Carlo du "coverage" soit de la 
### probabilite que theta soit present dans l'intervalle entre theta-inf et 
### theta-sup. 
### On somme ces probabilites pour chaque echantillon creee. 
### creation des estimateurs. 
function_calc_MCSE<-function(n,p,nsim,coverage){
  somme_proba<-0
  alpha<-1-coverage
  for(i in c(1:nsim)){
  result<-simul_bernoulli(n,p)
  estim_moins<-mean(result)-qnorm(1-(alpha/2))*var(result)
  estim_plus<-mean(result)+qnorm(1-(alpha/2))*var(result)
  reponse=ifelse((estim_plus>p && p>estim_moins)==TRUE,1,0)
  somme_proba<-somme_proba + reponse
  }
  coverage_estimated<-somme_proba/(nsim)
  MCSE<-sqrt(coverage_estimated*(1-coverage_estimated)/nsim)
  return(MCSE)
}
n<-5
nsim<-4
p<-0.33
coverage<-0.95
test_MCerror<-function_calc_MCSE(n,p,nsim,coverage)
