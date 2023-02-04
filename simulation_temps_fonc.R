######################Simulation de temps, fonction.#####
####################################
#######################

simul_temps_exp<-function(n,beta){
  u<-runif(n)
  return(-log(1-u)/beta)
}
simul_temp_weibull<-function(n,beta,k){
  u<-runif(n)
  return((-log(1-u)/beta)^(1/k))
}