#install.packages("Rlab")                         # Install Rlab package
library("Rlab")
simul_bernoulli<-function(n,p){
  ### simuler un n_echantillon d'une Bernoulli(p).
  return(rbern(n,p))
}
n<-10
test<-simul_bernoulli(100,0.1)

biais_pi<-function(n,p){
  ### calculer le biais de la probabilite estimee de toxicite selon une Bernoulli(p). 
  ### Comparaison avec la valeur theorique. 
  simulation<-simul_bernoulli(n,p)
  biais<-abs(mean(simulation)-p)
  return(biais)
}
test_biais<-biais_pi(18,0.33)


