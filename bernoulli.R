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

Simuler_Nfois_n_echantillons_bern<-function(N,n,p){
  #### Simuler 
  vecteur_biais<-rep(NA,N)
  vecteur_taille<-rep(n,N)
  ##On utilise le p donné en entrée. 
  vecteur_biais<-sapply(vecteur_taille,biais_pi,p=p)
  return(vecteur_biais)
}
N<-100
p<-0.33
test_simul_bern_total<-Simuler_Nfois_n_echantillons_bern(N,n,p)
boxplot(test_simul_bern_total,main="Distribution du biais pour le modèle de guerison",col="red")
