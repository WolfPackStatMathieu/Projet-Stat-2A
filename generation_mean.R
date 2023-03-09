require(gridExtra)
library(ggplot2)
source("surv.R")
source("bernoulli.R")

##### Mettre dans la même liste les trois éléments en appelant la fonction "Simuler biais taille n" de surv.R (Plus besoin des deux Simuler_N_fois.)###
##### Enlever donc les conditions, devenues inutiles. 
##### Doit toujours renvoyer une moyenne mais deux liste de moyennes. Utiliser peut-être un dataframe. 
##### Le colmeans ne marchera plus comme on sera sur deux listes de plusieurs colonnes. Solutions. 
fonction_generation_taille_mean<-function(vector_size,liste_parameter,K){
  ### renvoie la génération avec des tailles différentes du modèle model (string) ayant comme paramètre la liste_parameter, 
  ### liste de paramètres avec le modèles. 1 seul comme bernoulli et 2 pour exp() (lambda et t_star).
  vector_size<-vector_size[order(vector_size)]
  ##### idée. 
  Value_bias<-lapply(vector_size,Simuler_biais_taillen,K=K,lambda=liste_parameter[['lambda']],t_star=liste_parameter[["t_star"]],
                      p=liste_parameter[["p"]],k=liste_parameter[["k"]])
  value_means<-as.data.frame(t(sapply(Value_bias,colMeans)))
  value_means$Modele_survie<-value_means$Modele_survie-p
  value_means$Modele_guerison<-value_means$Modele_guerison-p
  return(value_means)
  ##if (model=="bernoulli"){
    ##vecteur_realisation<-sapply(vector_size,Simuler_Nfois_n_echantillons_bern,N=K,p=liste_parameter[["p"]])
    ##return(colMeans(vecteur_realisation))
  ##}
  ##else{if(model=="surv"){
  ##  vecteur_realisation<-sapply(vector_size,Simuler_Nfois_n_echantillons,N=K,lambda=liste_parameter[["lambda"]],t_star=liste_parameter[["t_star"]])
  ##  return(colMeans(vecteur_realisation))
  ##}
  ##if(model=="weibull"){
  ##  vecteur_realisation<-sapply(vector_size,Simuler_Nfois_n_weibull,N=K,lambda=liste_parameter[["lambda"]],t_star=liste_parameter[["t_star"]],
  ##                              k=liste_parameter[["k"]])
  ##  return(colMeans(vecteur_realisation))
  ##}}
}
fonction_sapply<-function(x){
  return(sapply(x,var))
}

fonction_generation_eqm<-function(vector_size,liste_parameter,K){
  ### renvoie la génération avec des tailles différentes avec un lambda,k,t_star,p. 
  vector_size<-vector_size[order(vector_size)]
  ##### idée. 
  Value_bias<-lapply(vector_size,Simuler_biais_taillen,K=K,lambda=liste_parameter[['lambda']],t_star=liste_parameter[["t_star"]],
                     p=liste_parameter[["p"]],k=liste_parameter[["k"]])
  value_means<-as.data.frame(t(sapply(Value_bias,colMeans)))
  value_variance<-as.data.frame(t(sapply(Value_bias,fonction_sapply)))
  value_eqm<-(value_means)^(2)+value_variance
  return(value_eqm)
}
################# TEST exp de la méthode.#####
N<-10
vecteur_size<-sample(c(1:100),N)
lamdba_test<-3
t_star<-6
p<-0.33
k<-1
liste_parameter<-list(lambda_test,t_star,p=p,k)
names(liste_parameter)<-c("lambda","t_star","p","k")
K2<-20
test_exp_taillemoy<-fonction_generation_taille_mean(vector_size=vecteur_size,K=K2,liste_parameter = liste_parameter)
test_exp_eqm<-fonction_generation_eqm(vector_size=vecteur_size,K=K2,liste_parameter = liste_parameter)

#################### Plot des résultats en fonction de la taille.########
donnees_taille_biaismoyen<-cbind.data.frame(vecteur_size[order(vecteur_size)],test_exp_taillemoy)
colnames(donnees_taille_biaismoyen)<-c("Size","Mean_Bias_Cure","Mean_Bias_Surv")
plot(donnees_taille_biaismoyen$Size,donnees_taille_biaismoyen$Mean_Bias_Cure,main="The mean bias according to the size with Survival function")
points(x=donnees_taille_biaismoyen$Size,y=donnees_taille_biaismoyen$Mean_Bias_Surv,col="red")

#####################Test bernoulli######
prop<-0.33
liste_param<-list(prop)
names(liste_param)<-c("p")
modele<-"bernoulli"
k<-20
test_bern_taillemoy<-fonction_generation_taille_mean(vector_size=vecteur_size,model=modele,K=k,liste_parameter = liste_param)

######## Test Weibull#####
#N<-100
#vecteur_size2<-vecteur_size
#lamdba_test<-3
#t_star<-6
#k<-2
#p<-0.33
#liste_parameter<-list(lambda_test,t_star,k,p=p)
#names(liste_parameter)<-c("lambda","t_star","k","p")
#modele_wei<-"weibull"
#test_weibull<-fonction_generation_taille_mean(vector_size=vecteur_size2,model=modele,
                                              #K=k,liste_parameter = liste_param)
#donnees_weibull<-b_moy<-cbind.data.frame(vecteur_size2[order(vecteur_size2)],test_weibull)
#colnames(donnees_weibull)<-c("Size","Mean_Bias")
#plot(donnees_weibull)

#################### Plot des résultats en fonction de la taille.########
donnees_bern_biaismoyen<-cbind.data.frame(vecteur_size[order(vecteur_size)],test_bern_taillemoy)
colnames(donnees_bern_biaismoyen)<-c("Size","Mean_Bias")
h_DPImean<-KernSmooth::dpill(x=donnees_bern_biaismoyen$Size,y=donnees_bern_biaismoyen$Mean_Bias)
estimation_ymoy<-KernSmooth::locpoly(x=donnees_bern_biaismoyen$Size,y=donnees_bern_biaismoyen$Mean_Bias,bandwidth=h_DPImean,degree=3,gridsize = N)$y
plot(donnees_bern_biaismoyen,main="The mean bias according to the size with Bernoulli")
lines(x=donnees_bern_biaismoyen$Size,y=estimation_ymoy,col="blue")
