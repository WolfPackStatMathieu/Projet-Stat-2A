require(gridExtra)
library(ggplot2)
source("surv.R")
source("bernoulli.R")

##### Mettre dans la m?me liste les trois ?l?ments en appelant la fonction "Simuler biais taille n" de surv.R (Plus besoin des deux Simuler_N_fois.)###
##### Enlever donc les conditions, devenues inutiles. 
##### Doit toujours renvoyer une moyenne mais deux liste de moyennes. Utiliser peut-?tre un dataframe. 
##### Le colmeans ne marchera plus comme on sera sur deux listes de plusieurs colonnes. Solutions. 
fonction_generation_taille_mean<-function(vector_size,liste_parameter,K){
  require(parallel)
  ### renvoie la g?n?ration avec des tailles diff?rentes du mod?le model (string) ayant comme param?tre la liste_parameter, 
  ### liste de param?tres avec le mod?les. 1 seul comme bernoulli et 2 pour exp() (lambda et t_star).
  
  #on r?ordonne vector_size par ordre des tailles d'echantillon
  vector_size<-vector_size[order(vector_size)]
  ##### id?e. 
  Value_bias<-lapply(vector_size,Simuler_biais_taillen,K=K,lambda=liste_parameter[['lambda']],t_star=liste_parameter[["t_star"]],
                      p=liste_parameter[["p"]],k=liste_parameter[["k"]])
  value_means<-as.data.frame(t(sapply(Value_bias,colMeans)))
  p<-liste_parameter[["p"]]
  value_means$Modele_survie<-value_means$Modele_survie-p
  value_means$Modele_guerison<-value_means$Modele_guerison-p
  value_means$Modele_bernoulli<-value_means$Modele_bernoulli-p
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

################# TEST exp de la m?thode.#####
N<-10
vecteur_size<-sample(c(1:100),N)
lambda_test<-3
t_star<-6
p<-0.33
k<-1
liste_parameter<-list(lambda_test,t_star,p=p,k)
names(liste_parameter)<-c("lambda","t_star","p","k")
K2<-20
test_exp_taillemoy<-fonction_generation_taille_mean(vector_size=vecteur_size,K=K2,liste_parameter = liste_parameter)

#################### Plot des r?sultats en fonction de la taille.########
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
#test_bern_taillemoy<-fonction_generation_taille_mean(vector_size=vecteur_size,K=k,liste_parameter = liste_param)

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


