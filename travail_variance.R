########################## Fonctions calcul variance pour les différents modèles. ##########
source("surv.R")
source("weibull.R")
source("bernoulli.R")
#####  (I) Fonctions calculer la variance pour une taille d'échantillon.#####

###### 1) fonction variance survie_exp. #####
variance_exp_Ntimes_sizen<-function(N,n,lambda,t_star){
  vecteur_biais<-rep(NA,N)
  vecteur_taille<-rep(n,N)
  vecteur_resultat<-sapply(vecteur_taille,simul_survie,t_star=t_star,lambda=lambda)
  return(var(vecteur_resultat))
}
#####2) fonction variance weibull.#####
variance_weib_Ntimes_sizen<-function(N,n,lambda,k,t_star){
  vecteur_biais<-rep(NA,N)
  vecteur_taille<-rep(n,N)
  vecteur_resultat<-sapply(vecteur_taille,simul_survie_weibull,lambda=lambda,k=k,t_star=t_star)
  return(var(vecteur_resultat))
}
#####3) fonction variance bernoulli.#####
variance_bern_Ntimes_sizen<-function(N,n,p){
  vecteur_biais<-rep(NA,N)
  vecteur_taille<-rep(n,N)
  vecteur_resultat<-lapply(vecteur_taille,simul_bernoulli,p=p)
  ## on calcule la moyenne pour chaque échantillon de taille n.
  vecteur_mean_by_sample<-sapply(vecteur_resultat,mean)
  ## on calcule la variance pour les échantillons de même taille n.
  return(var(vecteur_mean_by_sample))
}

########test######
####test surv#####
t<-6
n<-100
N<-20
lambda<-0.2
var_estimated<-variance_exp_Ntimes_sizen(N,n,lambda,t_star=t)
k<-3
var_estimated_weib<-variance_weib_Ntimes_sizen(N=N,n=n,lambda=lambda,k=k,t_star=t)

#### test bern###
p<-0.33
n<-100
N<-20
var_estimated_bern<-variance_bern_Ntimes_sizen(N,n=n,p=p)

##### (II) Fonctions calculer la variance pour plusieurs tailles.####
variance_sizes<-function(vector_size,model,liste_parameter,K){
  vector_size<-vector_size[order(vector_size)]
  if (model=="bernoulli"){
    #pour plusieurs tailles, calculer la variance pour chaque taille. 
    realisation_variance<-sapply(vector_size,variance_bern_Ntimes_sizen,N=K,p=liste_parameter[["p"]])
    return(realisation_variance)
  }
  else{if(model=="surv"){
    realisations_variance<-sapply(vector_size,variance_exp_Ntimes_sizen,N=K,lambda=liste_parameter[["lambda"]],t_star=liste_parameter[["t_star"]])
    return(realisations_variance)
  }
    if(model=="weibull"){
      vecteur_realisation<-sapply(vector_size,variance_weib_Ntimes_sizen,N=K,lambda=liste_parameter[["lambda"]],t_star=liste_parameter[["t_star"]],
                                  k=liste_parameter[["k"]])
      return(vecteur_realisation)
    }}
}

#####test tailles differentes. #####
A<-50
vecteur_size<-sample(c(1:1000),A)
print(vecteur_size[order(vecteur_size)])
### test bern####
p<-0.33
n<-100
N<-20
modele<-"bernoulli"
liste_param<-list(p)
names(liste_param)<-c("p")
test_bern_var_sizes<-variance_sizes(vector_size = vecteur_size,model=modele,liste_parameter = liste_param,K=N)

###test surv exp####
n<-100
N<-20
A<-50
vecteur_size<-sample(c(1:1000),A)
lambda_test<-0.33
t<-6
liste_param_exp<-list(lambda_test,t)
names(liste_param_exp)<-c("lambda","t_star")
modele_exp<-"surv"
test_exp_var_sizes<-variance_sizes(vector_size = vecteur_size,model=modele_exp,liste_parameter = liste_param_exp,K=N)

#### test weibull####
k<-2
modele_weib<-"weibull"
liste_paramweib<-list(lambda_test,t,k)
names(liste_paramweib)<-c("lambda","t_star","k")
test_wei_var_sizes<-variance_sizes(vector_size = vecteur_size,model=modele_weib,liste_parameter = liste_paramweib,K=N)
plot(vecteur_size[order(vecteur_size)],test_wei_var_sizes)
