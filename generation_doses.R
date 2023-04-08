source("generation_mean.R")
source("surv.R")
source("estimateurs/estimateur_cure.R")
source("utils.R")
set.seed(133)
#### Ne doit plus dependre de l'argument modele.######
fonction_simul_doses_mean<-function(vector_size,nombre_doses,vecteur_parametres,K){
  # vector_size :
  vector_size<-vector_size[order(vector_size)]
  matrix_bias_doses<-list(rep(NA,nombre_doses))
  for(indice in c(1:nombre_doses)){
    liste_param<-vecteur_parametres[[indice]]
    ### besoin de modifier la fonction fonction_generation_taille_mean.
    moyenne_taille_dose<-fonction_generation_taille_mean(vector_size = vector_size,
                                                         liste_parameter = liste_param,K)
    matrix_bias_doses[[indice]]=moyenne_taille_dose
  }
  names(matrix_bias_doses)<-c(1:nombre_doses)
  return(matrix_bias_doses)
}
####### Calculer l'EQM des deux estimateurs pour plusieurs tailles. #####################
fonction_generation_eqm<-function(vector_size,liste_parameter,K){
  #vector_size: un vecteur de N tailles d echantillons chacun de taille n_i
  # liste_parameter: la liste des parametres du modele
  # K: le nombre d echantillons (pour chaque taille n_i, il y aura K echantillons)
  
  ### renvoie la generation avec des tailles differentes avec un lambda,k,t_star,p. 
  # on classe les échantillons par ordre croissant
  vector_size<-vector_size[order(vector_size)]
  # on calcule la valeur du biais pour chaque echantillon de chaque taille
  Value_bias<-lapply(vector_size,Simuler_biais_taillen,K=K,lambda=liste_parameter[['lambda']],t_star=liste_parameter[["t_star"]],
                     p=liste_parameter[["p"]],k=liste_parameter[["k"]])
  function_eqm<-function(data,p){
    return(colMeans((data-p)^2))
  }
  # on calcule la valeur moyenne du biais pour chaque taille d echantillon
  value_eqm<-as.data.frame(t(sapply(Value_bias,function_eqm,p=liste_parameter[["p"]])))
  return(value_eqm)
  # on calcule la variance du biais pour chaque taille d echantillon
}
N<-10
vecteur_size<-sample(c(1:100),N)
lambda_test<-3
t_star<-6
p<-0.33
k<-1
liste_parameter<-list(lambda_test,t_star,p=p,k)
names(liste_parameter)<-c("lambda","t_star","p","k")
K2<-20
test_exp_eqm<-fonction_generation_eqm(vector_size=vecteur_size,K=K2,liste_parameter = liste_parameter)
##############################################################

function_estim_doses<-function(n,liste_params,nb_doses,t_star){
  require(dfcrm)
  df<-matrix(NA,n,4)
  df<-as.data.frame(df)
  data_returns<-as.data.frame(matrix(NA,nb_doses,4))
  colnames(data_returns)<-c("estimateur_bernoulli","estimateur_survie","estimateur_guerison","p")
  colnames(df)<-c("dose","sensible","tox_time","is_observed")
  df$dose<-sample(c(1:nb_doses),n,replace=TRUE)
  for (k in c(1:nb_doses)){
    index_dosek<-which(df$dose==k)
    sous_liste<-liste_params[[k]]
    n_k<-length(index_dosek)
    df[index_dosek,]<-cbind(rep(k,n_k),Generation_un_ech(n=n_k,lambda=sous_liste[["lambda"]],t_star=t_star,p=sous_liste[["p"]],k=sous_liste[["k"]]))
    data_returns[k,"estimateur_bernoulli"]<-fonction_Bern(df[index_dosek,])
    data_returns[k,"p"]<-sous_liste[["p"]]
  }
  dose_recalibree<-crm(prior=data_returns$p,target=0.50, tox =df$is_observed, level =df$dose,n=n, model="logistic")$dosescaled
  fonction_surv<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  indice_cens<-which(df$is_observed==0)
  if(length(indice_cens)==0){
    estimateur_cure<-rep(1,nb_doses)
    estimateur_surv<-rep(1,nb_doses)
    data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimateur_surv,estimateur_cure)
  }
  else{
  df$factdose<-as.factor(dose_recalibree[df$dose])
  fit_surv <- survfit(fonction_surv ~factdose, data = df)
  fit_cure<-flexsurvcure(Surv(tox_time,event=is_observed)~factdose, data = df, link="logistic", dist="weibullPH", mixture=T)
  Predicted_survival_prob<-summary(fit_cure, t=t_star, type="survival", tidy=T)
  colnames(Predicted_survival_prob)<-c("time","est","lcl","ucl","categorie")
  estimation_cure<-rep(NA,nb_doses)
  estimation_surv<-rep(NA,nb_doses)
  for (j in c(1:nb_doses)){
    indice<-which(Predicted_survival_prob$categorie==dose_recalibree[j])
    estimation_cure[j]<-1-Predicted_survival_prob[indice,"est"]
    estimation_surv[j]<-1-tp.surv(fit_surv,t_star)[[j]][1,][["surv"]]}
  }
  data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimation_surv,estimation_cure)
  return(data_returns)
}

###########################################
fonction_estim_doses_sizen<-function(K,n,liste_params,nb_doses,t_star){
  ### G?n?re la moyenne des estimateurs pour la taille n
  result<-lapply(rep(n,K),function_estim_doses,liste_params=liste_params,nb_doses=nb_doses,t_star=t_star)
  matrice<-as.data.frame(matrix(NA,nb_doses,4))
  colnames(matrice)<-c("numero_dose","moyenne_estimateur_survie","moyenne_estimateur_guerison","p")
  matrice$numero_dose<-c(1:nb_doses)
  for(j in c(1:nb_doses)){
    ensemble_obs_dosek<-t(cbind.data.frame(sapply(result,function(x,indice){return(x[indice,])},indice=j)))
    ensemble_obs_dosek<-as.data.frame(ensemble_obs_dosek)
    ensemble_obs_dosek$estimateur_bernoulli<-as.numeric(ensemble_obs_dosek$estimateur_bernoulli)
    ensemble_obs_dosek$estimateur_guerison<-as.numeric(ensemble_obs_dosek$estimateur_guerison)
    ensemble_obs_dosek$estimateur_modele_survie<-as.numeric(ensemble_obs_dosek$estimateur_survie)
    ensemble_obs_dosek$p<-as.numeric(ensemble_obs_dosek$p)
    matrice[j,c("moyenne_estimateur_guerison","moyenne_estimateur_survie","p")]<-colMeans(ensemble_obs_dosek)
  }
  return(matrice)
}
Realisations_estim_cas_mult<-function(K,n,liste_params,nb_doses,t_star){
  ### G?n?re la moyenne des estimateurs pour la taille n
  result<-lapply(rep(n,K),function_estim_doses,liste_params=liste_params,nb_doses=nb_doses,t_star=t_star)
  matrice<-list(rep(NA,nb_doses))
  for(j in c(1:nb_doses)){
    ensemble_obs_dosek<-t(cbind.data.frame(sapply(result,function(x,indice){return(x[indice,])},indice=j)))
    ensemble_obs_dosek<-as.data.frame(ensemble_obs_dosek)
    ensemble_obs_dosek$estimateur_bernoulli<-as.numeric(ensemble_obs_dosek$estimateur_bernoulli)
    ensemble_obs_dosek$estimateur_guerison<-as.numeric(ensemble_obs_dosek$estimateur_guerison)
    ensemble_obs_dosek$estimateur_modele_survie<-as.numeric(ensemble_obs_dosek$estimateur_survie)
    ensemble_obs_dosek$p<-as.numeric(ensemble_obs_dosek$p)
    matrice[[j]]<-ensemble_obs_dosek}
  return(matrice)
}
fonction_simul_doses_eqm<-function(vector_size,vecteur_parametres,K,t_star){
  vector_size<-vector_size[order(vector_size)]
  nb_doses<-length(vecteur_parametres)
  liste_gg<-list(rep(NA,nb_doses))
  resultat_all_sizes<-lapply(vector_size,calcul_eqm_size_Ktimes,nb_doses=nb_doses,K=K,vecteur_param=vecteur_parametres,t_star=t_star)
  for (d in c(1:nb_doses)){
    ensemble_eqm_dosek<-as.data.frame(t(cbind(sapply(resultat_all_sizes,function(x,indice){return(x[indice,])},indice=d))))
    print(ensemble_eqm_dosek)
    gg1<-{ggplot(data=ensemble_eqm_dosek,aes(x=n,y=eqm_guerison))}
    liste_gg[[d]]<-gg1
  }
  return(liste_gg)
}
calcul_eqm_size_Ktimes<-function(size,vecteur_param,nb_doses,K,t_star){
  
  matrice_eqm_doses<-as.data.frame(matrix(NA,nb_doses,5))
  colnames(matrice_eqm_doses)<-c("eqm_Bernoulli","eqm_guerison","eqm_survie","p","n")
  result<-lapply(rep(n,K),function_estim_doses,liste_params=vecteur_param,nb_doses=nb_doses,t_star=t_star)
  for(j in c(1:nb_doses)){
    ensemble_obs_dosek<-t(cbind.data.frame(sapply(result,function(x,indice){return(x[indice,])},indice=j)))
    ensemble_obs_dosek<-as.data.frame(ensemble_obs_dosek)
    p<-vecteur_param[[j]][["p"]]
    ensemble_obs_dosek$estimateur_bernoulli<-as.numeric(ensemble_obs_dosek$estimateur_bernoulli)
    ensemble_obs_dosek$estimateur_guerison<-as.numeric(ensemble_obs_dosek$estimateur_guerison)
    ensemble_obs_dosek$estimateur_modele_survie<-as.numeric(ensemble_obs_dosek$estimateur_survie)
    ensemble_obs_dosek$p<-as.numeric(ensemble_obs_dosek$p)
    eqm_bern<-mean((ensemble_obs_dosek$estimateur_bernoulli-p)^(2))
    eqm_surv<-mean((ensemble_obs_dosek$estimateur_modele_survie-p)^(2))
    eqm_cure<-mean((ensemble_obs_dosek$estimateur_guerison-p)^(2))
    matrice_eqm_doses[j,c("eqm_Bernoulli","eqm_guerison","eqm_survie","p","n")]<-c(eqm_bern,
                                                                               eqm_cure,
                                                                               eqm_surv,
                                                                               ensemble_obs_dosek$p,
                                                                               size)
  }
  return(matrice_eqm_doses)
}
######## Partie TEST#####
n<-25
k<-1
lambda<-0.1
p<-0.33
k2<-1
lambda2<-0.2
p2<-0.6
liste1<-list(lambda,k,p)
names(liste1)<-c("lambda","k","p")
liste2<-list(lambda2,k2,p2)
names(liste2)<-c("lambda","k","p")
t_star<-6
nb_doses<-2
p3<-0.7
k3<-3
lambda3<-0.6
liste3<-list(lambda3,k3,p3)
names(liste3)<-c("lambda","k","p")
liste_whole<-list(liste1,liste2,liste3)
test_multiple_doses<-function_estim_doses(n,liste_params = liste_whole,nb_doses=3,t_star=t_star)
K<-10
test_K_sizen<-fonction_estim_doses_sizen(K=K,n=n,liste_params = liste_whole,nb_doses=length(liste_whole),t_star=t_star)
vect_size<-sample(c(20:100),10)
liste_alt<-list(liste1,liste2)
test<-fonction_simul_doses_eqm(vector_size=vect_size,vecteur_parametres = liste_alt,K=1,t_star=t_star)

########## TEST surv####
# N<-20
# p<-0.33
# vecteur_size<-sample(c(1:100),N)
# lambda_test<-0.33
# t_star<-6
# k1<-1
# liste_parameter<-list(lambda_test,t_star,p,k1)
# names(liste_parameter)<-c("lambda","t_star","p","k")
# lb_test2<-0.2
# t_star2<-7
# p2<-0.5
# k2<-1
# liste_2<-list(lb_test2,t_star2,p2,k2)
# names(liste_2)<-c("lambda","t_star","p","k")
# vecteur_param<-list(liste_parameter,liste_2)
# nb_doses<-2
# k<-20
# test_surv<-fonction_simul_doses_mean(vector_size = vecteur_size,nombre_doses=nb_doses,
#                                 vecteur_parametres = vecteur_param,K=k)
# test_eqm<-fonction_simul_doses_eqm(vector_size = vecteur_size,nombre_doses=nb_doses,
                                   # vecteur_parametres = vecteur_param,K=k)
#install.packages("plotly")
library(purrr)
library(plotly)
# donnees<-cbind.data.frame(vecteur_size,test_surv[1,])
# colnames(donnees)<-c("Size","Mean_Bias")
# 
# donnees2<-cbind.data.frame(vecteur_size,test_surv[2,])
# colnames(donnees2)<-c("Size","Mean_Bias")
# graph1<-plot_ly(data=donnees,type="scatter",x=~Size,y=~Mean_Bias)
# graph1<-graph1 %>% layout(xaxis = list(title = 'Size'), yaxis = list(title = 'Bias with first dose'))
# graph1
# graph2<-plot_ly(data=donnees,type="scatter",x=~Size,y=~Mean_Bias)
# graph2<-graph2 %>% layout(xaxis = list(title = 'Size'), yaxis = list(title = 'Bias with second dose'))
# fig<-subplot(graph1,graph2,nrows=2) 
# fig<-fig %>% layout(plot_bgcolor='#e5ecf6')
# fig

