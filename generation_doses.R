source("generation_mean.R")
source("surv.R")
source("estimateurs/estimateur_cure.R")
#### Ne doit plus dépendre de l'argument modele.######
fonction_simul_doses_mean<-function(vector_size,nombre_doses,vecteur_parametres,K){
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

function_estim_doses<-function(n,liste_params,nb_doses,t_star){
  df<-matrix(NA,n,3)
  df<-as.data.frame(df)
  data_returns<-as.data.frame(matrix(NA,nb_doses,3))
  colnames(data_returns)<-c("estimateur_bernoulli","estimateur_survie","estimateur_guerison","p")
  colnames(df)<-c("dose","obs","temps")
  df$dose<-sample(c(1:nb_doses))
  for (k in c(1:nb_doses)){
    index_dosek<-which(df$dose==k)
    sous_liste<-liste_params[[k]]
    df[index_dosek,"obs"]<-simul_bernoulli(length(index_dosek),sous_liste[["p"]])
    estimateur_bern<-mean(df[index_dosek,"obs"])
    df[index_dosek,"temps"]<-ifelse(df[index_dosek,"obs"]==1,NA,t_star)
    index_obs_dosek<-which(df$dose==k & df$obs==1)
    vect1<-df[index_dosek,"obs"]
    print("------")
    if(length(index_obs_dosek)>=1){
      df[index_obs_dosek,"temps"]<-simul_weibull(length(index_obs_dosek),lambda=sous_liste[["lambda"]],k=sous_liste[["k"]])
      df[index_obs_dosek,"obs"]<-ifelse(df[index_obs_dosek,"temps"]<t_star,1,0)}
    vect2<-df[index_dosek,"obs"]
    estimateur_cure<-fonction_cure(df,t_star=t_star)
    data_dosek<-df[index_dosek,c("temps","obs")]
    data_dosek$temps<-as.numeric(data_dosek$temps)
    estimateur_surv<-Calcul_estim_depuis_df(data_dosek,nom_col_obs = "obs",nom_coltemps = "temps")
    estimateurs<-c(estimateur_bern,estimateur_surv,estimateur_cure)
    ligne_dosek<-c(estimateurs,sous_liste[["p"]])
    data_returns[k,]<-ligne_dosek
  }
  return(data_returns)
}

set.seed(133)
fonction_estim_doses_sizen<-function(K,n,liste_params,nb_doses,t_star){
  ### Génère la moyenne des estimateurs pour la taille n
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
    print(ensemble_obs_dosek)
    matrice[j,c("moyenne_estimateur_guerison","moyenne_estimateur_survie","p")]<-colMeans(ensemble_obs_dosek)
  }
  return(matrice)
}
fonction_simul_doses_eqm<-function(vector_size,nombre_doses,vecteur_parametres,K){
  vector_size<-vector_size[order(vector_size)]
  print(nombre_doses)
  matrix_bias_doses<-list(rep(NA,nombre_doses))
  for(indice in c(1:nombre_doses)){
    liste_param<-vecteur_parametres[[indice]]
    ### besoin de modifier la fonction fonction_generation_taille_mean.
    moyenne_taille_dose<-fonction_generation_eqm(vector_size = vector_size,
                                                         liste_parameter = liste_param,K)
    matrix_bias_doses[[indice]]=moyenne_taille_dose
  }
  names(matrix_bias_doses)<-c(1:nombre_doses)
  return(matrix_bias_doses)
}

######## Partie TEST#####
n<-10
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
liste_whole<-list(liste1,liste2)
t_star<-6
nb_doses<-2
test_multiple_doses<-function_estim_doses(n,liste_params = liste_whole,nb_doses=nb_doses,t_star=t_star)

K<-10
test_K_sizen<-fonction_estim_doses_sizen(K=K,n=n,liste_params = liste_whole,nb_doses=nb_doses,t_star=t_star)


########## TEST surv####
N<-20
p<-0.33
vecteur_size<-sample(c(1:100),N)
lamdba_test<-0.33
t_star<-6
k1<-1
liste_parameter<-list(lambda_test,t_star,p,k1)
names(liste_parameter)<-c("lambda","t_star","p","k")
lb_test2<-0.2
t_star2<-7
p2<-0.5
k2<-1
liste_2<-list(lb_test2,t_star2,p2,k2)
names(liste_2)<-c("lambda","t_star","p","k")
vecteur_param<-list(liste_parameter,liste_2)
nb_doses<-2
k<-20
test_surv<-fonction_simul_doses_mean(vector_size = vecteur_size,nombre_doses=nb_doses,
                                vecteur_parametres = vecteur_param,K=k)
test_eqm<-fonction_simul_doses_eqm(vector_size = vecteur_size,nombre_doses=nb_doses,
                                   vecteur_parametres = vecteur_param,K=k)
#install.packages("plotly")
library(purrr)
library(plotly)
donnees<-cbind.data.frame(vecteur_size,test_surv[1,])
colnames(donnees)<-c("Size","Mean_Bias")

donnees2<-cbind.data.frame(vecteur_size,test_surv[2,])
colnames(donnees2)<-c("Size","Mean_Bias")
graph1<-plot_ly(data=donnees,type="scatter",x=~Size,y=~Mean_Bias)
graph1<-graph1 %>% layout(xaxis = list(title = 'Size'), yaxis = list(title = 'Bias with first dose'))
graph1
graph2<-plot_ly(data=donnees,type="scatter",x=~Size,y=~Mean_Bias)
graph2<-graph2 %>% layout(xaxis = list(title = 'Size'), yaxis = list(title = 'Bias with second dose'))
fig<-subplot(graph1,graph2,nrows=2) 
fig<-fig %>% layout(plot_bgcolor='#e5ecf6')
fig

