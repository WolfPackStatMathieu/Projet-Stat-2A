source("generation_mean.R")
source("surv.R")
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
  colnames(data_returns)<-c("estimateur_guerison","estimateur_modele_survie","p")
  colnames(df)<-c("dose","obs","temps")
  df$dose<-sample(c(1:nb_doses))
  for (k in c(1:nb_doses)){
    index_dosek<-which(df$dose==k)
    sous_liste<-liste_params[[k]]
    df[index_dosek,"obs"]<-simul_bernoulli(length(index_dosek),sous_liste[["p"]])
    estimateur_cure<-mean(df[index_dosek,"obs"])
    df[index_dosek,"temps"]<-ifelse(df[index_dosek,"obs"]==1,NA,t_star)
    index_obs_dosek<-which(df$dose==k & df$obs==1)
    vect1<-df[index_dosek,"obs"]
    print(vect1)
    if(length(index_obs_dosek)>=1){
      df[index_obs_dosek,"temps"]<-simul_weibull(length(index_obs_dosek),lambda=sous_liste[["lambda"]],k=sous_liste[["k"]])
      df[index_obs_dosek,"temps"]<-ifelse(df[index_obs_dosek,"temps"]<t_star,df[index_obs_dosek,"temps"],t_star)
      df[index_obs_dosek,"obs"]<-ifelse(df[index_obs_dosek,"temps"]<t_star,1,0)}
    vect2<-df[index_dosek,"obs"]
    print(vect1==vect2)
    data_dosek<-df[index_dosek,c("temps","obs")]
    data_dosek$temps<-as.numeric(data_dosek$temps)
    estimateur_surv<-Calcul_estim_depuis_df(data_dosek,nom_col_obs = "obs",nom_coltemps = "temps")
    estimateur_surv_cure<-c(estimateur_cure,estimateur_surv)
    ligne_dosek<-c(estimateur_surv_cure,sous_liste[["p"]])
    data_returns[k,]<-ligne_dosek
  }
  return(data_returns)
}

n<-20
k<-1
lambda<-0.5
p<-0.33
k2<-1
lambda2<-1
p2<-0.5
liste1<-list(lambda,k,p)
names(liste1)<-c("lambda","k","p")
liste2<-list(lambda2,k2,p2)
names(liste2)<-c("lambda","k","p")
liste_whole<-list(liste1,liste2)
t_star<-6
nb_doses<-2
test_multiple_doses<-function_estim_doses(n,liste_params = liste_whole,nb_doses=nb_doses,t_star=t_star)
fonction_simul_doses_eqm<-function(vector_size,nombre_doses,vecteur_parametres,K){
  vector_size<-vector_size[order(vector_size)]
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
########## TEST surv####
N<-100
p<-0.33
vecteur_size<-sample(c(1:1000),N)
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
