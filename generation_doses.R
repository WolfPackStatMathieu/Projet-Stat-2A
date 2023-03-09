source("generation_mean.R")

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
