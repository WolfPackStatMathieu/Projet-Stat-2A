source("generation_mean.R")
fonction_simul_doses<-function(vector_size,nombre_doses,vecteur_parametres,modele,K){
  vector_size<-vector_size[order(vector_size)]
  matrix_bias_doses<-matrix(NA,nrow = nombre_doses,ncol=length(vector_size))
  for(indice in c(1:nombre_doses)){
    liste_param<-vecteur_parametres[[indice]]
    moyenne_taille_dose<-fonction_generation_taille_mean(vector_size = vector_size,modele,
                                                         liste_parameter = liste_param,K)
    matrix_bias_doses[indice,]=moyenne_taille_dose
  }
  colnames(matrix_bias_doses)<-c(1:length(vector_size))
  return(matrix_bias_doses)
}

########## TEST surv####
N<-100
vecteur_size<-sample(c(1:1000),N)
lamdba_test<-0.33
t_star<-6
liste_parameter<-list(lambda_test,t_star)
names(liste_parameter)<-c("lambda","t_star")
lb_test2<-0.2
t_star2<-7
liste_2<-list(lb_test2,t_star2)
names(liste_2)<-c("lambda","t_star")
vecteur_param<-list(liste_parameter,liste_2)
modele<-"surv"
nb_doses<-2
k<-20
test_surv<-fonction_simul_doses(vector_size = vecteur_size,nombre_doses=nb_doses,
                                vecteur_parametres = vecteur_param,modele=modele,K=k)
#install.packages("plotly")
install.packages("purrr")
library(purrr)
library(plotly)
donnees<-cbind.data.frame(vecteur_size,test_surv[1,])
colnames(donnees)<-c("Size","Mean_Bias")

donnees2<-cbind.data.frame(vecteur_size,test_surv[2,])
colnames(donnees2)<-c("Size","Mean_Bias")
graph1<-plot_ly(data=donnees,type="scatter",x=~Size,y=~Mean_Bias)
graph1
graph2<-plot_ly(data=donnees,type="scatter",x=~Size,y=~Mean_Bias)
graph2
fig<-subplot(graph1,graph2,nrows=2)
fig
