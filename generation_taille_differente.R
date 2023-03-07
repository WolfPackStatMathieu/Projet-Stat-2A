source("surv.R")
source("bernoulli.R")
fonction_generation_taille_differente<-function(vector_size,model,liste_parameter){ezzzzzzze
  ### renvoie la génération avec des tailles différentes du modèle model (string) ayant comme paramètre la liste_parameter, 
  ### liste de paramètres avec le modèles. 1 seul comme bernoulli et 2 pour exp() (lambda et t_star).
  if (model=="bernoulli"){
    vecteur_realisation<-sapply(vector_size[order(vector_size)],biais_pi,liste_parameter[["p"]])
    return(vecteur_realisation)
  }
  else{if(model=="surv"){
    vecteur_realisation<-sapply(vector_size[order(vector_size)],fonction_biais_survie,lambda=liste_parameter[["lambda"]],t_star=liste_parameter[["t_star"]])
    return(vecteur_realisation)
  }}
}
#################TEST EXP #####################
vecteur_size<-sample(c(1:100),10)
lamdba_test<-0.33
t_star<-6
liste_parameter<-list(lambda_test,t_star)
names(liste_parameter)<-c("lambda","t_star")
modele<-"surv"
test_generation_taillediff_exp<-fonction_generation_taille_differente(vector_size=vecteur_size,model=modele,liste_parameter = liste_parameter)

#################TEST BERNOULLI ###################
prop<-0.33
list_param<-list(prop)
names(list_param)<-c("p")
modele2<-"bernoulli"
test_generation_taillediff_bern<-fonction_generation_taille_differente(vector_size = vecteur_size,model=modele2,liste_parameter = list_param)

fonction_graph_fonc_size<-function(vector_size,model,liste_parameter){
  vector_size<-vector_size[order(vector_size)]
  results_vector<-fonction_generation_taille_differente(vector_size = vector_size,model=model,liste_parameter = liste_parameter)
  bias_size_data<-cbind.data.frame(vector_size,results_vector)
  colnames(bias_size_data)<-c("Size","Bias")
  return(bias_size_data)
}

############# TEST graph ###########

######1) exp#####
N<-50
vecteur_size<-sample(c(10:1000),N)
lamdba_test<-0.33
t_star<-6
liste_parameter<-list(lambda_test,t_star)
names(liste_parameter)<-c("lambda","t_star")
modele<-"surv"
test_graph_exp<-fonction_graph_fonc_size(vector_size = vecteur_size,model=modele,liste_parameter=liste_parameter)
h_DPI<-KernSmooth::dpill(x=test_graph_exp$Size,y=test_graph_exp$Bias)
estimation_y<-KernSmooth::locpoly(x=test_graph_exp$Size,y=test_graph_exp$Bias,bandwidth=h_DPI,degree=3,gridsize = N)$y
plot(test_graph_exp,col="blue",main="Value of the bias in surv model according to the size",type="p")
lines(x=test_graph_exp$Size,y=estimation_y)
