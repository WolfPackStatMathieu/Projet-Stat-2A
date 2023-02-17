
source("surv.R")
source("bernoulli.R")
fonction_generation_taille_differente<-function(vector_size,model,liste_parameter){
  ### renvoie la génération avec des tailles différentes du modèle model (string) ayant comme paramètre la liste_parameter, 
  ### liste de paramètres avec le modèles. 1 seul comme bernoulli et 2 pour exp() (lambda et t_star).
  if (model=="bernoulli"){
    vecteur_realisation<-sapply(vector_size,biais_pi,liste_parameter[["p"]])
    return(vecteur_realisation)
  }
  else{if(model=="surv"){
    vecteur_realisation<-sapply(vector_size,fonction_biais_survie,lambda=liste_parameter[["lambda"]],t_star=liste_parameter[["t_star"]])
    return(vecteur_realisation)
  }}
}
vecteur_size<-sample(c(1:100),10)
lamdba_test<-0.33
t_star<-6
liste_parameter<-list(lambda_test,t_star)
names(liste_parameter)<-c("lambda","t_star")
modele<-"surv"
test_generation_taillediff<-fonction_generation_taille_differente(vector_size=vecteur_size,model=modele,liste_parameter = liste_parameter)
