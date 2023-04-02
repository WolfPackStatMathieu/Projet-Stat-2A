source("generation_doses.R")
plots_scenario_mult <- function(K, n,liste_params, t_star,nb_doses){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  # df à 3 colones (modèle de guérison, modèle de survie, modèle de bernouilli)
  res <- Realisations_estim_cas_mult(K,n,liste_params,nb_doses,t_star)
  result<-list(rep(NA,nb_doses))
  # on renomme les colonnes
  for (j in c(1:nb_doses)){
  p<-liste_params[["p"]]
  lambda<-liste_params[["lambda"]]
  k<-liste_params[["k"]]
  res_j<-as.data.frame(res[[j]][,c("estimateur_bernoulli","estimateur_guerison","estimateur_survie")])-res[[j]]$p
  colnames(res_j) <- c("Bernoulli", "Guérison", "SUrvie")
  
  # bornes
  borne_min <- min(res_j)
  borne_max <- max(res_j) 
  
  
  # On tranforme les colonnes déjà présentes en une seule colonne (valeurs)
  # ensuite ajouter une nouvelle colonne modele qui servira a 
  # distinguer les 2 modèles
  df <- res_j %>% gather(key = "modele", value = "valeurs")
  
  # boxplot
  boxplot <- ggplot(df, aes(x = modele, y = valeurs, fill = modele)) + 
    geom_violin(alpha = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00","Sky blue")) +
    theme_classic()+
    ylim(borne_min, borne_max)
  
  # Add labels and title
  result[[j]]<-{boxplot + 
    labs(x = "Modèles", y = "Biais moyen", 
         title = "Comparaison du biais moyen pour K n-échantillons",
         caption = sprintf("K = %s, lambda = %s, k = %s, n = %s,p=%s" , 
                           as.character(K), 
                           as.character(lambda), 
                           as.character(k), 
                           as.character(n),
                           as.character(p))) +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"))}
  }
  return(result)
}
p<-0.33
lambda_test<-0.33
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
test<-plots_scenario_mult(K=1900,n=100,liste_params = vecteur_param,t_star=6,nb_doses=2)
