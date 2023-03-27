################################ Scénario 1 ################################################

rm(list = ls())
source("surv.R")
source("generation_mean.R")
library(ggplot2)


plots_scenario_1 <- function(K, n, lambda, t_star, p, k){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  # df à 3 colones (modèle de guérison, modèle de survie, modèle de bernouilli)
  res <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
  res <- res - p
  
  # on renomme les colonnes
  colnames(res) <- c("Bernouilli", "Survie", "Guérison")
  
  # bornes
  borne_min <- min(res)
  borne_max <- max(res) 
  
  
  # On tranforme les colonnes déjà présentes en une seule colonne (valeurs)
  # ensuite ajouter une nouvelle colonne modele qui servira a 
  # distinguer les 2 modèles
  df <- res %>% gather(key = "modele", value = "valeurs")
  
  # boxplot
  boxplot <- ggplot(df, aes(x = modele, y = valeurs, fill = modele)) + 
    geom_violin(alpha = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00","Sky blue")) +
    theme_classic()+
    ylim(borne_min, borne_max)
  
  # Add labels and title
  boxplot + 
    labs(x = "Modèles", y = "Biais moyen", 
         title = "Comparaison du biais moyen pour K n-échantillons",
         caption = sprintf("K = %s, lambda = %s, k = %s, n = %s" , 
                           as.character(K), 
                            as.character(lambda), 
                           as.character(k), 
                           as.character(n))) +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"))
  
}

biais.selon.taille_echantillon <- function(K, lambda, t_star, p, k){
  # On fixe un n de départ à 10 individus et on incrément par 5 jusqu'a 100
  debut <- 10
  fin <- 100
  pas <- 5
  n <- seq(debut,fin , pas)
  
  # On calcule le biais pour K simulations et n-échantillons
  liste_parameter <- list(lambda, t_star, p, k)
  names(liste_parameter)<-c("lambda","t_star","p","k")
  result_final <- fonction_generation_taille_mean(vector_size = n, liste_parameter = liste_parameter, K=K)
  result_final$n <- n

  colnames(result_final) <- c("modele_bernoulli","modele_survie", "modele_guerison", "taille_echantillon")
  # plot
  borne_min <- min(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)
  borne_max <- max(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)

  gg <- ggplot(data = result_final, aes(x = taille_echantillon))+
    geom_smooth(aes(y = modele_guerison, col = "modele guerison"), size = 1)+
    geom_smooth(aes(y = modele_survie, col = "modele survie"), size = 1)+
    geom_smooth(aes(y = modele_bernoulli, col = "modele bernoulli"), size = 1)+
    ggtitle("Evolution du biais moyen en fonction de la taille d'échantillon") +
    xlab("Taille echantillon") + ylab("Biais moyen") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          plot.title = element_text(size = 12))+
    ylim(borne_min, borne_max)+
    labs(caption = sprintf("K = %s, lambda = %s, k = %s, n variant de %s à %s par pas de %s" ,
                           as.character(K),
                           as.character(lambda),
                           as.character(k),
                           as.character(debut),
                           as.character(fin),
                           as.character(pas)))

  return(gg)

}



plots_scenario_1(K=1900, n=100, lambda=0.5, t_star=6, p=0.3, k=1)

biais.selon.taille_echantillon(K = 1900, lambda = 0.5, t_star = 6, p = 0.3, k=1)





