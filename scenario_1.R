################################ Sc�nario 1 ################################################

rm(list = ls())
source("surv.R")


plots_scenario <- function(K, n, lambda, t_star, p, k){
  require(gridExtra)
  # df � 2 colones (mod�le de gu�rison et mod�le de survie)
  res <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
  # Ajouter une abscisse pour le ggplot
  res$Echantillon <- c(1:K)
 
  # plot 1 - mod�le de gu�rison
  gg1 <- ggplot(data = res, aes(x = Echantillon, y = Modele_guerison))+
    geom_point(col = "blue1")+
    labs(x = "Echantillon", y = "Biais moyen ")+
    ggtitle("Mod�le de gu�rison")+
    theme_classic()
  
  # plot 1 - mod�le de survie
  gg2 <- ggplot(data = res, aes(x = Echantillon, y = Modele_survie))+
    geom_point(col = "red1")+
    labs(x = "Echantillon", y = "Biais moyen ")+
    ylim(range(res$Modele_guerison))+
    ggtitle("Mod�le de survie")+
    theme_classic()
  
  # Sur un m�me plan
  whole_g <- grid.arrange(gg1, gg2, ncol = 2, top = "Comparaison du biais moyen des 2 m�thodes pour N �chantillons")
  # return(whole_g)
  return(res)
  
}


plots_scenario_1 <- function(K, n, lambda, t_star, p, k){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  # df � 2 colones (mod�le de gu�rison et mod�le de survie)
  res <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
  res <- res - p
  
  
  # bornes
  borne_min <- min(res)
  borne_max <- max(res) 
  
  
  # On tranforme les colonnes d�j� pr�sentes en une seule colonne (valeurs)
  # ensuite ajouter une nouvelle colonne modele qui servira a 
  # distinguer les 2 mod�les
  df <- res %>% gather(key = "modele", value = "valeurs")
  
  # boxplot
  boxplot <- ggplot(df, aes(x = modele, y = valeurs, fill = modele)) + 
    geom_violin(alpha = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00")) +
    theme_classic()+
    ylim(borne_min, borne_max)
  
  # Add labels and title
  boxplot + 
    labs(x = "Mod�les", y = "Biais moyen", 
         title = "Comparaison du biais moyen pour K n-�chantillons",
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
  # On fixe un n de d�part � 10 individus et on incr�ment par 5 jusqu'a 50
  debut <- 10
  fin <- 100
  pas <- 5
  n <- seq(debut,fin , pas)
  
  # On cr�e une liste de dataframe nulle qu'on va stocker les biais pour 
  # differentes tailles d'�chantillons (n)
  result <- list()
  
  for(i in seq_along(n)){
    res <- Simuler_biais_taillen(K, n[i], lambda, t_star, p, k)
    result[[i]] <- res
  }
  # On veut la moyenne de chaque de colone pour n taille d'�chantillon
  # qu'on stockera dans une nouvelle dataframe
  
  # Initialiser une matrice pour stocker les moyennes de chaque colonne pour chaque dataframe
  means_matrix <- matrix(0, nrow = ncol(result[[1]]), ncol = length(result))
  
  for (i in 1:length(result)) {
    means_matrix[, i] <- colMeans(result[[i]]) - p
  }
  
  # Cr�er un dataframe � partir de la matrice de moyennes
  result_final <- data.frame(t(means_matrix), n)
  colnames(result_final) <- c("modele_guerison", "modele_survie", "taille_echantillon")
  # plot 
  borne_min <- min(result_final$modele_guerison, result_final$modele_survie)
  borne_max <- max(result_final$modele_guerison, result_final$modele_survie)

  gg <- ggplot(data = result_final, aes(x = taille_echantillon))+
    geom_smooth(aes(y = modele_guerison, col = "modele guerison"), size = 1)+
    geom_smooth(aes(y = modele_survie, col = "modele survie"), size = 1)+
    ggtitle("Evolution du biais moyen en fonction de la taille d'�chantillon") +
    xlab("Taille echantillon") + ylab("Biais moyen") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          plot.title = element_text(size = 12))+
    ylim(borne_min, borne_max)+
    labs(caption = sprintf("K = %s, lambda = %s, k = %s, n variant de %s � %s par pas de %s" , 
                           as.character(K), 
                           as.character(lambda), 
                           as.character(k),
                           as.character(debut), 
                           as.character(fin), 
                           as.character(pas)))

  return(gg)

}



plots_scenario_1(K=10000, n=100, lambda=0.5, t_star=6, p=0.3, k=1)

biais.selon.taille_echantillon(K = 10000, lambda = 0.5, t_star = 6, p = 0.3, k=1)



