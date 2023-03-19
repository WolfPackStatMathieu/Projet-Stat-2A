################################ Scénario 1 ################################################

rm(list = ls())
source("surv.R")

K <- 50
n <- 25
lambda <- 0.5
t_star <- 6
p <- 0.30
k <- 1


res <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
res

plots_scenario <- function(K, n, lambda, t_star, p, k){
  require(gridExtra)
  # df à 2 colones (modèle de guérison et modèle de survie)
  res <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
  # Ajouter une abscisse pour le ggplot
  res$Echantillon <- c(1:K)
 
  # plot 1 - modèle de guérison
  gg1 <- ggplot(data = res, aes(x = Echantillon, y = Modele_guerison))+
    geom_point(col = "blue1")+
    labs(x = "Echantillon", y = "Biais moyen ")+
    ggtitle("Modèle de guérison")+
    theme_classic()
  
  # plot 1 - modèle de survie
  gg2 <- ggplot(data = res, aes(x = Echantillon, y = Modele_survie))+
    geom_point(col = "red1")+
    labs(x = "Echantillon", y = "Biais moyen ")+
    ylim(range(res$Modele_guerison))+
    ggtitle("Modèle de survie")+
    theme_classic()
  
  # Sur un même plan
  whole_g <- grid.arrange(gg1, gg2, ncol = 2, top = "Comparaison du biais moyen des 2 méthodes pour N échantillons")
  # return(whole_g)
  return(res)
  
}


plots_scenario_1 <- function(K, n, lambda, t_star, p, k){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  # df à 2 colones (modèle de guérison et modèle de survie)
  res <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
  # On tranforme les colonnes déjà présentes en une seule colonne (valeurs)
  # ensuite ajouter une nouvelle colonne modele qui servira a 
  # distinguer les 2 modèles
  df <- res %>% gather(key = "modele", value = "valeurs")
  
  # boxplot
  boxplot <- ggplot(df, aes(x = modele, y = valeurs, fill = modele)) + 
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00")) +
    theme_classic()
  
  # Add labels and title
  boxplot + 
    labs(x = "Modèles", y = "Biais moyen", title = "Comparaison du biais moyen pour N échantillons") +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14, face = "bold"))
  
}

biais.selon.taille_echantillon <- function(K, lambda, t_star, p, k){
  # On fixe un n de départ à 10 individus et on incrément par 5 jusqu'a 50
  n <- seq(10, 100, 5)
  
  # On crée une liste de dataframe nulle qu'on va stocker les biais pour 
  # differentes tailles d'échantillons (n)
  result <- list()
  
  for(i in seq_along(n)){
    res <- Simuler_biais_taillen(K, n[i], lambda, t_star, p, k)
    result[[i]] <- res
  }
  # On veut la moyenne de chaque de colone pour n taille d'échantillon
  # qu'on stockera dans une nouvelle dataframe
  
  # Initialiser une matrice pour stocker les moyennes de chaque colonne pour chaque dataframe
  means_matrix <- matrix(0, nrow = ncol(result[[1]]), ncol = length(result))
  
  for (i in 1:length(result)) {
    means_matrix[, i] <- colMeans(result[[i]])
  }
  
  # Créer un dataframe à partir de la matrice de moyennes
  result_final <- data.frame(t(means_matrix), n)
  colnames(result_final) <- c("modele_guerison", "modele_survie", "taille_echantillon")
  return(result_final)
  
}

plots_scenario_1(K, n, lambda, t_star, p, k)







# plot
gg <- ggplot(data = result_final, aes(x = taille_echantillon))+
  geom_line(aes(y = modele_guerison),col = "blue1", size = 1)+
  geom_line(aes(y = modele_survie), col = "red1", size = 1)+
  ggtitle("Comparaison du biais moyen des 2 méthodes pour un n échantillon") +
  xlab("Taille echantillon") + ylab("Biais moyen") +
  scale_color_manual(values=c("black" = "black", "Modele guerison" = "blue1", "Modele survie" = "red1")) +
  theme_classic() +
  theme(legend.title=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 16))


