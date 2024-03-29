################################ Sc?nario 1 ################################################

rm(list = ls())
source("utils.R")
source("surv.R")
source("generation_mean.R")
#source("analyse_alt/mult_doses_comp.R")
source("analyse_alt/travail_comp.R")
library(ggplot2)


plots_scenario_1 <- function(K, n, lambda, t_star, p, k){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  # df ? 3 colones (mod?le de gu?rison, mod?le de survie, mod?le de bernouilli)
  res <- Simuler_biais_taillen(K, n,lambda, t_star, p, k)
  res <- res - p
  
  # on renomme les colonnes
  colnames(res) <- c("Bernoulli", "Survie", "Gu?rison")

  
  # bornes
  borne_min <- min(res)
  borne_max <- max(res) 
  
  
  # On tranforme les colonnes d?j? pr?sentes en une seule colonne (valeurs)
  # ensuite ajouter une nouvelle colonne modele qui servira a 
  # distinguer les 2 mod?les
  df <- res %>% gather(key = "modele", value = "valeurs")
  
  # boxplot
  boxplot <- ggplot(df, aes(x = modele, y = valeurs, fill = modele)) + 
    geom_violin(alpha = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00","purple")) +
    # theme_classic()+
    ylim(-0.2, 0.15)
  
  # Add labels and title
  boxplot + 
    labs(x = "Modeles", y = "Biais", 
         title = "Comparaison du biais pour N simulations et n fix?"
         ,caption = sprintf("N = %s, lambda = %s, alpha = %s, n = %s , p = %s" , 
                           as.character(K), 
                            as.character(round(lambda,2)), 
                           as.character(k), 
                           as.character(n),
                           as.character(p))) +
    theme(plot.title = element_text(hjust = 0.5, size = 20)
          ,plot.subtitle = element_text(hjust = 0, size = 10)
          ,axis.text = element_text(size = 15)
          ,axis.title = element_text(size = 15)
          ,legend.text = element_text(size = 12)
          , legend.title = element_text(size = 15)
          , plot.caption = element_text(size = 12)
          # ,text = element_text(size=rel(8))
    )
  
}



set.seed(133)

plots_scenario_1(K=1900, n=100, lambda=valeur_lambda, t_star=6, p=0.3, k=1)

set.seed(134)
### use the good alpha.
cible <- 0.98
alpha<-1
t_star<-6
p<-0.33
valeur_lambda<-exp(log(t_star^(-1*alpha)*-log(1-cible))/alpha)
1-exp(-(t_star*valeur_lambda)^(alpha))
plots_scenario_1(K=1900, n=25, lambda=valeur_lambda, t_star=6, p=0.3, k=alpha)
#celui ? n=100
plots_scenario_1(K=1900, n=100, lambda=valeur_lambda, t_star=6, p=0.3, k=alpha)


# set.seed(133)
# 
# plots_scenario_1(K=1900, n=25, lambda=0.5, t_star=6, p=0.3, k=1)
# 
# set.seed(134)
# ### use the good alpha.
# cible <- 0.98
# alpha<-1
# t_star<-6
# p<-0.33
# valeur_lambda<-exp(log(t_star^(-1*alpha)*-log(1-cible))/alpha)
# 1-exp(-(t_star*valeur_lambda)^(alpha))
# plots_scenario_1(K=1900, n=25, lambda=valeur_lambda, t_star=6, p=0.3, k=alpha)
# #celui ? n=100
# plots_scenario_1(K=1900, n=100, lambda=valeur_lambda, t_star=6, p=0.3, k=alpha)
# 




biais.selon.taille_echantillon <- function(K, lambda, t_star, p, k){
  require(ggplot2)
  require(gridExtra)
  # On fixe un n de d?part ? 10 individus et on incr?ment par 5 jusqu'a 100
  debut <- 20
  fin <- 100
  pas <- 5
  n <- seq(debut,fin , pas)
  
  # On calcule le biais pour K simulations et n-?chantillons
  liste_parameter <- list(lambda, t_star, p, k)
  names(liste_parameter)<-c("lambda","t_star","p","k")
  result_final <- fonction_generation_taille_mean(vector_size = n, liste_parameter = liste_parameter, K=K)
  result_final$n <- n

  colnames(result_final) <- c("modele_bernoulli","modele_survie", "modele_guerison", "taille_echantillon")
  # plot
  borne_min <- min(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)
  borne_max <- max(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)

  borne_min <- min(result_final$modele_bernoulli, result_final$modele_guerison, result_final$modele_survie)
  borne_max <- max(result_final$modele_bernoulli, result_final$modele_guerison, result_final$modele_survie)

  gg1 <- ggplot(data = result_final, aes(x = taille_echantillon)) +
    geom_smooth(aes(y = modele_guerison, col = "guerison"), size = 1.2, alpha = 0.5) +
    geom_smooth(aes(y = modele_bernoulli, col = "bernoulli"), size = 1.2, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values = c("guerison" = "red1", "bernoulli" = "blue")) +
    ggtitle("Evolution du biais en \nfonction de n") +
    xlab("Taille echantillon") + ylab("Biais") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=20),
          axis.title=element_text(family = "Helvetica", size=20),
          plot.title = element_text(family = "Helvetica", size = 24)
          ,legend.text = element_text(size = 20)
          # , legend.title = element_text(size = 22)
          , plot.caption = element_text(size = 20)) +
    ylim(borne_min, borne_max)

  gg2 <- ggplot(data = result_final, aes(x = taille_echantillon)) +
    geom_smooth(aes(y = modele_guerison, col = "guerison"), size = 1.2, alpha = 0.5) +
    geom_smooth(aes(y = modele_survie, col = "survie"), size = 1.2, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values = c("guerison" = "red1", "survie" = "darkgreen")) +
    ggtitle("Evolution du biais moyen en \n fonction de n") +
    xlab("Taille echantillon") + ylab("Biais") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=20),
          axis.title=element_text(family = "Helvetica", size=20),
          plot.title = element_text(family = "Helvetica", size = 24),
          legend.text = element_text(size = 20)
          # , legend.title = element_text(size = 22)
          , plot.caption = element_text(size = 20)) +
    ylim(borne_min, borne_max)+
    labs(caption = sprintf("N = %s, p=%s,lambda=%s,alpha=%s" ,
                           as.character(K),
                           as.character(p),
                           as.character(round(lambda,2)),
                           as.character(alpha)))

  gg <- grid.arrange(gg1, gg2, ncol = 2, widths = c(7,7))

}

eqm.selon.taille_echantillon <- function(K, lambda, t_star, p, k){
  require(ggplot2)
  require(gridExtra)
  # On fixe un n de d?part ? 10 individus et on incr?ment par 5 jusqu'a 100
  debut <- 20
  fin <- 100
  pas <- 5
  n <- seq(debut,fin , pas)
  
  # On calcule le biais pour K simulations et n-?chantillons
  liste_parameter <- list(lambda, t_star, p, k)
  names(liste_parameter)<-c("lambda","t_star","p","k")
  result_final <- fonction_generation_taille_eqm(vector_size = n, liste_parameter = liste_parameter, K=K)
  result_final$n <- n
  colnames(result_final) <- c("modele_bernoulli","modele_survie", "modele_guerison", "taille_echantillon")
  # plot
  borne_min <- min(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)
  borne_max <- max(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)

  # define color palette
  palette <- c("#0072B2", "#D55E00", "#E69F00")

  gg1 <- {ggplot(data = result_final, aes(x = taille_echantillon)) +
    geom_smooth(aes(y = modele_guerison, col = "guerison"), size = 1.2, alpha = 0.5) +
    geom_smooth(aes(y = modele_bernoulli, col = "bernoulli"), size = 1.2, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values = c("guerison" = "red1", "bernoulli" = "blue1")) +
    xlab("Taille echantillon") + ylab("EQM") +
    #theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=20),
          axis.title=element_text(family = "Helvetica", size=20),
          plot.title = element_text(family = "Helvetica", size = 24)
          , legend.text = element_text(family = "Helvetica", size = 20)
          ,text = element_text(size=rel(20))) +
    ylim(borne_min, borne_max)}

  gg2 <- {ggplot(data = result_final, aes(x = taille_echantillon)) +
    geom_smooth(aes(y = modele_guerison, col = "guerison"), size = 1.2, alpha = 0.5) +
    geom_smooth(aes(y = modele_survie, col = "survie"), size = 1.2, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values = c("guerison" = "red1", "survie" = "darkgreen")) +
    xlab("Taille echantillon") + ylab("EQM") +
    # theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=20),
          axis.title=element_text(family = "Helvetica", size=20),
          plot.title = element_text(family = "Helvetica", size = 24)
          , legend.text = element_text(family = "Helvetica", size = 20)
          ,text = element_text(size=rel(20))) +
    ylim(borne_min, borne_max)+
    labs(caption = sprintf("N = %s, p=%s,lambda=%s,alpha=%s" ,
                           as.character(K),
                           as.character(p),
                           as.character(round(lambda,2)),
                           as.character(alpha)))}

  gg <- {grid.arrange(gg1, gg2, ncol = 2, widths = c(7,7)
                     ,top =textGrob("Evolution de l'EQM en fonction de la taille d'echantillon n",gp=gpar(fontsize=24,font=3)))}
                     
}
# 
cible<-0.98
t_star<-6
alpha<-1
valeur_lambda<-exp(log(t_star^(-1*alpha)*-log(1-cible))/alpha)
1-exp(-(t_star*valeur_lambda)^(alpha))
set.seed(133)
# plots_scenario_1(K=1900, n=25, lambda=valeur_lambda, t_star=6, p=0.3, k=alpha)
# 
# plots_scenario_1(K=1, n=100, lambda=0.5, t_star=6, p=0.3, k=1)
# plots_scenario_1_alt(K=1900,n=100,p=0.3,type1="decreasing",t_star=6, type2 = "decreasing")
# plots_scenario_1(K=1900, n=100, lambda=0.5, t_star=6, p=0.3, k=1)
# 1-exp(-(0.5*6)^(1))
# 1-exp(-(2*6)^(1))
# lb<-2
# set.seed(133)
biais.selon.taille_echantillon(K =10, lambda =valeur_lambda, t_star = 6, p = 0.3, k=alpha)
#biais.selon.taille_echantillon(K=10,t_star=6,lambda=0.5,p=0.33,k=1)
# eqm.selon.taille_echantillon(K = 1900, lambda = valeur_lambda, t_star = 6, p = 0.3, k=alpha)
# 














####################### ALT ##################
plots_scenario_1_alt <- function(K, n, p,type1,t_star,type2,graine=133){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  # df ? 3 colones (mod?le de gu?rison, mod?le de survie, mod?le de bernouilli)
  graine_liste<-graine+c(1:K)
  res <-as.data.frame(t(cbind.data.frame(sapply(graine_liste,fonction_estim_comp_once,n=n,p_cause1=p,type1=type1,type2=type2,t_star=t_star))))
  res$Survie<-as.numeric(res$Survie)
  res$Bernoulli<-as.numeric(res$Bernoulli)
  res$Guerison<-as.numeric(res$Guerison)
  res <- res - p
  # on renomme les colonnes
  
  # bornes
  borne_min <- min(res)
  borne_max <- max(res) 
  # On tranforme les colonnes d?j? pr?sentes en une seule colonne (valeurs)
  # ensuite ajouter une nouvelle colonne modele qui servira a 
  # distinguer les 2 mod?les
  df <- res %>% gather(key = "modele", value = "valeurs")
  
  # boxplot
  boxplot <- ggplot(df, aes(x = modele, y = valeurs, fill = modele)) + 
    geom_violin(alpha = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00","purple")) +
    # theme_classic()+
    ylim(borne_min, borne_max)
  
  # Add labels and title
  boxplot + 
    labs(x = "Modeles", y = "Biais", 
         title = "Comparaison du biais pour N simulations et n fixe",
         caption = sprintf("N = %s, p=%s,n=%s,type1=%s,type2=%s",as.character(K),as.character(p),as.character(n),type1,type2)) +
    theme(plot.title = element_text(hjust = 0.5, size = 20)
          ,plot.subtitle = element_text(hjust = 0, size = 10)
          ,axis.text = element_text(size = 15)
          ,axis.title = element_text(size = 15)
          ,legend.text = element_text(size = 12)
          , legend.title = element_text(size = 15)
          , plot.caption = element_text(size = 12)
          # ,text = element_text(size=rel(8))
    )
  
}

set.seed(133)
plots_scenario_1_alt(K=200,n=100,p=0.3,graine=133,type1="constant",t_star=6, type2 = "constant")


eqm.selon.taille_echantillon_alt<-function(K, type1, p,graine,t_star){
  require(ggplot2)
  require(gridExtra)
  # On fixe un n de d?part ? 10 individus et on incr?ment par 5 jusqu'a 100
  # On calcule le biais pour K simulations et n-?chantillons
  result_final<-as.data.frame(eqm.selon.alpha(K=K, type1=type1, p_cause1=p,graine=graine,type2=type1,t_star=t_star))
  colnames(result_final) <- c("taille_echantillon","modele_survie","modele_guerison", "modele_bernoulli")
  # plot
  borne_min <- min(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)
  borne_max <- max(result_final$modele_guerison, result_final$modele_survie,result_final$modele_bernoulli)
  
  gg1 <- ggplot(data = result_final, aes(x = taille_echantillon)) +
    geom_smooth(aes(y = modele_guerison, col = "guerison"), size = 1.2, alpha = 0.5) +
    geom_smooth(aes(y = modele_bernoulli, col = "bernoulli"), size = 1.2, alpha = 0.5) +
    scale_color_manual(name = "Mod?les", values = c("guerison" = "red1", "bernoulli" = "blue1")) +
    ggtitle("Evolution de l'eqm en \nfonction de n") +
    xlab("Taille echantillon") + ylab("EQM") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=10),
          axis.title=element_text(family = "Helvetica", size=12),
          plot.title = element_text(family = "Helvetica", size = 10)) +
    # ylim(borne_min, borne_max)
    ylim(0.01, 0.03)
  
  gg2 <- ggplot(data = result_final, aes(x = taille_echantillon)) +
    geom_smooth(aes(y = modele_guerison, col = "guerison"), size = 1.2, alpha = 0.5) +
    geom_smooth(aes(y = modele_survie, col = "survie"), size = 1.2, alpha = 0.5) +
    scale_color_manual(name = "Mod?les", values = c("guerison" = "red1", "survie" = "darkgreen")) +
    ggtitle("Evolution de l'eqm en \nfonction de n") +
    xlab("Taille echantillon") + ylab("EQM") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=10),
          axis.title=element_text(family = "Helvetica", size=12),
          plot.title = element_text(family = "Helvetica", size = 10)) +
    # ylim(borne_min, borne_max)+
    ylim(0.01, 0.03)+
    labs(caption = sprintf("N = %s, p=%s,type=%s" ,
                           as.character(K),
                           as.character(p),
                           as.character(type1)))
  
  gg <- grid.arrange(gg1, gg2, ncol = 2, widths = c(7,7))
}
# test_eqm<-eqm.selon.taille_echantillon_alt(K=300, type1="constant", p=0.33,graine=133,t_star=6)
# test_eqm2<-eqm.selon.taille_echantillon_alt(K=300, type1="decreasing", p=0.33,graine=133,t_star=6)
# test_eqm<-eqm.selon.taille_echantillon_alt(K=300, type1="increasing", p=0.33,graine=133,t_star=6)
