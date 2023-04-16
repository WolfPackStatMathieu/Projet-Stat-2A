################## IMPORT #######

source("generation_echantillon/generation_ech_comp.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_cure.R")
source("estimateurs/estimateur_KM.R")
library(survival)
########### fonction un n_echantillon#####
fonction_estim_comp_once<-function(p_cause1,n,type1,type2,t_star,graine=133){
  p_cause2<-(1-p_cause1)
  data<-generation_comp(p_cause1 = p_cause1,p_cause2=p_cause2,t_star=t_star,nombre_obs = n,type1=type1,type2=type2,graine = graine)
  data$tox_time<-ifelse(data$status==2,t_star+1,data$tox_time)
  data$is_observed<-ifelse(data$tox_time>t_star,0,1)
  indices_non_obs<-which(data$is_observed==0)
  if(length(indices_non_obs)==n){
    # tous pas observés donc censures
    estimateursurv<-0
    estimateurbern<-0
    estimateurcure<-0
    sous_liste<-list(estimateursurv, estimateurbern, estimateurcure) 
    names(sous_liste)<-c("Survie","Bernoulli","Guerison")
    return(sous_liste)
  }
  estimateursurv<-fonction_KM(df=data,t_star)
  estimateurbern<-fonction_Bern(df=data)
  estimateurcure<-fonction_cure(df=data,t_star)
  sous_liste<-list(estimateursurv,estimateurbern,estimateurcure)
  names(sous_liste)<-c("Survie","Bernoulli","Guerison")
  return(sous_liste)
}
#fonction_estim_depuis_df<-function(data){
 # data$tox_time<-ifelse(data$status==2,t_star+1,data$tox_time)
  #data$is_observed<-ifelse(data$tox_time>t_star,0,1)
  #indices_non_obs<-which(data$is_observed==0)
  #if(length(indices_non_obs)==n){
    # tous pas observés donc censures
   # estimateursurv<-0
    #estimateurbern<-0
    #estimateurcure<-0
  #  sous_liste<-list(estimateursurv, estimateurbern, estimateurcure) 
   # names(sous_liste)<-c("Survie","Bernoulli","Guerison")
    #return(sous_liste)
  #}
  #estimateursurv<-fonction_KM(df=data,t_star)
  #estimateurbern<-fonction_Bern(df=data)
  #estimateurcure<-fonction_cure(df=data,t_star)
  #sous_liste<-list(estimateursurv,estimateurbern,estimateurcure)
  #names(sous_liste)<-c("Survie","Bernoulli","Guerison")
  #return(sous_liste)
#}

p_cause1<-0.35
n<-10
type1<-"decreasing"
type2<-"decreasing"
t_star<-6
test_estim_comp<-fonction_estim_comp_once(p_cause1,n=n,type1,type2,t_star=t_star,graine=145)
set.seed(133)
Simuler_estim_mult_times<-function(K,p_cause1,n,type1,type2,t_star,graine){
  graine_inf <- graine
  graine_sup <- graine + K-1
  ensemble_graine<-c(graine_inf:graine_sup)
  result<-cbind(sapply(ensemble_graine,fonction_estim_comp_once,p_cause1=p_cause1,type1=type1,type2=type2,t_star=t_star,n=n))
  result<-as.data.frame(t(result))
  colnames(result)<-c("Survie","Bernoulli","Guerison")
  return(colMeans(sapply(result,as.numeric)))
}
#Simuler_estim_mult_test<<-function(K,p_cause1,n,type1,type2,t_star,graine){
 # ensemble_liste<-generation_comp_Ktimes(p_cause1=p_cause1,p_cause2=1-p_cause1,t_star=t_star,nombre_obs=n,graine=graine,type1=type1
  #                                       ,type2=type2,N=K)
  #result<-as.data.frame(t(cbind.data.frame(sapply(ensemble_liste,fonction_estim_depuis_df))))
  #result$Survie<-as.numeric(result$Survie)
  #result$Bernoulli<-as.numeric(result$Bernoulli)
  #result$Guerison<-as.numeric(result$Guerison)
  #return(colMeans(result))
#}
#test<-Simuler_estim_mult_times(K=5,p_cause1 = 0.33,type1='constant',type2="constant",t_star=6,graine=133,n=10)
#test<-Simuler_estim_mult_times(K=10,p_cause1=p_cause1,p_cause2=p_cause2,n=n,type1=type1,type2=type2,t_star=6)
biais.selon.lambda_alt <-function(p_cause1,K,t_star,type1,type2,graine){
  results <- NULL
  n <- 25
  while(n<200){
    vec.biais <- Simuler_estim_mult_times(K=K,p_cause1=p_cause1,n=n,type1=type1,type2=type2,t_star=t_star,graine=graine)
    biais_surv<-vec.biais[[1]]-p_cause1
    biais.bern<-vec.biais[[2]]-p_cause1
    biais.cure<-vec.biais[[3]]-p_cause1
    results<-rbind(results,c(n,biais_surv,biais.cure,biais.bern))
    n <- n+5
  }
  return(results)
}
eqm.selon.alpha<-function(p_cause1,K,t_star,type1,type2,graine){
  results <- NULL
  n <- 18
  graine_inf <- graine
  graine_sup <- graine + K
  ensemble_graine<-c(graine_inf:graine_sup)
  while(n<200){
    liste_global<-as.data.frame(t(cbind.data.frame(sapply(ensemble_graine,fonction_estim_comp_once,
                                          p_cause1=p_cause1,type1=type1,type2=type2,t_star=t_star,n=n))))
    liste_global$Guerison<-as.numeric(liste_global$Guerison)
    liste_global$Survie<-as.numeric(liste_global$Survie)
    liste_global$Bernoulli<-as.numeric(liste_global$Bernoulli)
    valeurs<-colMeans((liste_global-p_cause1)^2)
    results<-rbind(results,c(n,valeurs[1],valeurs[3],valeurs[2]))
    n <- n+20
  }
  return(results)
}

fonction_compar_plotsn_lambda_alt <- function(N,t_star, vect_cause1,type1,type2,graine) {
  library(gridExtra)
  library(ggplot2)
  # Generate the data
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[1]],K=N, t_star=t_star, type1,type2,graine=graine)
  RES0.3.3 <- data.frame(RES)
  colnames(RES0.3.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[2]],K=N, t_star=t_star, type1,type2,graine=graine)
  RES0.5.3 <- data.frame(RES)
  colnames(RES0.5.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[3]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.7.3 <- data.frame(RES)
  colnames(RES0.7.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  # Get the min and max bounds of each variable to be used in the plots
  borne_min <- min(
    min(RES0.3.3$mean.surv),
    min(RES0.5.3$mean.surv),
    min(RES0.7.3$mean.surv)
  )
  
  borne_max <- max(
    max(RES0.7.3$mean.surv),
    max(RES0.3.3$mean.surv),
    max(RES0.5.3$mean.surv)
  )
  
  borne_min.c <- min(
    min(RES0.7.3$mean.cure),
    min(RES0.3.3$mean.cure),
    min(RES0.3.3$mean.cure)
  )
  
  borne_max.c <- max(
    max(RES0.7.3$mean.cure),
    max(RES0.5.3$mean.cure),
    max(RES0.3.3$mean.cure)
  )
  
  borne_min.b <- min(
    min(RES0.5.3$mean.bernoulli),
    min(RES0.7.3$mean.bernoulli),
    min(RES0.3.3$mean.bernoulli)
  )
  
  borne_max.b <- max(
    max(RES0.7.3$mean.bernoulli),
    max(RES0.5.3$mean.bernoulli),
    max(RES0.3.3$mean.bernoulli)
  )
  
  # Plot the data
  gg1 <-  ggplot(RES0.3.3, aes(n, mean.surv)) +
    geom_line(aes(color = "0.3"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.surv, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.7.3, aes(n, mean.surv, color = "0.7"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("red", "black", "blue")) +
    ylim(borne_min -0.1, borne_max+0.1)+
    labs(
      title = "Modèle de survie",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  
  gg2 <-  ggplot(RES0.3.3, aes(n, mean.cure)) +
    geom_line(aes(color = "0.3"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.cure, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.7.3, aes(n, mean.cure, color = "0.7"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("red", "black", "blue")) +
    ylim(borne_min.c -0.1, borne_max.c+0.1)+
    labs(
      title = "Modèle de guérison",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  
  gg3 <-  ggplot(RES0.3.3, aes(n, mean.bernoulli)) +
    geom_line(aes(color = "0.3"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.bernoulli, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.7.3, aes(n, mean.bernoulli, color = "0.7"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("red", "black", "blue")) +
    ylim(borne_min.b -0.1, borne_max.b+0.1)+
    labs(
      title = "Modèle de Bernoulli",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  
  g <- grid.arrange(gg1, gg2, gg3, top = "influence de n et de p1")
  return(g)
  
}
# test_alt<-fonction_compar_plotsn_lambda_alt(N=500, t_star=6, vect_cause1=c(0.33,0.5,0.7),type1="decreasing",type2="decreasing",graine=133)
# test_alt2<-fonction_compar_plotsn_lambda_alt(N=10, t_star=6, vect_cause1=c(0.33,0.5,0.7),type1="increasing",type2="increasing",graine=133)
# test_alt3<-fonction_compar_plotsn_lambda_alt(N=10, t_star=6, vect_cause1=c(0.33,0.5,0.7),type1="constant",type2="constant",graine=133)




fonction_compar_plotsn_lambda_alt_8p <- function(N,t_star, vect_cause1=c(0.03,0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7),type1,type2,graine=133) {
  library(gridExtra)
  library(ggplot2)
  library(scales)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[1]],K=N, t_star=t_star,type1,type2,graine=graine)
  RES0.3.3 <- data.frame(RES)
  colnames(RES0.3.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[2]],K=N, t_star=t_star,type1,type2,graine=graine)
  RES0.5.3 <- data.frame(RES)
  colnames(RES0.5.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[3]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.7.3 <- data.frame(RES)
  colnames(RES0.7.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[4]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.4 <- data.frame(RES)
  colnames(RES0.4) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[5]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.5 <- data.frame(RES)
  colnames(RES0.5) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[6]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.6 <- data.frame(RES)
  colnames(RES0.6) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[7]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.7 <- data.frame(RES)
  colnames(RES0.7) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[8]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.8 <- data.frame(RES)
  colnames(RES0.8) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  # Get the min and max bounds of each variable to be used in the plots
  # les bornes min et max du modele de survie
  borne_min <- min(
    min(RES0.3.3$mean.surv),
    min(RES0.5.3$mean.surv),
    min(RES0.7.3$mean.surv)
    ,min(RES0.4$mean.surv)
    ,min(RES0.5$mean.surv)
    ,min(RES0.6$mean.surv)
    ,min(RES0.7$mean.surv)
    ,min(RES0.8$mean.surv)
  )
  
  borne_max <- max(
    max(RES0.7.3$mean.surv),
    max(RES0.3.3$mean.surv),
    max(RES0.5.3$mean.surv)
    ,max(RES0.4$mean.surv)
    ,min(RES0.5$mean.surv)
    ,min(RES0.6$mean.surv)
    ,max(RES0.7$mean.surv)
    ,max(RES0.8$mean.surv)
  )
  # les bornes min et max du modele de guerison
  borne_min.c <- min(
    min(RES0.7.3$mean.cure),
    min(RES0.3.3$mean.cure),
    min(RES0.3.3$mean.cure)
    ,min(RES0.4$mean.cure)
    ,min(RES0.5$mean.cure)
    ,min(RES0.6$mean.cure)
    ,min(RES0.7$mean.cure)
    ,min(RES0.8$mean.cure)
  )
  
  borne_max.c <- max(
    max(RES0.7.3$mean.cure),
    max(RES0.5.3$mean.cure),
    max(RES0.3.3$mean.cure)
    ,max(RES0.4$mean.cure)
    ,max(RES0.4$mean.cure)
    ,max(RES0.5$mean.cure)
    ,max(RES0.6$mean.cure)
    ,max(RES0.7$mean.cure)
    ,max(RES0.8$mean.cure)
  )
  
  # les bornes min et max du modele de Bernoulli
  borne_min.b <- min(
    min(RES0.5.3$mean.bernoulli),
    min(RES0.7.3$mean.bernoulli),
    min(RES0.3.3$mean.bernoulli)
    ,min(RES0.4$mean.bernoulli)
    ,min(RES0.5$mean.bernoulli)
    ,min(RES0.6$mean.bernoulli)
    ,min(RES0.7$mean.bernoulli)
    ,min(RES0.8$mean.bernoulli)
  )
  
  borne_max.b <- max(
    max(RES0.7.3$mean.bernoulli),
    max(RES0.5.3$mean.bernoulli),
    max(RES0.3.3$mean.bernoulli)
    ,max(RES0.4$mean.bernoulli)
    ,min(RES0.4$mean.bernoulli)
    ,min(RES0.5$mean.bernoulli)
    ,min(RES0.6$mean.bernoulli)
    ,min(RES0.7$mean.bernoulli)
    ,min(RES0.8$mean.bernoulli)
  )
  # Plot the data
  # le modèle de survie
  gg1 <-  ggplot(RES0.3.3, aes(n, mean.surv)) +
    geom_line(aes(color = "0.03"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.surv, color = "0.1"), size = 1) +
    geom_line(data = RES0.7.3, aes(n, mean.surv, color = "0.2"), size = 1) +
    geom_line(data = RES0.4, aes(n, mean.surv, color = "0.3"), size = 1) +
    geom_line(data = RES0.5, aes(n, mean.surv, color = "0.4"), size = 1) +
    geom_line(data = RES0.6, aes(n, mean.surv, color = "0.5"), size = 1) +
    geom_line(data = RES0.7, aes(n, mean.surv, color = "0.6"), size = 1) +
    geom_line(data = RES0.8, aes(n, mean.surv, color = "0.7"), size = 1) +
    scale_color_manual(name = "p1", values = c("#0072B2", "red", "#009E73", "#F0E442",
                                               "purple", "#D55E00", "blue1", "#000000")) +
    # scale_colour_colorblind() +
    ylim(borne_min -0.1, borne_max+0.1)+
    labs(
      title = "Modèle de survie",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  # le modele de guerison
  gg2 <-  ggplot(RES0.3.3, aes(n, mean.cure)) +
    geom_line(aes(color = "0.03"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.cure, color = "0.1"), size = 1) +
    geom_line(data = RES0.7.3, aes(n, mean.cure, color = "0.2"), size = 1) +
    geom_line(data = RES0.4, aes(n, mean.cure, color = "0.3"), size = 1) +
    geom_line(data = RES0.5, aes(n, mean.cure, color = "0.4"), size = 1) +
    geom_line(data = RES0.6, aes(n, mean.cure, color = "0.5"), size = 1) +
    geom_line(data = RES0.7, aes(n, mean.cure, color = "0.6"), size = 1) +
    geom_line(data = RES0.8, aes(n, mean.cure, color = "0.7"), size = 1) +
    scale_color_manual(name = "p1", values = c("#0072B2", "red", "#009E73", "#F0E442",
                                               "purple", "#D55E00", "blue1", "#000000")) +
    # scale_colour_colorblind() +
    ylim(borne_min.c -0.1, borne_max.c+0.1)+
    labs(
      title = "Modèle de guérison",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  #le modele de Bernoulli
  gg3 <-  ggplot(RES0.3.3, aes(n, mean.bernoulli)) +
    geom_line(aes(color = "0.03"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.bernoulli, color = "0.1"), size = 1) +
    geom_line(data = RES0.7.3, aes(n, mean.bernoulli, color = "0.2"), size = 1) +
    geom_line(data = RES0.4, aes(n, mean.bernoulli, color = "0.3"), size = 1) +
    geom_line(data = RES0.5, aes(n, mean.bernoulli, color = "0.4"), size = 1) +
    geom_line(data = RES0.6, aes(n, mean.bernoulli, color = "0.5"), size = 1) +
    geom_line(data = RES0.7, aes(n, mean.bernoulli, color = "0.6"), size = 1) +
    geom_line(data = RES0.8, aes(n, mean.bernoulli, color = "0.7"), size = 1) +
    scale_color_manual(name = "p1", values = c("#0072B2", "red", "#009E73", "#F0E442",
                                               "purple", "#D55E00", "blue1", "#000000")) +
    # scale_colour_colorblind() +
    ylim(borne_min.b -0.1, borne_max.b+0.1)+
    labs(
      title = "Modèle de Bernoulli",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  # on remet tout dans un seul graphique
  g <- grid.arrange(gg1, gg2, gg3, top = sprintf("Influence de n et de p1 pour un alpha de type %s", type1)
                    ,bottom = sprintf("généré avec N = %s pour chaque taille n", N),nrow=2)
  return(g)
  
}



fonction_compar_plotsn_lambda_alt_8p(N=100, t_star=6, vect_cause1=c(0.03,0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7),type1="constant",type2="constant", graine=133)

fonction_compar_plotsn_lambda_alt_8p(N=2, t_star=6, vect_cause1=c(0.03,0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7),type1="constant",type2="constant", graine=133)
fonction_compar_plotsn_lambda_alt_8p(N=2, t_star=6, vect_cause1=c(0.03,0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7),type1="increasing",type2="increasing", graine=133)


fonction_ggplot_evol_biais_alt <- function(N,t_star, p,type1,type2,graine=133) {
  library(gridExtra)
  library(ggplot2)
  library(scales)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=p,K=N, t_star=t_star,type1,type2,graine=graine)
  RES0.3.3 <- data.frame(RES)
  colnames(RES0.3.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  borne_min <- min(RES0.3.3$mean.surv, RES0.3.3$mean.cure,RES0.3.3$mean.bernoulli)
  borne_max <- max(RES0.3.3$mean.surv, RES0.3.3$mean.cure,RES0.3.3$mean.bernoulli)
  
  gg1 <- ggplot(data =RES0.3.3, aes(x = n)) +
    geom_smooth(aes(y = mean.cure, col = "modele guerison"), size = 1, alpha = 0.5) +
    geom_smooth(aes(y = mean.bernoulli, col = "modele bernoulli"), size = 1, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values=c("modele guerison"="red1","modele bernoulli"="blue1")) +
    ggtitle("Evolution du biais moyen en \n fonction de la taille d'echantillon") +
    xlab("Taille echantillon") + ylab("Biais moyen") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=10),
          axis.title=element_text(family = "Helvetica", size=12),
          plot.title = element_text(family = "Helvetica", size = 10)) +
    ylim(borne_min, borne_max)
  
  gg2 <- ggplot(data = RES0.3.3, aes(x = n)) +
    geom_smooth(aes(y = mean.cure, col = "modele guerison"), size = 1, alpha = 0.5) +
    geom_smooth(aes(y = mean.surv, col = "modele survie"), size = 1, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values=c("modele guerison"="red1","modele survie"="darkgreen")) +
    ggtitle("Evolution du biais moyen en \n fonction de la taille d'echantillon") +
    xlab("Taille echantillon") + ylab("Biais moyen") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=10),
          axis.title=element_text(family = "Helvetica", size=12),
          plot.title = element_text(family = "Helvetica", size = 10)) +
    ylim(borne_min, borne_max)+
  labs(caption = sprintf("N = %s, n variant de %s a %s \n par pas de %s,type1=%s,type2=%s, p=%s" ,
                         as.character(N),
                         as.character(20),
                         as.character(200),
                         as.character(5),
                         as.character(type1),
                         as.character(type2),
                         as.character(p)))
  
  gg <- grid.arrange(gg1, gg2, ncol = 2, widths = c(8,8))
}
fonction_ggplot_evol_biais_alt(N=300,t_star=6, p=0.3,type1="constant",type2="constant",graine=133)

