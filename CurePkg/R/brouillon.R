source("generation_echantillon/generation_ech_comp.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_cure.R")
source("estimateurs/estimateur_KM.R")
library(survival)
library(scales)

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
    geom_line(data = RES0.5.3, aes(n, mean.surv, color = "0.1"), size = 0.6) +
    geom_line(data = RES0.7.3, aes(n, mean.surv, color = "0.2"), size = 0.6) +
    geom_line(data = RES0.4, aes(n, mean.surv, color = "0.3"), size = 0.6) +
    geom_line(data = RES0.5, aes(n, mean.surv, color = "0.4"), size = 0.6) +
    geom_line(data = RES0.6, aes(n, mean.surv, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.7, aes(n, mean.surv, color = "0.6"), size = 0.6) +
    geom_line(data = RES0.8, aes(n, mean.surv, color = "0.7"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "red")) +
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
    geom_line(data = RES0.5.3, aes(n, mean.cure, color = "0.1"), size = 0.6) +
    geom_line(data = RES0.7.3, aes(n, mean.cure, color = "0.2"), size = 0.6) +
    geom_line(data = RES0.4, aes(n, mean.cure, color = "0.3"), size = 0.6) +
    geom_line(data = RES0.5, aes(n, mean.cure, color = "0.4"), size = 0.6) +
    geom_line(data = RES0.6, aes(n, mean.cure, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.7, aes(n, mean.cure, color = "0.6"), size = 0.6) +
    geom_line(data = RES0.8, aes(n, mean.cure, color = "0.7"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "red")) +
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
    geom_line(data = RES0.5.3, aes(n, mean.bernoulli, color = "0.1"), size = 0.6) +
    geom_line(data = RES0.7.3, aes(n, mean.bernoulli, color = "0.2"), size = 0.6) +
    geom_line(data = RES0.4, aes(n, mean.bernoulli, color = "0.3"), size = 0.6) +
    geom_line(data = RES0.5, aes(n, mean.bernoulli, color = "0.4"), size = 0.6) +
    geom_line(data = RES0.6, aes(n, mean.bernoulli, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.7, aes(n, mean.bernoulli, color = "0.6"), size = 0.6) +
    geom_line(data = RES0.8, aes(n, mean.bernoulli, color = "0.7"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "red")) +
    # scale_colour_colorblind() +
    ylim(borne_min.b -0.1, borne_max.b+0.1)+
    labs(
      title = "Modèle de Bernoulli",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  # on remet tout dans un seul graphique
  g <- grid.arrange(gg1, gg2, gg3, top = "influence de n et de p1")
  return(g)
  
}


test_alt<-fonction_compar_plotsn_lambda_alt_8p(N=100, t_star=6, vect_cause1=c(0.03,0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7),type1="decreasing",type2="decreasing", graine=139)

