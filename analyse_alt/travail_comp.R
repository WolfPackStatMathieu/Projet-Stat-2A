################## IMPORT #######

source("generation_echantillon/generation_ech_comp.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_cure.R")
source("estimateurs/estimateur_KM.R")
library(survival)
########### fonction un n_echantillon#####
fonction_estim_comp_once<-function(p_cause1,p_cause2,n,type1,type2,t_star){
  data<-generation_comp(p_cause1 = p_cause1,p_cause2=p_cause2,t_star=t_star,nombre_obs = n,type1=type1,type2=type2)
  indices_non_obs<-which(data$status==0)
  if(length(indices_non_obs)==n){
    sous_liste<-list(rep(1,3))
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
p_cause1<-0.35
p_cause2<-0.40
n<-10
type1<-"decreasing"
type2<-"decreasing"
t_star<-6
test_estim_comp<-fonction_estim_comp_once(p_cause1,p_cause2 = p_cause2,n=n,type1,type2,t_star=t_star)
set.seed(133)
Simuler_estim_mult_times<-function(K,p_cause1,p_cause2,n,type1,type2,t_star){
  result<-as.data.frame(t(cbind(sapply(rep(n,K),fonction_estim_comp_once,p_cause1=p_cause1,p_cause2=p_cause2,type1=type1,type2=type2,t_star=t_star))))
  colnames(result)<-c("Survie","Guerison","Bernoulli")
  return(colMeans(sapply(result,as.numeric)))
}
#test<-Simuler_estim_mult_times(K=10,p_cause1=p_cause1,p_cause2=p_cause2,n=n,type1=type1,type2=type2,t_star=6)
biais.selon.lambda_alt <-function(p_cause1,p_cause2,K, lambda, t_star, k,type1,type2){
  results <- NULL
  n <- 20
  while(n<200){
    vec.biais <- Simuler_estim_mult_times(K=K,p_cause1=p_cause1,p_cause2=p_cause2,n=n,type1=type1,type2=type2,t_star=t_star)
    biais_surv<-vec.biais$Survie-p_cause1-p_cause2
    biais.cure<-vec.biais$Guerison-p_cause1-p_cause2
    biais.km<-vec.biais$Bernoulli-p_cause1-p_cause2
    results<-rbind(results,c(n,biais_surv,biais.cure,biais.km))
    n <- n+20
  }
  return(results)
}
fonction_compar_plotsn_lambda_alt <- function(N, window_lambda, t_star, p_cause1,p_cause2, k) {
  library(gridExtra)
  library(ggplot2)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=p_cause1,p_cause2=p_cause2,K=N, lambda=window_lambda[[1]], t_star=t_star, k,type1,type2)
  RES0.1.3 <- data.frame(RES)
  colnames(RES0.1.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=p_cause1,p_cause2=p_cause2,K=N, lambda=window_lambda[[2]], t_star=t_star, k,type1,type2)
  RES0.2.3 <- data.frame(RES)
  colnames(RES0.2.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1,p_cause2,K, lambda=window_lambda[[3]], t_star, k,type1,type2)
  RES0.5.3 <- data.frame(RES)
  colnames(RES0.5.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  # Get the min and max bounds of each variable to be used in the plots
  borne_min <- min(
    min(RES0.5.3$mean.surv),
    min(RES0.1.3$mean.surv),
    min(RES0.2.3$mean.surv)
  )
  
  borne_max <- max(
    max(RES0.5.3$mean.surv),
    max(RES0.1.3$mean.surv),
    max(RES0.2.3$mean.surv)
  )
  
  borne_min.c <- min(
    min(RES0.5.3$mean.cure),
    min(RES0.1.3$mean.cure),
    min(RES0.2.3$mean.cure)
  )
  
  borne_max.c <- max(
    max(RES0.5.3$mean.cure),
    max(RES0.1.3$mean.cure),
    max(RES0.2.3$mean.cure)
  )
  
  borne_min.b <- min(
    min(RES0.5.3$mean.bernoulli),
    min(RES0.1.3$mean.bernoulli),
    min(RES0.2.3$mean.bernoulli)
  )
  
  borne_max.b <- max(
    max(RES0.5.3$mean.bernoulli),
    max(RES0.1.3$mean.bernoulli),
    max(RES0.2.3$mean.bernoulli)
  )
  
  # Plot the data
  gg1 <-  ggplot(RES0.2.3, aes(n, mean.surv)) +
    geom_line(aes(color = "0.2")) +
    geom_line(data = RES0.5.3, aes(n, mean.surv, color = "0.5")) +
    geom_line(data = RES0.1.3, aes(n, mean.surv, color = "0.1")) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min -0.1, borne_max+0.1)+
    labs(
      title = "Modèle de survie",
      x = "n",
      y = "biais moyen",
      color = "n"+
        theme_minimal())
  
  gg2 <-  ggplot(RES0.2.3, aes(n, mean.cure)) +
    geom_line(aes(color = "0.2")) +
    geom_line(data = RES0.5.3, aes(n, mean.cure, color = "0.5")) +
    geom_line(data = RES0.1.3, aes(n, mean.cure, color = "0.1")) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min.c -0.1, borne_max.c+0.1)+
    labs(
      title = "Modèle de guérison",
      x = "n",
      y = "biais moyen",
      color = "n"+
        theme_minimal())
  
  gg3 <-  ggplot(RES0.2.3, aes(n, mean.bernoulli)) +
    geom_line(aes(color = "0.2")) +
    geom_line(data = RES0.5.3, aes(n, mean.bernoulli, color = "0.5")) +
    geom_line(data = RES0.1.3, aes(n, mean.bernoulli, color = "0.1")) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min.b -0.1, borne_max.b+0.1)+
    labs(
      title = "Modèle de bernoulli",
      x = "n",
      y = "biais moyen",
      color = "n" +
        theme_minimal())
  
  g <- grid.arrange(gg1, gg2, gg3, top = "influence de n et lambda")
  return(g)
  
}
test_alt<-fonction_compar_plotsn_lambda_alt(N=20, window_lambda=c(0.1,0.2,0.5), t_star=6, p_cause1=0.33,p_cause2=0.4, k=1)
