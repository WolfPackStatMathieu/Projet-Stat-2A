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
n<-10
type1<-"decreasing"
type2<-"decreasing"
t_star<-6
test_estim_comp<-fonction_estim_comp_once(p_cause1,n=n,type1,type2,t_star=t_star,graine=145)
set.seed(133)
Simuler_estim_mult_times<-function(K,p_cause1,n,type1,type2,t_star){
  result<-as.data.frame(t(cbind(sapply(rep(n,K),fonction_estim_comp_once,p_cause1=p_cause1,type1=type1,type2=type2,t_star=t_star))))
  colnames(result)<-c("Survie","Guerison","Bernoulli")
  return(colMeans(sapply(result,as.numeric)))
}
#test<-Simuler_estim_mult_times(K=10,p_cause1=p_cause1,p_cause2=p_cause2,n=n,type1=type1,type2=type2,t_star=6)
biais.selon.lambda_alt <-function(p_cause1,K,t_star, k,type1,type2){
  results <- NULL
  n <- 20
  while(n<200){
    vec.biais <- Simuler_estim_mult_times(K=K,p_cause1=p_cause1,n=n,type1=type1,type2=type2,t_star=t_star)
    biais_surv<-vec.biais[[1]]-p_cause1
    biais.cure<-vec.biais[[2]]-p_cause1
    biais.km<-vec.biais[[3]]-p_cause1
    results<-rbind(results,c(n,biais_surv,biais.cure,biais.km))
    n <- n+20
  }
  return(results)
}


fonction_compar_plotsn_lambda_alt <- function(N,t_star, vect_cause1,k,type1,type2) {
  library(gridExtra)
  library(ggplot2)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[1]],K=N, t_star=t_star, k,type1,type2)
  RES0.3.3 <- data.frame(RES)
  colnames(RES0.3.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[2]],K=N, t_star=t_star, k,type1,type2)
  RES0.5.3 <- data.frame(RES)
  colnames(RES0.5.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[3]],K=N,t_star=t_star, k,type1,type2)
  RES0.7.3 <- data.frame(RES)
  colnames(RES0.7.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  print(RES0.7.3)
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
test_alt<-fonction_compar_plotsn_lambda_alt(N=10, t_star=6, vect_cause1=c(0.33,0.5,0.7), k=1,type1="decreasing",type2="decreasing")

