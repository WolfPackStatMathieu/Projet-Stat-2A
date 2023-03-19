source("surv.R")
source("bernoulli.R")
source("weibull.R")


biais.selon.k <-function(K, n, lambda, t_star,p){
  k <- seq(1, 5, by = 0.1)
  results <- NULL
  
  for(i in c(1:length(k))){
      vec.biais <- Simuler_biais_taillen(K, n, lambda, t_star, p, k[i])
      biais.surv <- vec.biais$Modele_survie
      results <- rbind(results, c(k[i], mean(biais.surv) - p))
  }
  return(results)
  
}



fnct_compar_plt_biais.selon.k<-function(N, n, window_lambda,t_star,p){
  #' Plot des valeurs des biais moyens selon la taille des echantillons et du lambda.
  #'
  #' @param N nombre de tailles d'echantillon differents.
  #' @param window_lambda
  #' @param t_star fin de la fenetre d'observation
  #'
  #' @return Plot des valeurs des biais moyens en fonction du lambda et de la taille des echantillons.
  #' @export
  #'
  #' @examples
  #' ######Test ######

  
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N, n, window_lambda[1],t_star,p=p)
  RES0.2.3<-data.frame(RES)
  print(RES0.2.3)
  colnames( RES0.2.3)<- c("k","mean.bias")
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N, n, window_lambda[2],t_star,p=p)
  RES0.5.3<-data.frame(RES)
  colnames( RES0.5.3)<- c("k","mean.bias")
  
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N,n, window_lambda[3],t_star,p=p)
  RES0.1.3<-data.frame(RES)
  colnames( RES0.1.3)<- c("k","mean.bias")
  borne_min<-min(min(RES0.5.3$mean.bias),min(RES0.1.3$mean.bias),min(RES0.2.3$mean.bias))
  borne_max<-max(max(RES0.5.3$mean.bias),max(RES0.1.3$mean.bias),max(RES0.5.3$mean.bias))
  
  plot(RES0.2.3$k,RES0.2.3$mean.bias,title=paste("Influence of k"),
       ylim=c(-0.1+borne_min,borne_max+0.1),
       type='b',xlab="k",ylab="biais moyen")
  title("Influence de k et lambda")
  lines(RES0.5.3$k,RES0.5.3$mean.bias,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.bias,type="b",col="red")
  abline(h=0)
  legend("topright",c("0.1","0.2","0.5"),col=c("red","black","blue"),lty=1)
}


fnct_compar_plt_biais.selon.k(15, 10, c(0.1,0.2, 0.5), 6, 0.33)






















NSimulations.selon.n<-function(N,lambda,t_star){
  results<- NULL
  n<- 20
  while (n<200)
  {
    vecteur_biais<-rep(NA,N)
    biais<-  Simuler_Nfois_n_echantillons(N,n,lambda,t_star)
    results<-as.data.frame(results,c(n,mean(biais) ))
    n<- n+20
  }
  return(results)
}



set.seed(12345)
RES<- NULL
RES<- NSimulations.selon.n(10,0.2,3)
RES0.2.3<-data.frame(RES)
colnames( RES0.2.3)<- c("n","mean.bias")

set.seed(12345)
RES<- NULL
RES<- NSimulations.selon.n(10,0.5,3)
RES0.5.3<-data.frame(RES)
colnames( RES0.5.3)<- c("n","mean.bias")

set.seed(12345)
RES<- NULL
RES<- NSimulations.selon.n(10,0.1,3)
RES0.1.3<-data.frame(RES)
colnames( RES0.1.3)<- c("n","mean.bias")

plot(RES0.2.3$n,RES0.2.3$mean.bias,title=paste("Influence of n"),
     ylim=c(0,0.1),type='b',bty="n",xlab="nbre sujets",ylab="biais moyen")
title("Influence de n et lambda")
lines(RES0.5.3$n,RES0.5.3$mean.bias,type="b",col="blue")
lines(RES0.1.3$n,RES0.1.3$mean.bias,type="b",col="red")
legend("topright",c("0.1","0.2","0.5"),col=c("red","black","blue"),lty=1,bty="n")

