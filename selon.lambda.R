source("surv.R")
source("weibull.R")


biais.selon.lambda <-function(K, lambda, t_star,p){
  results <- NULL
  
  n <- 20
  while(n<200){
      vec.biais <- Simuler_biais_taillen(K, n, lambda, t_star, p, k)
      biais.surv <- vec.biais$Modele_survie
      biais.cure <- vec.biais$Modele_guerison
      biais.bernoulli <- vec.biais$Modele_bernoulli
      results <- rbind(results, c(n, mean(biais.surv) - p, mean(biais.cure) -p, mean(biais.bernoulli) -p ))
      n <- n+20
  }
  return(results)
}


fonction_compar_plotsn_lambda<-function(N,window_lambda,t_star,p,k){
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
  #' t_star<-3
  #' N<-10
  #' window_lambda<-c(0.2,0.5,0.1)
  #' result<-fonction_compar_plotsn_lambda(N,window_lambda,t_star)
  
  set.seed(12345)
  RES<- NULL
  RES<- NSimulations.selon.n(N,window_lambda[1],t_star,k=k,p=p)
  RES0.2.3<-data.frame(RES)
  colnames( RES0.2.3)<- c("n","mean.bias")
  set.seed(12345)
  RES<- NULL
  RES<- NSimulations.selon.n(N,window_lambda[2],t_star,k=k,p=p)
  RES0.5.3<-data.frame(RES)
  colnames( RES0.5.3)<- c("n","mean.bias")
  
  set.seed(12345)
  RES<- NULL
  RES<- NSimulations.selon.n(N,window_lambda[3],t_star,k=k,p=p)
  RES0.1.3<-data.frame(RES)
  colnames( RES0.1.3)<- c("n","mean.bias")
  borne_min<-min(min(RES0.5.3$mean.bias),min(RES0.1.3$mean.bias),min(RES0.2.3$mean.bias))
  borne_max<-max(max(RES0.5.3$mean.bias),max(RES0.1.3$mean.bias),max(RES0.5.3$mean.bias))
  plot(RES0.2.3$n,RES0.2.3$mean.bias,title=paste("Influence of n"),
       ylim=c(-0.1+borne_min,borne_max+0.1),
       type='b',bty="n",xlab="nbre sujets",ylab="biais moyen")
  title("Influence de n et lambda")
  lines(RES0.5.3$n,RES0.5.3$mean.bias,type="b",col="blue")
  lines(RES0.1.3$n,RES0.1.3$mean.bias,type="b",col="red")
  abline(h=0)
  legend("topright",c("0.1","0.2","0.5"),col=c("red","black","blue"),lty=1,bty="n")
}


fonction_compar_plotsn_lambda<-function(N, n, window_lambda,t_star,p){
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
  
  colnames( RES0.2.3)<- c("n","mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N, n, window_lambda[2],t_star,p=p)
  RES0.5.3<-data.frame(RES)
  colnames( RES0.5.3)<- c("n","mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N,n, window_lambda[3],t_star,p=p)
  RES0.1.3<-data.frame(RES)
  colnames( RES0.1.3)<- c("n","mean.surv", "mean.cure", "mean.bernoulli")
  
  
  
  borne_min<-min(min(RES0.5.3$mean.surv),min(RES0.1.3$mean.surv),min(RES0.2.3$mean.surv))
  borne_max<-max(max(RES0.5.3$mean.surv),max(RES0.1.3$mean.surv),max(RES0.5.3$mean.surv))
  
  borne_min.c <- min(min(RES0.5.3$mean.cure),min(RES0.1.3$mean.cure),min(RES0.2.3$mean.cure))
  borne_max.c <- max(max(RES0.5.3$mean.cure),max(RES0.1.3$mean.cure),max(RES0.5.3$mean.cure))
  
  borne_min.b <- min(min(RES0.5.3$mean.bernoulli),min(RES0.1.3$mean.bernoulli),min(RES0.2.3$mean.bernoulli))
  borne_max.b <- max(max(RES0.5.3$mean.bernoulli),max(RES0.1.3$mean.bernoulli),max(RES0.5.3$mean.bernoulli))
  
  par(mfrow=c(2,2))
  plot(RES0.2.3$k,RES0.2.3$mean.surv,main="Modèle de survie",
       ylim=c(-0.1+borne_min,borne_max+0.1),
       type='b',xlab="n",ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.surv,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.surv,type="b",col="red")
  abline(h=0)
  
  
  plot(RES0.2.3$k,RES0.2.3$mean.cure, main = "Modèle de guérison",
       ylim=c(-0.1+borne_min.c,borne_max.c+0.1),
       type='b',xlab="n",ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.cure,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.cure,type="b",col="red")
  abline(h=0)
  
  plot(RES0.2.3$k,RES0.2.3$mean.bernoulli,main="Modèle de Bernoulli",
       ylim=c(-0.1+borne_min.b,borne_max.b+0.1),
       type='b',xlab="n",ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.bernoulli,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.bernoulli,type="b",col="red")
  abline(h=0)
  legend("topright",
         c("0.1","0.2","0.5"),
         col=c("red","black","blue"),
         bty="n")
  mtext("Influence de n", side = 3, line = -24, outer = TRUE)
  
}



