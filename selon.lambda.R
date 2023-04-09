source("surv.R")
source("weibull.R")


biais.selon.lambda <-function(K, lambda, t_star,p, k){
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





fonction_compar_plotsn_lambda1 <- function(N, window_lambda, t_star, p, k) {
  library(gridExtra)
  library(ggplot2)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.lambda(K=N, lambda=window_lambda[1], t_star=t_star, p = p, k)
  RES0.1.3 <- data.frame(RES)
  colnames(RES0.1.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda(K=N, lambda=window_lambda[2], t_star=t_star, p = p, k)
  RES0.2.3 <- data.frame(RES)
  colnames(RES0.2.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda(K=N, lambda=window_lambda[3], t_star, p = p, k)
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
  
  gg1 <-  ggplot(RES0.2.3, aes(n, mean.bernoulli)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.bernoulli, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.1.3, aes(n, mean.bernoulli, color = "0.1"), size = 0.6) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min.b -0.1, borne_max.b+0.1)+
    labs(
      title = "Modèle de Bernoulli",
      x = "n",
      y = "biais moyen",
      color = "n" )+
    theme_bw()
  gg2 <-  ggplot(RES0.2.3, aes(n, mean.surv)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.surv, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.1.3, aes(n, mean.surv, color = "0.1"), size = 0.6) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min -0.1, borne_max+0.1)+
    labs(
      title = "Modèle de Survie",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  
  gg3 <-  ggplot(RES0.2.3, aes(n, mean.cure)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(n, mean.cure, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.1.3, aes(n, mean.cure, color = "0.1"), size = 0.6) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min.c -0.1, borne_max.c+0.1)+
    labs(
      title = "Modèle de Guérison",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()

  
  g <- grid.arrange(gg1, gg2, gg3, top = "influence de n et lambda" )
  return(g)
  
}

fonction_compar_plotsn_lambda1(1, c(0.1, 0.2, 0.5), 6, 0.33, 1)

?grid.arrange
