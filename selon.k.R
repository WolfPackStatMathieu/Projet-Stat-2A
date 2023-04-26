source("surv.R")
source("weibull.R")


biais.selon.k <-function(K, n, lambda, t_star,p){
  k <- seq(0.8, 5, by = 0.1)
  results <- NULL
  
  for(i in c(1:length(k))){
      vec.biais <- Simuler_biais_taillen(K, n, lambda, t_star, p, k[i])
      biais.surv <- vec.biais$Modele_survie
      biais.cure <- vec.biais$Modele_guerison
      biais.bernoulli <- vec.biais$Modele_bernoulli
      results <- rbind(results, c(k[i], mean(biais.surv) - p, mean(biais.cure) -p, mean(biais.bernoulli) -p ))
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

  colnames( RES0.2.3)<- c("k","mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N, n, window_lambda[2],t_star,p=p)
  RES0.5.3<-data.frame(RES)
  colnames( RES0.5.3)<- c("k","mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N,n, window_lambda[3],t_star,p=p)
  RES0.1.3<-data.frame(RES)
  colnames( RES0.1.3)<- c("k","mean.surv", "mean.cure", "mean.bernoulli")
  
  
  borne_min<-min(min(RES0.5.3$mean.surv),min(RES0.1.3$mean.surv),min(RES0.2.3$mean.surv))
  borne_max<-max(max(RES0.5.3$mean.surv),max(RES0.1.3$mean.surv),max(RES0.5.3$mean.surv))
  
  borne_min.c <- min(min(RES0.5.3$mean.cure),min(RES0.1.3$mean.cure),min(RES0.2.3$mean.cure))
  borne_max.c <- max(max(RES0.5.3$mean.cure),max(RES0.1.3$mean.cure),max(RES0.5.3$mean.cure))
  
  borne_min.b <- min(min(RES0.5.3$mean.bernoulli),min(RES0.1.3$mean.bernoulli),min(RES0.2.3$mean.bernoulli))
  borne_max.b <- max(max(RES0.5.3$mean.bernoulli),max(RES0.1.3$mean.bernoulli),max(RES0.5.3$mean.bernoulli))
  
  par(mfrow=c(2,2))
  plot(RES0.2.3$k,RES0.2.3$mean.surv,main="Modèle de survie",
            ylim=c(-0.1+borne_min,borne_max+0.1),
            type='b',xlab=expression(alpha),ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.surv,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.surv,type="b",col="red")
  abline(h=0)
  

  plot(RES0.2.3$k,RES0.2.3$mean.cure, main = "Modèle de guérison",
            ylim=c(-0.1+borne_min.c,borne_max.c+0.1),
            type='b',xlab=expression(alpha),ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.cure,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.cure,type="b",col="red")
  abline(h=0)

  plot(RES0.2.3$k,RES0.2.3$mean.bernoulli,main="Modèle de Bernoulli",
            ylim=c(-0.1+borne_min.b,borne_max.b+0.1),
            type='b',xlab=expression(alpha),ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.bernoulli,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.bernoulli,type="b",col="red")
  abline(h=0)
  legend("topright",
       c("0.1","0.2","0.5"),
      col=c("red","black","blue"),
      lty=1)
  
  # list_plot[[1]] <- plot_s
  # list_plot[[2]] <- plot_c
  # list_plot[[3]] <- plot_b
  # 
  # return(list_plot)

  mtext("Influence de alpha", side = 3, line = -24, outer = TRUE)

}

### C'est la fonction à utiliser pour le plot

fnct_compar_plt_biais.selon.k1 <- function(N, n, window_lambda, t_star, p) {
  library(gridExtra)
  library(ggplot2)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.k(N, n, window_lambda[1], t_star, p = p)
  RES0.1.3 <- data.frame(RES)
  colnames(RES0.1.3) <- c("k", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.k(N, n, window_lambda[2], t_star, p = p)
  RES0.2.3 <- data.frame(RES)
  colnames(RES0.2.3) <- c("k", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.k(N, n, window_lambda[3], t_star, p = p)
  RES0.5.3 <- data.frame(RES)
  colnames(RES0.5.3) <- c("k", "mean.surv", "mean.cure", "mean.bernoulli")
  
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
  
gg1 <-  ggplot(RES0.2.3, aes(k, mean.bernoulli)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(k, mean.bernoulli, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.1.3, aes(k, mean.bernoulli, color = "0.1"), size = 0.6) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min.b -0.1, borne_max.b+0.1)+
    labs(
      title = "Modèle de Bernoulli",
      x = expression(alpha),
      y = "biais moyen",
      color = expression(alpha))+
    theme_bw()


gg2 <-  ggplot(RES0.2.3, aes(k, mean.surv)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.5.3, aes(k, mean.surv, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.1.3, aes(k, mean.surv, color = "0.1"), size = 0.6) +
    scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
    ylim(borne_min -0.1, borne_max+0.1)+
    labs(
      title = "Modèle de Survie",
      x = expression(alpha),
      y = "biais moyen",
      color = expression(alpha))+
    theme_bw()

gg3 <-  ggplot(RES0.2.3, aes(k, mean.cure)) +
  geom_line(aes(color = "0.2"), size = 0.6) +
  geom_line(data = RES0.5.3, aes(k, mean.cure, color = "0.5"), size = 0.6) +
  geom_line(data = RES0.1.3, aes(k, mean.cure, color = "0.1"), size = 0.6) +
  scale_color_manual(name = expression(lambda), values = c("red", "black", "blue")) +
  ylim(borne_min.c -0.1, borne_max.c+0.1)+
  labs(
    title = "Modèle de Guérison",
    x = expression(alpha),
    y = "biais moyen",
    color = expression(alpha))+
  theme_bw()
g <- grid.arrange(gg1, gg2, gg3, top = paste("influence de", expression(alpha)))

return(g)

}

fnct_compar_plt_biais.selon.k1(N=100, 20, c(0.1, 0.2, 0.5), 6, 0.33)


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
  
  colnames( RES0.2.3)<- c("k","mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N, n, window_lambda[2],t_star,p=p)
  RES0.5.3<-data.frame(RES)
  colnames( RES0.5.3)<- c("k","mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES<- NULL
  RES<- biais.selon.k(N,n, window_lambda[3],t_star,p=p)
  RES0.1.3<-data.frame(RES)
  colnames( RES0.1.3)<- c("k","mean.surv", "mean.cure", "mean.bernoulli")
  
  
  borne_min<-min(min(RES0.5.3$mean.surv),min(RES0.1.3$mean.surv),min(RES0.2.3$mean.surv))
  borne_max<-max(max(RES0.5.3$mean.surv),max(RES0.1.3$mean.surv),max(RES0.5.3$mean.surv))
  
  borne_min.c <- min(min(RES0.5.3$mean.cure),min(RES0.1.3$mean.cure),min(RES0.2.3$mean.cure))
  borne_max.c <- max(max(RES0.5.3$mean.cure),max(RES0.1.3$mean.cure),max(RES0.5.3$mean.cure))
  
  borne_min.b <- min(min(RES0.5.3$mean.bernoulli),min(RES0.1.3$mean.bernoulli),min(RES0.2.3$mean.bernoulli))
  borne_max.b <- max(max(RES0.5.3$mean.bernoulli),max(RES0.1.3$mean.bernoulli),max(RES0.5.3$mean.bernoulli))
  
  par(mfrow=c(2,2))
  plot(RES0.2.3$k,RES0.2.3$mean.surv,main="Modèle de survie",
       ylim=c(-0.1+borne_min,borne_max+0.1),
       type='b',xlab=expression(alpha),ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.surv,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.surv,type="b",col="red")
  abline(h=0)
  
  
  plot(RES0.2.3$k,RES0.2.3$mean.cure, main = "Modèle de guérison",
       ylim=c(-0.1+borne_min.c,borne_max.c+0.1),
       type='b',xlab=expression(alpha),ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.cure,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.cure,type="b",col="red")
  abline(h=0)
  
  plot(RES0.2.3$k,RES0.2.3$mean.bernoulli,main="Modèle de Bernoulli",
       ylim=c(-0.1+borne_min.b,borne_max.b+0.1),
       type='b',xlab=expression(alpha),ylab="biais moyen")
  lines(RES0.5.3$k,RES0.5.3$mean.bernoulli,type="b",col="blue")
  lines(RES0.1.3$k,RES0.1.3$mean.bernoulli,type="b",col="red")
  abline(h=0)
  legend("topright",
         c("0.1","0.2","0.5"),
         col=c("red","black","blue"),
         lty=1)
  plot.new()
  legend("topright",
         c("0.1","0.2","0.5"),
         col=c("red","black","blue"),
         lty=1)
  mtext("Influence de alpha", side = 3, line = -24, outer = TRUE)
}




fnct_compar_plt_biais.selon.k1(N=1, 20, c(0.1, 0.2, 0.5), 6, 0.33)
fonction_compar_plotsn_lambda1(N=10, c(0.1, 0.2, 0.5), 6, 0.33, 1)



