

## utils.r

tp.surv <- function(obj, times)
{
  x <- summary(obj)
  if(is.null(x$strata))
  {
    y <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err))
    res <- t(sapply(times,function(z,y){tps.surv(y,z)},y=y))
  } else
  {
    res <- NULL
    ld <- c(0,cumsum(table(x$strata)))
    for(i in 1:(length(ld)-1))
    {
      y <- as.data.frame(matrix(cbind(x$time,x$surv,x$lower,x$upper,x$std.err)[(1+ld[i]):ld[i+1],],ncol=5))
      res[[i]] <- t(sapply(times,function(z,y){tps.surv(y,z)},y=y))
    }
  }
  return(res)
}
clep <- function(x,y)
{
  a <- which(y<=x)
  b <- rep(0,length(y))
  b[a[length(a)]] <- 1
  return(b)
}
extps.surv <- function(obj, times)
{
  y <- obj
  names(y) <- c("time","surv","lower","upper","se")
  y$indic <- apply(sapply(times,clep,y=y[,1]),1,sum)
  y <- y[y$indic==1,]
  y <- cbind(times,y)
  names(y)[1:2] <- c("time","lastev.time")
  return(y[,1:6])
}

tps.surv <- function(obj, time)
{
  y <- rbind(c(0,1,NA,NA,NA),obj)
  names(y) <- c("time","surv","lower","upper","se")
  y$indic <- clep(time,y[,1])
  y <- y[y$indic==1,]
  y <- cbind(time,y)
  names(y)[1:2] <- c("time","lastev.time")
  return(y[,1:6])
}



#' @return une liste contenant un estimateur de du modele de guerison et un estimateur

source("utils.r")
library(survival)
library(flexsurvcure)

#' du modele de survie
Simuler_biais_un_n_ech<-function(n,lambda,t_star,p,k){
  
  vecteur_censure<-simul_bernoulli(n,p) #simule un n-echantillon de loi de Bernoulli
  #   vecteur_censure <- rbinom(n = n, size = 1, prob = p)
  
  #le 0/1 indique si l'individu est a risuqe de DLT ou si il est gueri
  
  vecteur_temp<-rep(NA,n) # cree un vecteur des temps associ�s
  # l'estimateur du modele de guerison est la moyenne des 1 du vecteur_censure
  estimateur_cure<-mean(vecteur_censure) 
  
  
  # on cree un dataframe qui acolle la DLT au vecteur_temps
  df<-cbind.data.frame(vecteur_censure,vecteur_temp)
  #on les renomme
  colnames(df)<-c("censure","temps")
  #recuperation des numeros de lignes des individus censures
  id_censures<-which(df$censure==1)
  #recuperation des numeros de lignes des individus a risque de DLT
  id_obs<-which(df$censure==0)
  #tous les individu censure se voient attribues comme temps la limite de la 
  #fenetre d observation
  df[id_censures,2] <- t_star+1
  # les autres individus se voient attribuer un temps simule a partir d une 
  # loi de Weibull (qui peut etre une loi exponentielle si k=1)
  df[id_obs,2]<-simul_weibull(length(id_obs),lambda,k)
  # bien sur, si le temps observe est superieur a la fenetre d observation, alors
  # on le remplace par la fin de fenetre d observation
  # df$temps<-ifelse(df$temps<t_star,df$temps,t_star) #TODO � enlever
  # mettre un temps infini (ou sup�rieur � t_star)
  
  # on renomme les colonnes pour une meilleure interpretation
  colnames(df)<-c("isobserved","tox_time")
  # on remplit la colonne isobserved avec des 1 si on observe une toxicite avant
  #la fin de la fenetre d observation, sinon on met des 0
  df$isobserved<-ifelse(df$tox_time<t_star,1,0)
  
  ## jeu : (identifiant DLT, temps survenue DLT)
  
  ### FN DE GENERATION DU JEU DE DONNEES
  
  
  ### DEBUTE DES MODELES d'ANALYSE ############################
  
  ### 10) estimation selon modele de Bernoulli qui ignore les temps d'evenement
  est.bern  <- mean(df$isobserved)
  
  
  ### 1 ) estimation selon modele de survie avec censure non informative : estimation Kaplan Meier
  # cela nous permet d utiliser le package survival et les surv_object
  surv_object<-Surv(df$tox_time,event=df$isobserved)
  fit <- survfit(surv_object ~1, data = df)
  # summary(fit)
  
  est.t                 <- tp.surv(fit,t_star) [3]           # rend estimation de Km au temps t*=6 (fn definie dans utils.r)
  est.km            <- 1-est.t              # calcule estimation de 1-S(t) au temps t selon la methode de Kaplan Meier +++++
  
  
  low.est <- tp.surv(fit,6) [4]          # rend borne inf de IC 95% de Km au temps t*=6
  up.est  <- tp.surv(fit,6) [5]          # rend borne sup de IC 95% de Km au temps t*=6
  low.km.rate <- 1-up.test             # calcule borne inf de IC de 1-S(t) au temps t selon la methode de Kaplan Meier
  up.km.rate  <- 1-low.test            # calcule borne sup de IC de 1-S(t) au temps t selon la methode de Kaplan Meier
  
  
  
  ### 2) estimation selon modele de cure
  # TODO comment est rendu le mod�le pour retrouver le taux de gu�rison?
  cure_model <- flexsurvcure(surv_object ~1, data = df, link="logistic", dist="weibullPH", mixture=T)
  print(cure_model)
  cure.rate <- ???
    
    
    
    bias.bern <- est.bern-p
  bias.km   <-  est.km-p
  bias.cure  <-  cure.rate-p
  
  # biais relatifs
  bias.bern.rel <- bias.bern/p*100
  
  
  # on cherche a recuperer les donnees au temps T=6
  #afin de pouvoir tracer la droite Toxicite =f(dose)
  quantile <-quantile(fit)
  quantile$quantile
  centiles <- quantile(fit, 1:100/100)
  cent <-centiles$quantile
  m<-1
  individu <- cent[m]
  # on touche la proportion de tstar au premier NA
  while (is.na(individu)==FALSE) {
    individu <- cent[m]
    m<- m+1
  }
  transformation <- m-1
  estimateur_survie <- transformation / 100
  # on prepare une liste avec les deux estimateurs calcules
  liste_biais<-list(estimateur_cure,estimateur_survie)
  names(liste_biais)<-c("Modele_guerison","Modele_survie")
  return(liste_biais)
}