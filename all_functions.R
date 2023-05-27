#Fichiers à mettre : utils et la méthode de génération cas risques compétitifs (dire qu'elle 
#a été un peu modifiée)#

####################Scenario 1###############
  
  ##########Première méthode.######
  ##############Generation des données##################
###############Simulation_temps###############
simul_weibull<-function(n,lambda,k){
  return(rweibull(n,shape=k,scale=1/lambda))
}

Generation_un_ech<-function(n,lambda,t_star,p,k){
  vecteur_censure<-rbinom(n,1,p)
  vecteur_temp<-rep(NA,n) # cree un vecteur des temps associ?s
  # l'estimateur du modele de Bernoulli est la moyenne des 1 du vecteur_censure
  # on cree un dataframe qui acolle la DLT au vecteur_temps
  df<-cbind.data.frame(vecteur_censure,vecteur_temp)
  #on les renomme
  colnames(df)<-c("sensible","temps")
  #recuperation des numeros de lignes des individus censures
  id_non_sensibles<-which(df$sensible==0)
  #recuperation des numeros de lignes des individus a risque de DLT
  id_sensibles<-which(df$sensible==1)
  #tous les individu censure se voient attribues comme temps la limite de la 
  #fenetre d observation
  df[id_non_sensibles,2]<-t_star+1
  # les autres individus se voient attribuer un temps simule a partir d une 
  # loi de Weibull (qui peut etre une loi exponentielle si k=1)
  df[id_sensibles,2]<-simul_weibull(length(id_sensibles),lambda,k)
  # bien sur, si le temps observe est superieur a la fenetre d observation, alors
  # on le remplace par la fin de fenetre d observation
  # on renomme les colonnes pour une meilleure interpretation
  # on remplit la colonne isobserved avec des 1 si on observe une toxicite avant
  #la fin de la fenetre d observation, sinon on met des 0
  colnames(df)<-c("sensible","tox_time")
  df$is_observed<-ifelse(df$tox_time<t_star,1,0)
  return(df)
}
calcule_prop_censure<-function(N, n, lambda, t_star, p, k){
  #initialisation du dataframe
  result_censures<- as.data.frame(matrix(NA, nrow = length(N), 3))
  colnames(result_censures)<-c("prop_censure_totale", "prop_censure_gueris", "prop_censure_additionnelle")
  j<-1 #compteur de ligne
  for (i in 1:N){
    ech <-Generation_un_ech(n,lambda,t_star,p,k) #genere un echantillon
    #pourcentage de censure totale dans l echantillon
    result_censures[j,"prop_censure_totale"] <-as.numeric(table(ech$is_observed == 0)["TRUE"])/n 
    # censure due au pourcentage de gueris, au debut de la generation de l echantillon
    result_censures[j,"prop_censure_gueris"] <- as.numeric(table(ech$sensible == 0)["TRUE"])/n
    # calcul de la censure additionnelle= censure_totale - censure_gueris
    result_censures[j,"prop_censure_additionnelle"]<-result_censures[j,"prop_censure_totale"] - result_censures[j,"prop_censure_gueris"] 
    
    j<- j+1
  }
  
  # boxplot_plot_censures<-ggplot(result_censures) +
  violin_plot <- result_censures %>% 
    gather(key="Type_de_censure", value="Val") %>%
    ggplot( aes(x=Type_de_censure, y=Val, fill=Type_de_censure) ) +
    geom_violin() +
    ggtitle("Distribution des types de censure, modele de generation 1")+
    ylab("Pourcentage de censure") + xlab("Type de censure")+ 
    theme(axis.text=element_text(family = "Helvetica", size=18),
          axis.title=element_text(family = "Helvetica", size=18),
          plot.title = element_text(family = "Helvetica", size = 25)) +
    labs(caption = sprintf("N = %s, n = %s, lambda= %s,t_star= %s, p= %s, alpha= %s", as.character(N),as.character(n), as.character(lambda), as.character(t_star),as.character(p), as.character(k)))
  print(violin_plot)
  return(result_censures)
}

###############Estimateurs.##############
fonction_KM<-function(df,t_star){
  df<-df[,c("tox_time","is_observed")]
  indice_cens<-which(df$is_observed==0)
  if(length(indice_cens)==0){return(1)}
  surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  fit <- survfit(surv_object ~1, data = df)
  estimateur_survie<-1-tp.surv(fit,6)[3][[1]]
  return(estimateur_survie)
}
fonction_Bern<-function(df){
  return(mean(df$is_observed))
}
fonction_cure<-function(df,t_star){
  # retourne la probabilite de ne pas avoir fait de DLT a T_star
  indice_observed<-which(df$is_observed==1)
  indice_censored<-which(df$is_observed==0)
  df$covar<-rep(1,nrow(df))
  df2<-df[,c("covar","is_observed","tox_time")]
  #hb <- probcurehboot(covar, tox_time, is_observed, df2, x0 = 1, bootpars = controlpars(B =
  #                                                                   1999, hsmooth = 3))
  #prob_gueri<-probcure(x=covar,t=tox_time,dataset = df2,d=is_observed,x0=1, h =c(1,1.5,2),local=FALSE)
  #estimateur_tox<-1-prob_gueri[["q"]]$h1
  #result<-flexsurvcure(Surv(tox_time,event=is_observed)~1,data=df,
  #link="logistic", dist="weibullPH", mixture=T)
  ### the theta in the print is the real value of "theta", 
  ### the theta in the result is the qlogis of the first one. 
  ## plogis of the second one to get the real value back. 
  #Prob_cure<-plogis(coef(result)[1])
  #estimateur_tox<-1-Prob_cure
  #df2<-data.frame(df2)
  #pd <- smcure(Surv(tox_time,is_observed)~1+covar2,offset=NULL,
  #cureform=~1+covar2,data=df2,model="ph",
  #  Var = TRUE)
  #print("test2")
  #estimateur_tox<-1-plogis(coef(pd))
  require(cuRe)
  summary(df2)
  fit.wei <- fit.cure.model(Surv(tox_time,is_observed) ~ 1, formula.surv=list(~1),data =df2,
                            dist="weibull",link="logit")
  prob_cure<-plogis(as.numeric(fit.wei$coefs[1]))
  estimateur_tox<-1-prob_cure
  return(estimateur_tox)
}
#################Simulation en plusieurs fois.##########
Simuler_biais_un_n_ech<-function(n,lambda,t_star,p,k){
  database<-Generation_un_ech(n=n,lambda=lambda,t_star=t_star,p=p,k=k)
  estimateur_bern<-fonction_Bern(df=database)
  estimateur_surv<-fonction_KM(df=database)
  estimateur_cure<-fonction_cure(df=database,t_star=t_star)
  # on prepare une liste avec les deux estimateurs calcules
  liste_biais<-list(estimateur_bern,estimateur_surv,estimateur_cure)
  names(liste_biais)<-c("Modele_bernoulli","Modele_survie","Modele_guerison")
  return(liste_biais)
}
Simuler_biais_taillen<-function(K,n,lambda,t_star,p,k){
  # Simuler_biais_un_n_ech retourne le biais du modele de guerison
  # et le biais du modele de survie
  # on cr?e un dataframe de K lignes de ces deux biais, pour des ?chantillons de 
  #taille n. 
  df_biases<-as.data.frame(t(cbind.data.frame(sapply(rep(n,K),Simuler_biais_un_n_ech,lambda=lambda,t_star=t_star,p=p,k=k))))
  # on donne un nom aux deux colonnes
  df_biases$Modele_bernoulli<-as.numeric(df_biases$Modele_bernoulli)
  df_biases$Modele_survie<-as.numeric(df_biases$Modele_survie)
  df_biases$Modele_guerison<-as.numeric(df_biases$Modele_guerison)
  return(df_biases)
}

#############Biais##############
########influence des params.#####
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
  plot.new()
  legend("topright",
         c("0.1","0.2","0.5"),
         col=c("red","black","blue"),
         lty=1)
  mtext("Influence de alpha", side = 3, line = -24, outer = TRUE)
}
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

########## en fixant les paramètres.
Calcul_biais_moyen_taillen<-function(K,n,lambda,t_star,p,k){
  # on effectue la simulation des biais pour K ?chantillons de taille n selon
  # les deux mod?les (de gu?rison, de survie)
  data<-Simuler_biais_taillen(K,n,lambda,t_star,p,k)
  # on va calculer le biais moyen. On pr?pare donc un vecteur pour stocker les
  # deux biais moyens
  result<-rep(NA,3)
  #on calcule les biais moyens
  result<-colMeans(data)
  # Rappel : biais = estimateur - valeur th?orique
  result[1]<-result[1]-p
  result[2]<-result[2]-p
  result[3]<-result[3]-p
  return(result)
}
###########Evolution du biais#############


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

######### EQM ################
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

##############Deuxième méthode.#############
########## Génération des données. ############
generation_comp<-function(p_cause1,p_cause2,t_star,nombre_obs,graine,type1,type2){
  alpha1<-get_alpha(p_cause1,obswin=t_star,typ="weibull",typ_wb=type1)
  alpha2<-get_alpha(p_cause2,obswin=t_star,typ="weibull",typ_wb=type2)
  liste_dataset<-get_dataset0(n=nombre_obs,alpha1,alpha2,tstar=t_star,graine=graine,K=1,type="weibull")
  data<-liste_dataset$data_complete
  data<-as.data.frame(data)
  data$is_observed<-ifelse(data$status==0,0,1)
  data_estim<-data[,c("status","time","is_observed")]
  colnames(data_estim)<-c("status","tox_time","is_observed")
  return(data_estim)
}
######### Estimateurs.#####
#### Rien#####

######### Biais##########
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
    ggtitle("Evolution du biais moyen en \n fonction de n") +
    xlab("Taille echantillon") + ylab("Biais moyen") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=20),
          axis.title=element_text(family = "Helvetica", size=20),
          plot.title = element_text(family = "Helvetica", size = 24)
          , legend.text = element_text(family = "Helvetica", size = 20)
          ,text = element_text(size=rel(20))) +
    ylim(borne_min, borne_max)
  
  gg2 <- ggplot(data = RES0.3.3, aes(x = n)) +
    geom_smooth(aes(y = mean.cure, col = "modele guerison"), size = 1, alpha = 0.5) +
    geom_smooth(aes(y = mean.surv, col = "modele survie"), size = 1, alpha = 0.5) +
    scale_color_manual(name = "Modeles", values=c("modele guerison"="red1","modele survie"="darkgreen")) +
    ggtitle("Evolution du biais moyen en \n fonction de n") +
    xlab("Taille echantillon") + ylab("Biais moyen") +
    theme_classic() +
    theme(legend.title=element_blank(),
          axis.text=element_text(family = "Helvetica", size=20),
          axis.title=element_text(family = "Helvetica", size=20),
          plot.title = element_text(family = "Helvetica", size = 24)
          , legend.text = element_text(family = "Helvetica", size = 20)
          ,text = element_text(size=rel(20))) +
    ylim(borne_min, borne_max)+
    labs(caption = sprintf("N = %s, n variant de %s a %s \n par pas de %s,type1=%s,type2=%s, p=%s" ,
                           as.character(N),
                           as.character(20),
                           as.character(100),
                           as.character(5),
                           as.character(type1),
                           as.character(type2),
                           as.character(p)))
  
  gg <- grid.arrange(gg1, gg2, ncol = 2, widths = c(8,8))
}

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

### Utilisation des paramètres ####
fonction_compar_plotsn_lambda_alt_8p <- function(N,t_star, vect_cause1=c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8),type1,type2,graine=133) {
  library(gridExtra)
  library(ggplot2)
  library(scales)
  # Generate the data
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[1]],K=N, t_star=t_star,type1,type2,graine=graine)
  RES0.2 <- data.frame(RES)
  colnames(RES0.2) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[2]],K=N, t_star=t_star,type1,type2,graine=graine)
  RES0.3 <- data.frame(RES)
  colnames(RES0.3) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[3]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.4 <- data.frame(RES)
  colnames(RES0.4) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[4]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.5 <- data.frame(RES)
  colnames(RES0.5) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[5]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.6 <- data.frame(RES)
  colnames(RES0.6) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[6]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.7 <- data.frame(RES)
  colnames(RES0.7) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  
  set.seed(12345)
  RES <- biais.selon.lambda_alt(p_cause1=vect_cause1[[7]],K=N,t_star=t_star,type1,type2,graine=graine)
  RES0.8 <- data.frame(RES)
  colnames(RES0.8) <- c("n", "mean.surv", "mean.cure", "mean.bernoulli")
  # Get the min and max bounds of each variable to be used in the plots
  # les bornes min et max du modele de survie
  borne_min <- min(
    min(RES0.2$mean.surv),
    min(RES0.4$mean.surv),
    min(RES0.5$mean.surv)
    ,min(RES0.6$mean.surv)
    ,min(RES0.7$mean.surv)
    ,min(RES0.8$mean.surv)
    ,min(RES0.7$mean.surv)
  )
  
  borne_max <- max(
    max(RES0.2$mean.surv),
    max(RES0.3$mean.surv),
    max(RES0.4$mean.surv)
    ,max(RES0.5$mean.surv)
    ,max(RES0.6$mean.surv)
    ,max(RES0.7$mean.surv)
    ,max(RES0.8$mean.surv)
  )
  # les bornes min et max du modele de guerison
  borne_min.c <- min(
    min(RES0.2$mean.cure),
    min(RES0.3$mean.cure),
    min(RES0.4$mean.cure)
    ,min(RES0.5$mean.cure)
    ,min(RES0.6$mean.cure)
    ,min(RES0.7$mean.cure)
    ,min(RES0.8$mean.cure)
  )
  
  borne_max.c <- max(
    max(RES0.2$mean.cure),
    max(RES0.3$mean.cure),
    max(RES0.4$mean.cure)
    ,max(RES0.5$mean.cure)
    ,max(RES0.6$mean.cure)
    ,max(RES0.7$mean.cure)
    ,max(RES0.8$mean.cure)
  )
  
  # les bornes min et max du modele de Bernoulli
  borne_min.b <- min(
    min(RES0.2$mean.bernoulli),
    min(RES0.3$mean.bernoulli),
    min(RES0.4$mean.bernoulli)
    ,min(RES0.5$mean.bernoulli)
    ,min(RES0.6$mean.bernoulli)
    ,min(RES0.7$mean.bernoulli)
    ,min(RES0.8$mean.bernoulli)
  )
  
  borne_max.b <- max(
    max(RES0.2$mean.bernoulli),
    max(RES0.3$mean.bernoulli),
    max(RES0.4$mean.bernoulli)
    ,max(RES0.5$mean.bernoulli)
    ,max(RES0.6$mean.bernoulli)
    ,max(RES0.7$mean.bernoulli)
    ,max(RES0.8$mean.bernoulli)
  )
  # Plot the data
  # le modele de survie
  gg1 <-  ggplot(RES0.2, aes(n, mean.surv)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.3, aes(n, mean.surv, color = "0.3"), size = 0.6) +
    geom_line(data = RES0.4, aes(n, mean.surv, color = "0.4"), size = 0.6) +
    geom_line(data = RES0.5, aes(n, mean.surv, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.6, aes(n, mean.surv, color = "0.6"), size = 0.6)+
    geom_line(data = RES0.7, aes(n, mean.surv, color = "0.7"), size = 0.6) +
    geom_line(data = RES0.8, aes(n, mean.surv, color = "0.8"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("#0072B2", "red", "#009E73", "#F0E442",
                                               "purple", "#D55E00","blue")) +
    # scale_colour_colorblind() +
    ylim(borne_min -0.04, borne_max+0.04)+
    labs(
      title = "Modele de survie",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  # le modele de guerison
  gg2 <-  ggplot(RES0.2, aes(n, mean.cure)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.3, aes(n, mean.cure, color = "0.3"), size = 0.6)+
    geom_line(data = RES0.4, aes(n, mean.cure, color = "0.4"), size = 0.6) +
    geom_line(data = RES0.5, aes(n, mean.cure, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.6, aes(n, mean.cure, color = "0.6"), size = 0.6) +
    geom_line(data = RES0.7, aes(n, mean.cure, color = "0.7"), size = 0.6) +
    geom_line(data = RES0.8, aes(n, mean.cure, color = "0.8"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("#0072B2", "red", "#009E73", "#F0E442",
                                               "purple", "#D55E00","blue")) +
    # scale_colour_colorblind() +
    ylim(borne_min.c -0.04, borne_max.c+0.04)+
    labs(
      title = "Modele de guerison",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  #le modele de Bernoulli
  gg3 <-  ggplot(RES0.2, aes(n, mean.bernoulli)) +
    geom_line(aes(color = "0.2"), size = 0.6) +
    geom_line(data = RES0.3, aes(n, mean.bernoulli, color = "0.3"), size = 0.6) +
    geom_line(data = RES0.4, aes(n, mean.bernoulli, color = "0.4"), size = 0.6) +
    geom_line(data = RES0.5, aes(n, mean.bernoulli, color = "0.5"), size = 0.6) +
    geom_line(data = RES0.6, aes(n, mean.bernoulli, color = "0.6"), size = 0.6) +
    geom_line(data = RES0.7, aes(n, mean.bernoulli, color = "0.7"), size = 0.6) +
    geom_line(data = RES0.8, aes(n, mean.bernoulli, color = "0.8"), size = 0.6) +
    scale_color_manual(name = "p1", values = c("#0072B2", "red", "#009E73", "#F0E442",
                                               "purple", "#D55E00","blue")) +
    # scale_colour_colorblind() +
    ylim(borne_min.b -0.04, borne_max.b+0.04)+
    labs(
      title = "Modele de Bernoulli",
      x = "n",
      y = "biais moyen",
      color = "n")+
    theme_bw()
  # on remet tout dans un seul graphique
  g <- grid.arrange(gg1, gg2, gg3, top = sprintf("Influence de n et de p1 pour un alpha de type %s", type1)
                    ,bottom = sprintf("genere avec N = %s pour chaque taille n", N),nrow=2)
  return(g)
  
}




### Evolution ####

######### EQM ###########

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