library(survival)
#install.packages("roxygen2")
library(roxygen2)
source("bernoulli.R")
source("weibull.R")
#' @examples
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
############################# Premiere mod?lisation du mod?le de survie #############
############################## avec une loi exponentielle et t
simul_exp<-function(n,lambda){
  ###g?n?rer un ?chantillon de taille n suivant une loi exponentielle de param?tre lambda.
 return(rexp(n,lambda))
}






####################################### Deux estimateurs pour un échantillon crééé. ####################"
#################################################################################################################################

Simuler_biais_un_n_ech<-function(n,lambda,t_star,p,k){
  vecteur_censure<-simul_bernoulli(n,p) #simule un n-echantillon de loi de Bernoulli
  #le 0/1 indique si l'individu est a risuqe de DLT ou si il est gueri
  vecteur_temp<-rep(NA,n) # cree un vecteur des temps associés
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
  df[id_censures,2]<-t_star
  # les autres individus se voient attribuer un temps simule a partir d une 
  # loi de Weibull (qui peut etre une loi exponentielle si k=1)
  df[id_obs,2]<-simul_weibull(length(id_obs),lambda,k)
  # bien sur, si le temps observe est superieur a la fenetre d observation, alors
  # on le remplace par la fin de fenetre d observation
  df$temps<-ifelse(df$temps<t_star,df$temps,t_star)
  # on renomme les colonnes pour une meilleure interpretation
  colnames(df)<-c("isobserved","tox_time")
  # on remplit la colonne isobserved avec des 1 si on observe une toxicite avant
  #la fin de la fenetre d observation, sinon on met des 0
  df$isobserved<-ifelse(df$tox_time<t_star,1,0)
  # cela nous permet d utiliser le package survival et les surv_object
  surv_object<-Surv(df$tox_time,event=df$isobserved)
  fit <- survfit(surv_object ~1, data = df)
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


########## calculer l'estimateur du modèle de survie [POUR EVITER DE le mettre partout.]######

Calcul_estim_depuis_df<-function(df,nom_col_obs,nom_coltemps){
  surv_object<-Surv(df[,nom_coltemps],event=df[,nom_col_obs])
  fit <- survfit(surv_object ~1, data = df)
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
  return(estimateur_survie)
}



##################### On simule plusieurs fois les estimateurs.  ici. ##################################################
###########################################################################################

Simuler_biais_taillen<-function(K,n,lambda,t_star,p,k){
  # Simuler_biais_un_n_ech retourne le biais du modele de guerison
  # et le biais du modele de survie
  # on crée un dataframe de K lignes de ces deux biais, pour des échantillons de 
  #taille n. 
  df_biases<-as.data.frame(t(cbind.data.frame(sapply(rep(n,K),Simuler_biais_un_n_ech,lambda=lambda,t_star=t_star,p=p,k=k))))
  df_biases$Modele_guerison<-as.numeric(df_biases$Modele_guerison)
  df_biases$Modele_survie<-as.numeric(df_biases$Modele_survie)
  return(df_biases)
}
test_retour<-Simuler_biais_un_n_ech(n=10,lambda=0.5,t_star=6,0.33,2)
test_several_times<-Simuler_biais_taillen(n=10,lambda=0.5,t_star=6,p=0.33,k=2,K=10)
N<-100

################################# On calcule les biais moyens ici. ###############################################################
Calcul_biais_moyen_taillen<-function(K,n,lambda,t_star,p,k){
  data<-Simuler_biais_taillen(K,n,lambda,t_star,p,k)
  result<-rep(NA,2)
  result<-colMeans(data)
  result[1]<-result[1]-p
  result[2]<-result[2]-p
  return(result)
}
test_biais_moy<-Calcul_biais_moyen_taillen(n=10,lambda=0.5,t_star=6,p=0.33,k=2,K=10)
