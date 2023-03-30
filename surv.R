library(survival)
source("generation_echantillon/generation_echantillon.R")
source("estimateurs/estimateur_cure.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_KM.R")
############################# Premiere modelisation du modele de survie #############
simul_exp<-function(n,lambda){
  ###generer un echantillon de taille n suivant une loi exponentielle de parametre lambda.
 return(rexp(n,lambda))
}



####################################### Deux estimateurs pour un ?chantillon cr???. ####################"
#################################################################################################################################

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


########## calculer l'estimateur du mod?le de survie [POUR EVITER DE le mettre partout.]######
# on isole ici une partie du code de la fonction Simuler_biais_un_n_ech

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
# 
# test_retour<-Simuler_biais_un_n_ech(n=10,lambda=0.5,t_star=6,p=0.5,k=2)
# test_several_times<-Simuler_biais_taillen(n=100,lambda=0.5,t_star=6,p=0.33,k=2,K=10)
# N<-100

################################# On calcule les biais moyens ici. ###############################################################
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
# test_biais_moy<-Calcul_biais_moyen_taillen(n=10,lambda=0.5,t_star=6,p=0.33,k=2,K=10)




