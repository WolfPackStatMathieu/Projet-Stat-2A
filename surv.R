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



####################################### Deux estimateurs pour un échantillon crééé. ####################"
#################################################################################################################################

Simuler_biais_un_n_ech<-function(n,lambda,t_star,p,k){
  database<-Generation_un_ech(n=n,lambda=lambda,t_star=t_star,p=p,k=k)
  estimateur_bern<-fonction_Bern(df=database)
  # estimateur_surv<-fonction_KM(df=database)
  obj_surv<-Surv(as.numeric(df$tox_time),event=database$is_observed)
  obj <- survfit(Surv(as.numeric(df$tox_time),event=df$is_observed) ~1, data = df)
  estim_KM_new <- tp.surv(obj, 6) [3]
  estimateur_surv<- estim_KM_new[[1]]
  
  estimateur_cure<-fonction_cure(df=database,t_star=t_star)
  # on prepare une liste avec les deux estimateurs calcules
  liste_biais<-list(estimateur_bern,estimateur_surv,estimateur_cure)
  names(liste_biais)<-c("Modele_bernoulli","Modele_survie","Modele_guerison")
  return(liste_biais)
}


########## calculer l'estimateur du modèle de survie [POUR EVITER DE le mettre partout.]######
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
  # on crée un dataframe de K lignes de ces deux biais, pour des échantillons de 
  #taille n. 
  df_biases<-as.data.frame(t(cbind.data.frame(sapply(rep(n,K),Simuler_biais_un_n_ech,lambda=lambda,t_star=t_star,p=p,k=k))))
  # on donne un nom aux deux colonnes
  df_biases$Modele_bernoulli<-as.numeric(df_biases$Modele_bernoulli)
  df_biases$Modele_survie<-as.numeric(df_biases$Modele_survie)
  df_biases$Modele_guerison<-as.numeric(df_biases$Modele_guerison)
  return(df_biases)
}

test_retour<-Simuler_biais_un_n_ech(n=10,lambda=0.5,t_star=6,p=0.33,k=1)
test_several_times<-Simuler_biais_taillen(n=100,lambda=0.5,t_star=6,p=0.33,k=2,K=10)
N<-100

################################# On calcule les biais moyens ici. ###############################################################
Calcul_biais_moyen_taillen<-function(K,n,lambda,t_star,p,k){
  # on effectue la simulation des biais pour K échantillons de taille n selon
  # les deux modèles (de guérison, de survie)
  data<-Simuler_biais_taillen(K,n,lambda,t_star,p,k)
  # on va calculer le biais moyen. On prépare donc un vecteur pour stocker les
  # deux biais moyens
  result<-rep(NA,3)
  #on calcule les biais moyens
  result<-colMeans(data)
  # Rappel : biais = estimateur - valeur théorique
  result[1]<-result[1]-p
  result[2]<-result[2]-p
  result[3]<-result[3]-p
  return(result)
}
test_biais_moy<-Calcul_biais_moyen_taillen(n=10,lambda=0.5,t_star=6,p=0.33,k=2,K=10)



tp.surv <- function(obj, times) 
  # obj est un objet de type survfit(Surv_object)
  # times est (a priori) un vecteur de temps
  
  # # premier cas: pas de groupe
  # df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)
  # obj_surv<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  # obj <- survfit(Surv(as.numeric(df$tox_time),event=df$is_observed) ~1, data = df)
  # x <- summary(obj)
  # y <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err))
  
{
  x <- summary(obj) #Returns a list containing the survival curve, confidence 
  # limits for the curve, and other information.
  #Deux traitements: avec groupes selon les covariables (="strata") ou sans.
  # ex: x <- survfit( Surv(futime, fustat)~1, data=ovarian) # pas de groupe
  # x <- survfit( Surv(futime, fustat)~rx, data=ovarian) # deux groupes selon rx
  # x$strata
  if(is.null(x$strata))
  {
    # on recupere les colonnes time, surv, lower, upper et std.err
    y <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err))
    # y <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err))
    # on applique la fonction tpS.surv sur times.
    # tps.surv(y,z) retourne la ligne de y precedent time=z 
    # on le fait pour chaque valeur du vecteur times
    res <- t(sapply(times,function(z,y){tps.surv(y,z)},y=y))
  } else
  {
    res <- NULL
    # x$strata va recuperer le nombre d individus dans chaque groupe
    # table(x$strata) indique combien il y a de groupes de n individus
    # cumsum(table(x$strata)) donne le nombre de groupes de n individus, en cumulant
    # avec c(0,cumsum(table(x$strata))), on fait commencer à 0 le cumsum()
    ld <- c(0,cumsum(table(x$strata)))
    for(i in 1:(length(ld)-1))
    {
      # separation des groupes et applcation de tps.surv pour chaque groupe, avec le vecteur times
      y <- as.data.frame(matrix(cbind(x$time,x$surv,x$lower,x$upper,x$std.err)[(1+ld[i]):ld[i+1],],ncol=5))
      res[[i]] <- t(sapply(times,function(z,y){tps.surv(y,z)},y=y))
    }
  }
  return(res)
}
clep <- function(x,y)
  # prend en entree 
  # x : un temps 
  # y : colonne "time" d un summary(survit(Surv(Genretation_un_ech)))
  # retourne un vecteur de 0 et un seul 1, avec le 1 apparaissant a la derniere
  # ligne ou "time" est plus petit que x (le temps fourni)
  # x <- 3
  # y <- y[,1]
  # a <- which(y[,1]<=x)
  # b <-  rep(0,length(y))
  # b[a[length(a)]] <- 1
{
  a <- which(y<=x) # identifier les lignes où "time" est inferieur au time du x
  # OR les temps sont classés par ordre decroissant
  b <- rep(0,length(y)) # autant de 0 que de y
  b[a[length(a)]] <- 1 # on met 1 à l indice identifie par a[length(a)], qui est
  # la derniere ligne où time est inferieur a x
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

tps.surv <- function(obj, time)  # obj est un objet de type survfit(surv_object))
  # dont on a recupere les colonnes time, surv, lower, upper et std.err
  # time est un temps
  # retourne la ligne precedent time
  
  # time <- 3
  # df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)
  # obj_surv<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  # obj <- survfit(Surv(as.numeric(df$tox_time),event=df$is_observed) ~1, data = df)
  # x <- summary(obj)
  # obj <- as.data.frame(cbind(x$time,x$surv,x$lower,x$upper,x$std.err))
  # y<- rbind(c(0,1,NA,NA,NA),obj)
# names(y) <- c("time","surv","lower","upper","se")
# y$indic <- clep(3,y[,1])
# y <- y[y$indic==1,]
# y <- cbind(time,y) 
# names(y)[1:2] <- c("time","lastev.time")
{
  y <- rbind(c(0,1,NA,NA,NA),obj) # on ajoute en ligne 1 une ligne supplementaire
  # elle correspond à temps = 0 et taux de survie = 100 pourcent
  names(y) <- c("time","surv","lower","upper","se") # on renomme les colonnes
  y$indic <- clep(time,y[,1]) # un vecteur de 0 et un seul 1, avec le 1 
  # apparaissant a la derniere ligne ou "time" est plus petit que x (le temps fourni)
  y <- y[y$indic==1,] # on recupere cette ligne
  y <- cbind(time,y) # on y associe le temps utilise pour obtenir y
  names(y)[1:2] <- c("time","lastev.time") # on renomme la valeur initiale e nlastev.time
  return(y[,1:6]) # on retourne cette ligne
}

