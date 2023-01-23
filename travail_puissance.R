library(dfcrm)
help(dfcrm)
vecteur_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
vecteur_reponse<-c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
nom_dose<-c(1,2,3,4,5)
valeur_dose<-c(0.5,1,3,5,6)
valeurs_dose_toxicite<-cbind.data.frame(vecteur_dose,vecteur_reponse)
prior_probabilities<-c(0.05,0.1,0.15,0.33,0.5)
p<-0.33
crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18)
crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18,model="logistic")
teta<-infos$estimate
plot(x=valeur_dose,y=valeur_dose^(teta))
 
plot.mtd(infos)
#######Modèle de survie logistique.######
###################################
####################

#####I) Simulation des données et import des données simulées.######
#####On utilise le code fourni pour générer les données. 
N=18
res <- titesim(PI=c(0.05, 0.1, 0.15, 0.33, 0.50), 
               prior=getprior(0.05, 0.25, 2, 5), 
               0.25, N, 1,
               obswin=6,restrict=1,
               rate=3,
               accrual = "poisson", seed=1234)


base_tox <- data.frame(id=1:N, dose=res$level, time_arrival=res$arrival, toxicity.study.time=res$toxicity.study.time, toxicity.time=res$toxicity.time)
head(base_tox)
base_tox$toxicity.study.time[base_tox$toxicity.study.time==Inf] <- NA
base_tox$toxicity.time[base_tox$toxicity.time==Inf] <- NA
base_tox$toxicity.study.time <- round(base_tox$toxicity.study.time, 2)
base_tox$toxicity.time <- round(base_tox$toxicity.time, 2)
base_tox$time_arrival <- round(base_tox$time_arrival, 2)
essai_n18 <- base_tox
essai_n18
plot(essai_n18)
#####On écrit une base de données. 
write.table(essai_n18, file="essai_n18.txt", sep="\t", row.names=F)
donnees<-read.table("essai_n18.txt",header=TRUE)

#######II) Création des arguments de la fonction titecrm. ######

#Vecteur reponse avec reponse=1 si le temps d'apparition de la toxicité est connu. 
#Le vecteur reponse vaut 0 sinon. 
#Dans notre cas, on donne 0 si le temps d'apparition  de la toxicité est NA.
vecteur_reponse<-ifelse(is.na(donnees$toxicity.time)==FALSE,1,0)
t<-6
#Les doses administrées à chaque patient sont données par la colonne dose. 
level_dose<-donnees$dose

#Le nombre de patient correspond au nombre de lignes. 
nombre_int=nrow(donnees)

#L'argument weights de la fonction titecrm peut renvoyer au poids donné à chaque individu. 
#N'étant pas certains de sa signification à l'heure actuelle, nous donnons un poids uniforme. 
identifiant<-donnees$id

#La date d'arrivée du patient dans l'étude est donnée par la colonne time_arrival.
entree<-donnees$time_arrival

#Import fonctions nécessaires, calcul de la vraisemblance pour la toxicité. 
likelihood_tox_exp <- function(beta, event, dose_level, xref,time){
  res <- 1
  for (i in 1:length(event)){
    res <- res * (( (exp(exp(beta)*xref[dose_level[i]])) * (exp(-exp(xref[dose_level[i]]*exp(beta))*time[i])) )^(I(event[i]==1)*1) * (exp(-exp(xref[dose_level[i]]*exp(beta))*time[i]))^(1-(I(event[i]==1)*1)) )
  }
  return(res)
}

#la date de sortie est donnée par la colonne toxicity.study.time. 
observations_time<-ifelse(!is.na(donnees$toxicity.time),donnees$toxicity.time,t)

#Rappel: 
#Nous sommes dans le cadre où la fonction de survie 
#suit une loi exponentielle de paramètre exp(-xi*exp(beta)). Notons epsi cette valeur. 
# donc la fonction de densité est :
#epsi*exp(-epsi*t) [fonction densité d'une loi exponentielle.]

fonction_proba<-function(beta,temps,dose){
  epsilon<-exp(dose*exp(beta))
  return(epsilon*exp((-1)*(epsilon)*temps))
}

fonction_survie<-function(beta,temps,dose){
  epsilon<-exp(dose*exp(beta))
  return(exp(-epsilon*temps))
}

#2) calcul de la vraisemblance:
valeur_dose<-c(0.5,1,3,5,6)
id_dose<-c(1,2,3,4,5)
fonction_vraisemblance<-function(beta,observations_time,sortie,id_dose,valeur_dose,vecteur_reponse){
  res<-1
  for (j in (1:length(observations_time))){
    #on sélectionne la valeur de la dose. On a seulement l'identifiant de la dose
    #dans la base de données.
    dose<-valeur_dose[id_dose[i]]
    non_censure<-vecteur_reponse[i]
    temps<-observations_time[i]
    res<-res*(fonction_proba(beta,temps=temps,level_dose[i])^(I(non_censure==1))*fonction_survie(beta,temps=temps,dose)^(I(non_censure==0)))
  }
  return(res)
}


modele_survie<-function(p,observations_time,sortie,level_dose,identifiant,vecteur_reponse){
  #1) calcul de la vraisemblance.
  vraisemblance<-fonction_vraisemblance(beta,observations_time,sortie,id_dose,valeur_dose,vecteur_reponse)
  
}

