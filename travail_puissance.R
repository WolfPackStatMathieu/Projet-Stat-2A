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
id_dose<-donnees$dose
vecteur_reponse<-ifelse(is.na(donnees$toxicity.time)==FALSE,1,0)
fonction_vraisemblance<-function(beta,observations_time,id_dose,valeur_dose,vecteur_reponse){
  res<-1
  for (i in (1:length(observations_time))){
    #on sélectionne la valeur de la dose. On a seulement l'identifiant de la dose
    #dans la base de données.
    dose<-valeur_dose[id_dose[i]]
    non_censure<-vecteur_reponse[i]
    temps<-observations_time[i]
    nouvel_element<-fonction_proba(beta,temps=temps,dose)^(I(non_censure==1)*1)*fonction_survie(beta,temps=temps,dose)^(I(non_censure==0)*1)
    res<-res*nouvel_element
    print(cbind.data.frame(nouvel_element,temps,dose))
  }
  return(res)
}

denom_tox<-function(beta,observations_time,id_dose,valeur_dose,vecteur_reponse){
  result1<-fonction_vraisemblance(beta,observations_time,id_dose,valeur_dose,vecteur_reponse)*dnorm(beta,mean=0,sd=1.34)
  result2<-likelihood_tox_exp(beta,vecteur_reponse,id_dose,valeur_dose,observations_time)
  return(all(round(result2,digits=2)==round(result1,digits=2)))}
num_tox<-function(beta,observations_time,id_dose,valeur_dose,vecteur_reponse){denom_tox(beta,observations_time,id_dose,valeur_dose,vecteur_reponse)*beta}

modele_survie<-function(target,tstar,observations_time,id_dose,valeur_dose,vecteur_reponse){
  #1) calcul de la vraisemblance.
  #On génère (beta) fois une loi normale. 
  #2) calcul de l'estimateur. Methode de l'article : 
  #On a des variables déterministes que sont X et Y. Le beta dépend de ces variables. 
  #La loi de beta sera donc donnée en sachant x et Y.
  #Pour avoir une approximation de cette loi, on utilise la vraisemblance (sachant Beta) * la loi de beta. [On suppose que beta suit une loi normale.]
  #Ce calcul renvoie aux équations 7 et 8. 
  #On calcule l'espérance de la loi de beta sachant X et Y. On doit cependant bien diviser par la constante pour 
  #avoir la loi de beta sachant x et Y. Cette constante renvoie dans notre cas à f(X,Y).
  constante<-integrate(denom_tox,-Inf,Inf,observations_time=observations_time,id_dose=id_dose,vecteur_reponse=vecteur_reponse,valeur_dose=valeur_dose)$value
  beta_hat<-integrate(num_tox,-Inf,Inf,observations_time=observations_time,id_dose=id_dose,vecteur_reponse=vecteur_reponse,valeur_dose=valeur_dose)$value/constante
  
  #3) calcul du nouveau lambda. 
  lambda<-exp(exp(beta_hat)*valeur_dose)
  Proba_inf_t<-1-exp(-lambda*tstar)
  #4) choix de la dose. 
  distance_cible<-abs(Proba_inf_t,target)
  Doses_min<-valeur_dose[which(distance_cible==min(distance_cible))]
  #Soit il n'y a qu'une seule dose disponible soit on en prend une au hasard. 
  dose_choisi<-ifelse(length(Doses_min)==1,Doses_min,sample(Doses_min,1))
  return(beta_hat,dose_choisi)
}
tstar<-6
test_denom<-denom_tox(0.010,observations_time,id_dose,valeur_dose,vecteur_reponse)
#test<-modele_survie(p,tstar,observations_time,id_dose,valeur_dose = valeur_dose,vecteur_reponse = vecteur_reponse )

