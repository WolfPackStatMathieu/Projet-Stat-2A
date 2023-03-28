########################### Simulation de temps,test.###################
##########################
require(dfcrm)
source("simulation_temps_fonc.R")
source("fonctions.R")
source("MainFunctions_Andrillon_JBS_2020.R")
vecteur_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
vecteur_reponse<-c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
nom_dose<-c(1,2,3,4,5)
valeur_dose<-c(0.5,1,3,5,6)
t_star<-6
valeurs_dose_toxicite<-cbind.data.frame(vecteur_dose,vecteur_reponse)
prior_probabilities<-c(0.05,0.1,0.15,0.33,0.5)
p <- 0.33
crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18)
infos<-crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18,model="logistic")
teta<-infos$estimate
plot(x=valeur_dose,y=valeur_dose^(teta))

##estimate to get beta ####
p<-0.33
a<-3
vecteur_dose_scaled<-infos$dosescaled
beta<-infos$estimate
vecteur_p<-(1+exp(-a-exp(beta)*vecteur_dose_scaled))^(-1)
plot(x=c(1:5),y=vecteur_p,xlab="Niveau de la dose",ylab="Probabilite de toxicité",
     main="Valeur de la toxicité",type="b")
abline(h=p,col="blue")
#############calcul_temps.#######
n<-18
beta<-0.5
k<-3
temps_simul_exp<-simul_temps_exp(n,beta)
ks.test(temps_simul_exp,"pexp")
temps_weibull<-simul_temp_weibull(n,beta,k)
#shape correspond au k alors que scale correspond au lambda dans la prochaine ligne.
ks.test(temps_weibull,"pweibull",shape=k)
temps<-temps_simul_exp
temps<-ifelse(vecteur_reponse==0,6,temps)

#####
p <- 0.33
skeleton <- getprior_exp(halfwidth=0.05, target=p, nu=4, nlevel=5, tstar=6) 
xref <-  log(-log(1-skeleton)/6)              #Set of numerical labels for the doses investigated in the trial
<<<<<<< HEAD
test_bayes<-modele_survie_bayes(p,tstar,observations_time=temps,id_dose=vecteur_dose,valeur_dose =xref,vecteur_reponse = vecteur_reponse )
=======

test_bayes<-modele_survie_bayes(p,tstar,observations_time=temps,id_dose,valeur_dose =xref,vecteur_reponse = vecteur_reponse )
>>>>>>> b7c3a7d463521b102cd96e952867469a7e67f422
afficher_resultat(beta=test_bayes,x_ref=x_ref,probabilites_priori = skeleton)
#####
temps_beta_hasard<-simul_temps_alt(n)
temps_beta_hasard<-ifelse(vecteur_reponse==0,6,temps_beta_hasard)
test_bayes2<-modele_survie_bayes(p,tstar,observations_time=temps_beta_hasard,id_dose,valeur_dose =xref,vecteur_reponse = vecteur_reponse )
afficher_resultat(beta=test_bayes2,x_ref=x_ref,probabilites_priori = skeleton)
