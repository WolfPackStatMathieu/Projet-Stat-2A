################Modèle puissance. #######
set.seed(133)
library(dfcrm)
source("fonctions.R")
help(dfcrm)
vecteur_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
vecteur_reponse<-c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
nom_dose<-c(1,2,3,4,5)
valeur_dose<-c(0.5,1,3,5,6)
valeurs_dose_toxicite<-cbind.data.frame(vecteur_dose,vecteur_reponse)
prior_probabilities<-c(0.05,0.1,0.15,0.33,0.5)
p<-0.33
crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18)
infos<-crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18,model="logistic")
teta<-infos$estimate
plot(x=valeur_dose,y=valeur_dose^(teta))
plot.mtd(infos)

#######Modele de survie logistique.######
###################################
####################

#####I) Simulation des donnÃ©es et import des donnÃ©es simulÃ©es.######
#####On utilise le code fourni pour gÃ©nÃ©rer les donnÃ©es. 
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
#####On Ã©crit une base de donnÃ©es. 
write.table(essai_n18, file="essai_n18.txt", sep="\t", row.names=F)
donnees<-read.table("essai_n18.txt",header=TRUE)

#######II) Creation des arguments de la fonction titecrm. ######

#Vecteur reponse avec reponse=1 si le temps d'apparition de la toxicitÃ© est connu. 
#Le vecteur reponse vaut 0 sinon. 
#Dans notre cas, on donne 0 si le temps d'apparition  de la toxicitÃ© est NA.
vecteur_reponse<-ifelse(is.na(donnees$toxicity.time)==FALSE,1,0)
t<-6
#Les doses administrÃ©es à chaque patient sont donnÃ©es par la colonne dose. 
level_dose<-donnees$dose

#la date de sortie est donnee par la colonne toxicity.study.time. 
observations_time<-ifelse(!is.na(donnees$toxicity.time),donnees$toxicity.time,t)
vecteur_reponse<-ifelse(is.na(donnees$toxicity.time)==FALSE,1,0)
valeur_dose<-c(0.5,1,3,5,6)
id_dose<-donnees$dose
tstar<-6
############## Si on utilise l'inférence bayésienne de l'article. ######
test<-modele_survie_bayes(p,tstar,observations_time,id_dose,valeur_dose = valeur_dose,vecteur_reponse = vecteur_reponse )
windows<-runif(10,-0.1,0)
beta_init<-(-3)
test_beta<-modele_survie_sans_hypotheses(observations_time = observations_time,id_dose=id_dose,vecteur_reponse = vecteur_reponse,valeur_dose = valeur_dose,windows=windows)
test_beta_Newton<-modele_survie_Newton(observations_time = observations_time,id_dose=id_dose,vecteur_reponse = vecteur_reponse,valeur_dose,beta_init =beta_init)$estimate

######On remarque que la valeur de beta selon l'algorithme de Newton peut beaucoup varier. Par ailleurs, la log -vraisemblance a été choisie 
##### car la vraisemblance ne permettait pas d'avoir des itérations. En effet, la valeur du gradient était trop faible
#### au point initial. 
fenetre<-runif(10,-20,-1)
##### Autre méthode, utiliser plusieurs points initiaux. . ####
test_beta_newton_multiple<-modele_survie_Newton_multiple(observations_time = observations_time,id_dose=id_dose,
                                                         valeur_dose = valeur_dose,
                                                         vecteur_reponse = vecteur_reponse,
                                                         fenetre)
### Si on utilise cette méthode, on obtient des résultats très différents du modèle puissance. 
### Ces méthodes ne sont donc pas convenables. 
y_proba<-1-exp(-lambda(beta=test_beta,x=valeur_dose)*tstar)
plot(x=valeur_dose,y=y_proba,type="l")
beta_Newton<-test_beta_Newton
y_proba2<-1-exp(-lambda(beta=beta_Newton,x=valeur_dose)*tstar)
plot(x=valeur_dose,y=y_proba2)

