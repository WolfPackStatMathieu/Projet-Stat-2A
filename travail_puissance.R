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
infos<-crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18,model="logistic")
teta<-infos$estimate
plot(x=valeur_dose,y=valeur_dose^(teta))
plot.mtd(infos)

#######Modele de survie logistique.######
###################################
####################

#####I) Simulation des donn�es et import des donnees simulees.######
#####On utilise le code fourni pour g�n�rer les donn�es. 
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
#####On �crit une base de donn�es. 
write.table(essai_n18, file="essai_n18.txt", sep="\t", row.names=F)
donnees<-read.table("essai_n18.txt",header=TRUE)

#######II) Creation des arguments. ######

#Vecteur reponse avec reponse=1 si le temps d'apparition de la toxicit� est connu. 
#Le vecteur reponse vaut 0 sinon. 
#Dans notre cas, on donne 0 si le temps d'apparition  de la toxicit� est NA.
vecteur_reponse<-ifelse(is.na(donnees$toxicity.time)==FALSE,1,0)
t<-6
#Les doses administr�es � chaque patient sont donn�es par la colonne dose. 
level_dose<-donnees$dose

#la date de sortie est donnee par la colonne toxicity.study.time. 
observations_time<-ifelse(!is.na(donnees$toxicity.time),donnees$toxicity.time,t)

#Rappel: 
#Nous sommes dans le cadre o� la fonction de survie 
#suit une loi exponentielle de paramètre exp(-xi*exp(beta)). Notons epsi cette valeur. 
# donc la fonction de densit� est :
#epsi*exp(-epsi*t) [fonction densit� d'une loi exponentielle.]



#2) calcul de la vraisemblance:
valeur_dose<-c(0.5,1,3,5,6)
id_dose<-donnees$dose
vecteur_reponse<-ifelse(is.na(donnees$toxicity.time)==FALSE,1,0)


tstar<-6
############## Si on utilise l'inference bayesienne de l'article. ######
test<-modele_survie_bayes(p,tstar,observations_time,id_dose,valeur_dose = valeur_dose,vecteur_reponse = vecteur_reponse )
windows<-runif(10,-0.1,0)
beta_init<-(-3)
test_beta<-modele_survie_sans_hypotheses(observations_time = observations_time,id_dose=id_dose,vecteur_reponse = vecteur_reponse,valeur_dose = valeur_dose,windows=windows)
test_beta_Newton<-modele_survie_Newton(observations_time = observations_time,id_dose=id_dose,vecteur_reponse = vecteur_reponse,valeur_dose,beta_init =beta_init)$estimate

######On remarque que la valeur de beta selon l'algorithme de Newton peut beaucoup varier. Par ailleurs, la log -vraisemblance a ete choisie 
##### car la vraisemblance ne permettait pas d'avoir des it�rations. En effet, la valeur du gradient �tait trop faible
#### au point initial. 
fenetre<-runif(10,-20,-1)
##### Autre methode, utiliser plusieurs points initiaux. . ####
test_beta_newton_multiple<-modele_survie_Newton_multiple(observations_time = observations_time,id_dose=id_dose,
                                                         valeur_dose = valeur_dose,
                                                         vecteur_reponse = vecteur_reponse,
                                                         fenetre)
### Si on utilise cette methode, on obtient des resultats tres differents du modele puissance. 
### Ces methodes ne sont donc pas convenables. 
y_proba<-1-exp(-lambda(beta=test_beta,x=valeur_dose)*tstar)
plot(x=valeur_dose,y=y_proba,type="l")
beta_Newton<-test_beta_Newton
y_proba2<-1-exp(-lambda(beta=beta_Newton,x=valeur_dose)*tstar)
plot(x=valeur_dose,y=y_proba2)

