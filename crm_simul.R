vecteur_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
vecteur_reponse<-c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
nom_dose<-c(1,2,3,4,5)
valeur_dose<-c(0.5,1,3,5,6)
valeurs_dose_toxicite<-cbind.data.frame(vecteur_dose,vecteur_reponse)
prior_probabilities<-c(0.05,0.1,0.15,0.33,0.5)
crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18)
infos<-crm(prior=prior_probabilities,target=p,vecteur_reponse,vecteur_dose,18,model="logistic")
teta<-infos$estimate
plot(x=valeur_dose,y=valeur_dose^(teta))
plot.mtd(infos)
########################### Simulation de temps.###################
##########################





