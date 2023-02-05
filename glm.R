####################### GLM ###################
#######################
donnees<-read.table("essai_n18.txt",header=TRUE)
dlt<-ifelse(is.na(donnees$toxicity.study.time)==TRUE,0,1)
skeleton <- getprior_exp(halfwidth=0.05, target=p, nu=4, nlevel=5, tstar=6) 
xref <-  log(-log(1-skeleton)/6)
View(xref)
donnees2<-cbind.data.frame(donnees,dlt,xref[donnees$dose])
View(donnees2)
logit_proba_dlt<-glm(dlt~xref[donnees$dose],data=donnees2,family =binomial(link="logit"))
fonction_logit<-function(beta0=0,beta1,x){
  (1+exp(beta1*x+beta0))
  return(exp(beta1*x+beta0)/(1+exp(beta1*x+beta0)))
}
#beta0<-logit_proba_dlt$coefficients[["(Intercept)"]]
beta1<-logit_proba_dlt$coefficients[["xref[donnees$dose]"]]
y_predicted<-sapply(xref[c(1:5)],fonction_logit,beta0=beta0,beta1=beta1)
plot(x=c(1:5),y=y_predicted,xlab="Index de la dose",ylab="Valeur de la probabilité")
test_bayes<-modele_survie_bayes(p,tstar,observations_time,id_dose,valeur_dose =xref,vecteur_reponse = vecteur_reponse )
afficher_resultat(beta=test_bayes,x_ref,skeleton)
