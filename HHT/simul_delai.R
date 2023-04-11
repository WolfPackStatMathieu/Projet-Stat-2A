source("weibull.R")
source("surv.R")
simul_tps_hht <- function(modele, t_star,probabilite_a_priori){
  if(modele=="constant"){
    alpha<-1
  }
  if(modele=="increasing"){
    alpha<-3
  }
  if(modele=="decreasing"){
    alpha<-0.1
  }
  vec_res <- c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
  vec_tps <- rep(NA, length(vec_res))
  vec_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
  
  for (i in 1:length(vec_res)){
    if (vec_res[i] == 0){
      probabilite<-probabilite_a_priori[vec_dose[i]]
      lambda_dosek<-fonction_find_lambda(probabilite_priori=probabilite,t_star,alpha=alpha)
      tps_simul <- temps_simul1(1/lambda_dosek, alpha)
      #on a cette fois l'autre paramétrisation. 
      #on doit donc utiliser 1/lambda car dans weibull, la probabilité est selon la première paramétrisation. 
      nb_simul<-1
      reponse<-TRUE
      while(tps_simul<t_star){
        tps_simul<-temps_simul1(1/lambda_dosek,alpha)
        nb_simul<-nb_simul+1
        if (nb_simul>10){
          reponse<-FALSE
          break
        }
      }
      if(tps_simul<t_star){tps_simul<-7}
      vec_tps[i]<-tps_simul
    }
    else{
      tps_simul <-temps_simul2(1/lambda_dosek, alpha,t_star=t_star)
      while(tps_simul>t_star){
        tps_simul<-temps_simul2(1/lambda_dosek,alpha,t_star=t_star)
      }
      vec_tps[i]<-tps_simul
    }
  }
  data_HHT<-cbind.data.frame(vec_dose,vec_res,vec_tps)
  colnames(data_HHT)<-c("Dose","observe","temps")
  return(data_HHT)
}
#> 1-exp(-(1/ 14.98211)*6)
#1] 0.33
#
####??? fonction pour trouver lambda dans la paramétrisation (1/lambda)
fonction_find_lambda<-function(probabilite_priori,t_star,alpha){
  calcul_intermediaire<-(1/alpha)*log(-t_star^(alpha)/(log(1-probabilite_priori)))
  return(exp(calcul_intermediaire))
}
temps_simul1<-function(lambda,alpha){
  tps_simul<-simul_weibull(1,lambda,alpha)
  return(ifelse(is.na(tps_simul),0,tps_simul))
}
temps_simul2<-function(lambda,alpha,t_star){
  tps_simul<-simul_weibull(1,lambda,alpha)
  return(ifelse(is.na(tps_simul),t_star+1,tps_simul))
}
test<-simul_tps_hht("constant",t_star=6,probabilite_a_priori =c(0.05,0.1,0.15,0.33,0.5))
######### calcul des estimateurs.#####
library(survival)
fonction_estim_hht<-function(modele,t_star,target){
  require(dfcrm)
  probabilite_a_priori<-c(0.05,0.1,0.15,0.33,0.5)
  nb_doses<-length(probabilite_a_priori)
  donnees<-simul_tps_hht(modele,t_star,probabilite_a_priori)
  print(donnees)
  data_returns<-as.data.frame(matrix(NA,nb_doses,4))
  colnames(data_returns)<-c("estimateur_bernoulli","estimateur_survie","estimateur_guerison","p")
  vecteur<-c(1:nb_doses)
  fonction_MEAN<-function(vecteur,donnees){
    indice_admin_dose<-which(donnees$Dose==vecteur)
    if(length(indice_admin_dose)!=0){
    return(mean(donnees[indice_admin_dose,"observe"]))}
    else{
      return(0)
    }
  }
  dose_scaled<-crm(prior =probabilite_a_priori,target = target, tox = donnees$observe, level = donnees$Dose, n =nrow(donnees),model="logistic")$dosescaled
  estimateur_bern<-sapply(vecteur,fonction_MEAN,donnees=donnees)
  data_returns[,"estimateur_bernoulli"]<-estimateur_bern
  dose_all_missed<-fonction_miss(donnees,nb_doses=length(dose_scaled))
  donnees_tronq<-donnees[-which(donnees$Dose %in% dose_all_missed),]
  donnees_tronq$factdose<-as.factor(dose_scaled[donnees_tronq$Dose])
  fonction_surv<-Surv(as.numeric(donnees_tronq$temps),event=donnees_tronq$observe)
  fit_surv <- survfit(fonction_surv ~factdose, data = donnees_tronq)
  fit_cure<-flexsurvcure(Surv(temps,event=observe)~factdose, data = donnees_tronq, link="logistic", dist="weibullPH", mixture=T)
  Predicted_survival_prob<-summary(fit_cure, t=t_star, type="survival", tidy=T)
  colnames(Predicted_survival_prob)<-c("time","est","lcl","ucl","categorie")
  estimation_cure<-rep(NA,nb_doses)
  estimation_surv<-rep(NA,nb_doses)
  rang_dose<-1
  for (j in c(1:nb_doses)){
    if (j %in% dose_all_missed || !(j%in%donnees_tronq$Dose) ){estimation_cure[j]<-0
                                estimation_surv[j]<-0}
    else{
    indice<-which(Predicted_survival_prob$categorie==dose_scaled[j])
    estimation_cure[j]<-1-Predicted_survival_prob[indice,"est"]
    estimation_surv[j]<-1-tp.surv(fit_surv,t_star)[[rang_dose]][1,][["surv"]]
    rang_dose<-rang_dose+1
    }
    data_returns[j,"p"]<-probabilite_a_priori[j]
  }
  data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimation_surv,estimation_cure)
  return(data_returns)
}

fonction_miss<-function(data,nb_doses){
  vecteur_doses_NA<-c()
  for (j in c(1:nb_doses)){
    nb_num_doses<-which(data$Dose==j)
    nb_num_miss_dose<-which(data$Dose==j & data$observe==0)
    if(length(nb_num_doses)==length(nb_num_miss_dose) && length(nb_num_miss_dose)!=0){
      if (length(nb_num_doses)==length(nb_num_miss_dose)){vecteur_doses_NA<-append(vecteur_doses_NA,j)}
      }
    }
  return(vecteur_doses_NA)
}
test_estim<-fonction_estim_hht(modele="constant",t_star=6,target=0.33)

require(ggplot2)
ggplot(data=test_estim,aes(x=c(1:nrow(test_estim)),y=estimateur_guerison,col="Guérison"))+
  geom_point()+
  geom_point(data=test_estim,aes(y=c(0.05,0.1,0.15,0.33,0.5),col="Probabilites a priori"))+
  labs(x="Indice de la dose",y="Valeur de la probabilité",title="Valeur des probabilités de toxicité")
ggplot(data=test_estim,aes(x=c(1:nrow(test_estim)),y=estimateur_survie,col="Survie"))+
  geom_point()+
  geom_point(data=test_estim,aes(y=c(0.05,0.1,0.15,0.33,0.5),col="Probabilites a priori"))+
  labs(x="Indice de la dose",y="Valeur de la probabilité",title="Valeur des probabilités de toxicité")
