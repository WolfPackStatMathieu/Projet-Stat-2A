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
fonction_estim_hht<-function(modele,t_star){
  probabilite_a_priori<-c(0.05,0.1,0.15,0.33,0.5)
  nb_doses<-length(probabilite_a_priori)
  donnees<-simul_tps_hht(modele,t_star,probabilite_a_priori)
  donnees$Dose<-as.factor(donnees$Dose)
  data_returns<-as.data.frame(matrix(NA,nb_doses,4))
  colnames(data_returns)<-c("estimateur_bernoulli","estimateur_survie","estimateur_guerison","p")
  vecteur<-c(1:nb_doses)
  fonction_MEAN<-function(vecteur,donnees){
    return(mean(donnees[which(donnees$Dose==vecteur),"observe"]))
  }
  estimateur_bern<-sapply(vecteur,fonction_MEAN,donnees=donnees)
  data_returns[,"estimateur_bernoulli"]<-estimateur_bern
  fonction_surv<-Surv(as.numeric(donnees$temps),event=donnees$observe)
  fit_surv <- survfit(fonction_surv ~Dose, data = donnees)
  print("chien")
  fit_cure<-flexsurvcure(Surv(temps,event=observe)~Dose, data = donnees, link="logistic", dist="weibullPH", mixture=T) 
  Predicted_survival_prob<-summary(fit_cure, t=t_star, type="survival", tidy=T)
  estimation_cure<-rep(NA,nb_doses)
  estimation_surv<-rep(NA,nb_doses)
  for (j in c(1:nb_doses)){
    indice<-which(Predicted_survival_prob$Dose==j)
    indice_censure_dosej<-which(donnees$observe==1 & donnees$Dose==j)
    print(indice_censure_dosej)
    if(length(indice_censure_dosej)==0){
      estimation_cure[j]<-1
      estimation_surv[j]<-1
    }
    else{
    estimation_cure[j]<-1-Predicted_survival_prob[indice,"est"]
    estimation_surv[j]<-1-tp.surv(fit_surv,t_star)[[j]][1,][["surv"]]}
  }
  data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimation_surv,estimation_cure)
  return(data_returns)
}

test_estim<-fonction_estim_hht(modele="constant",t_star=6)
