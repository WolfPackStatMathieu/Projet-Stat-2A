source("weibull.R")
simul_tps_hht <- function(modele, t_star){
  if(modele=="constant"){
    alpha<-1
  }
  if(modele=="increasing"){
    alpha<-3
  }
  if(modele=="decreasing"){
    alpha<-0.1
  }
  probabilite_priori<-c(0.05,0.1,0.15,0.33,0.5)
  vec_res <- c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
  vec_tps <- rep(NA, length(vec_res))
  vec_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
  
  for (i in 1:length(vec_res)){
    if (vec_res[i] == 0){
      probabilite<-probabilite_priori[vec_dose[i]]
      lambda_dosek<-fonction_find_lambda(probabilite_priori=probabilite,t_star,alpha=alpha)
      tps_simul <- temps_simul1(lambda_dosek, alpha)
      nb_simul<-1
      reponse<-TRUE
      while(tps_simul<t_star){
        tps_simul<-temps_simul1(lambda_dosek,alpha)
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
      tps_simul <-temps_simul2(lambda_dosek, alpha,t_star=t_star)
      while(tps_simul>t_star){
        tps_simul<-temps_simul2(lambda_dosek,alpha,t_star=t_star)
      }
      vec_tps[i]<-tps_simul
    }
  }
  data_HHT<-cbind.data.frame(vec_dose,vec_res,vec_tps)
  return(data_HHT)
}
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
modele<-"constant"
test_simul_delai<-simul_tps_hht(modele=modele,t_star=6)
