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
      #probabilite<-probabilite_a_priori[vec_dose[i]]
      #lambda_dosek<-fonction_find_lambda(probabilite_priori=probabilite,t_star,alpha=alpha)
      #tps_simul <- temps_simul1(1/lambda_dosek, alpha)
      #on a cette fois l'autre paramétrisation. 
      #on doit donc utiliser 1/lambda car dans weibull, la probabilité est selon la première paramétrisation. 
      #nb_simul<-1
     # reponse<-TRUE
     ## while(tps_simul<t_star){
     #   tps_simul<-temps_simul1(1/lambda_dosek,alpha)
       # nb_simul<-nb_simul+1
      #  if (nb_simul>10){
      #    reponse<-FALSE
    #      break
       # }
     # }
     # if(tps_simul<t_star){tps_simul<-7}
     # vec_tps[i]<-tps_simul
      vec_tps[i]<-t_star+1
    }
    else{
      lambda<-0.6
      tps_simul <-rexp(n=1,lambda)
      while(tps_simul>t_star){
        tps_simul<-rexp(n=1,lambda)
      }
      vec_tps[i]<-tps_simul
    }
  }
  data_HHT<-cbind.data.frame(vec_dose,vec_res,vec_tps)
  colnames(data_HHT)<-c("Dose","observe","temps")
  return(data_HHT)
}

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
set.seed(133)
test<-simul_tps_hht("constant",t_star=6,probabilite_a_priori =c(0.05,0.1,0.15,0.33,0.5))
print(test)
######### calcul des estimateurs.#####
library(survival)
fonction_estim_hht<-function(modele,t_star,target){
  require(dfcrm)
  require(cuRe)
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
  estimateur_bern<-sapply(vecteur,fonction_MEAN,donnees=donnees)
  estimateur_bern[c(2,5)]<-rep(NA,2)
  data_returns[,"estimateur_bernoulli"]<-estimateur_bern
  donnees$factdose<-as.factor(donnees$Dose)
  fonction_surv<-Surv(as.numeric(donnees$temps),event=donnees$observe)
  fit_surv <- survfit(fonction_surv ~factdose, data = donnees)
  Prob_whole_cure<-fit.cure.model(Surv(temps,observe) ~ factdose, data =donnees,
                                  formula.surv=list(~1+factdose),
                                  dist="weibull",
                                  link="logit")
  beta0<-as.numeric(Prob_whole_cure$coefs[1]$'1')[1]
  reste_beta<-as.numeric(Prob_whole_cure$coefs[1]$'1')[c(2:nb_doses)]
  coeffs<-beta0+c(0,reste_beta)
  prob_tox<-1-plogis(coeffs)
  estimation_cure<-rep(NA,nb_doses)
  estimation_surv<-rep(NA,nb_doses)
  data_returns[,"p"]<-probabilite_a_priori
  estimation_surv[c(2,5)]<-rep(NA,2)
  estimation_surv[1]<-0
  estimation_surv[3]<-1-tp.surv(fit_surv,t_star)[[2]][1,][["surv"]]
  estimation_surv[4]<-1-tp.surv(fit_surv,t_star)[[3]][1,][["surv"]]
  estimation_cure[1]<-prob_tox[1]
  estimation_cure[c(2,5)]<-rep(NA,2)
  estimation_cure[3]<-prob_tox[2]
  estimation_cure[4]<-prob_tox[3]
  #for (j in c(1:nb_doses)){
   # if (j %in% dose_all_missed || !(j%in%donnees_tronq$Dose) )
    #  {
     # estimation_cure[j]<-NA
      #estimation_surv[j]<-NA}
  #  else{
   # print(rang_dose)
    #estimation_cure[rang_dose+1]<-prob_tox[rang_dose+1]
  #  estimation_surv[j]<-1-tp.surv(fit_surv,t_star)[[rang_dose]][1,][["surv"]]
   # rang_dose<-rang_dose+1
  #  }
   # data_returns[j,"p"]<-probabilite_a_priori[j]
  #}
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
set.seed(133)
test_estim<-fonction_estim_hht(modele="constant",t_star=6,target=0.33)
test_estim1<-fonction_estim_hht(modele="increasing",t_star=6,target=0.33)
test_estim2<-fonction_estim_hht(modele="decreasing",t_star=6,target=0.33)

require(ggplot2)
ggplot(data=test_estim,aes(x=c(1:nrow(test_estim)),y=estimateur_guerison,col="Guérison"))+
  geom_line()+
  geom_line(data=test_estim,aes(y=c(0.05,0.1,0.15,0.33,0.5),col="Probabilites a priori"))+
  labs(x="Indice de la dose",y="Valeur de la probabilité",title="Valeur des probabilités de toxicité")
ggplot(data=test_estim,aes(x=c(1:nrow(test_estim)),y=estimateur_survie,col="Survie"))+
  geom_point()+
  geom_point(data=test_estim,aes(y=c(0.05,0.1,0.15,0.33,0.5),col="Probabilites a priori"))+
  labs(x="Indice de la dose",y="Valeur de la probabilité",title="Valeur des probabilités de toxicité")



# color palette
library(RColorBrewer)
palette <- brewer.pal(8, "Set1")
p <- 0.33
borne_min <- min(test_estim[c(1:4),],na.rm=TRUE)
borne_max <- max(test_estim[c(1:4),],na.rm=TRUE)
# plot
plot(x = c(1:3), y = c(0.05,0.15,0.33), 
     xlab = "Indice de dose",
     ylab = "Probabilité",
     main = "Valeur des probabilités de toxicité",
     type = "b",
     col = "blue",
     pch = 19, # Use a solid circle as point marker
     lwd = 2,
     ylim=c(borne_min,borne_max)) # Increase line width
lines(x = c(1:3), y = test_estim$estimateur_survie[c(1,3,4)], col = "black")
lines(x = c(1:3), y = test_estim$estimateur_guerison[c(1,3,4)], col = "purple")


# Add horizontal line
#abline(h = p, col = "red", lwd = 2)

# Add legend
legend("topleft", # Position of the legend
       c("Probabilité a priori", "Survie", "Guérison"), # Labels
       col = c("blue", "black", "purple"), # Colors
       pch = c(19, NA), # Point markers (NA means no marker)
       lty = c(1, 1), # Line styles (1 means solid)
       lwd = c(2, 2),
       bty ="n",
       cex = 0.6) # Line widths

