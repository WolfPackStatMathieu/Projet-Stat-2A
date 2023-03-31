source("weibull.R")
source("estimateurs/mod_bernoulli.R")
source("estimateurs/estimateur_KM.R")
source("estimateurs/estimateur_cure.R")
source("utils.R")
vecteur_dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
vecteur_reponse<-c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
nom_dose<-c(1,2,3,4,5)
valeur_dose<-c(0.5,1,3,5,6)
df<-cbind.data.frame(vecteur_dose,vecteur_reponse)
colnames(df)<-c("label_dose","observed")
prob_prior<-c(0.05,0.10,0.15,0.33,0.50)
t_star<-6
Vecteur_k<-c(0.1,1,3)
generation_temps_fictif<-function(df,t_star,prob_prior,k=0.01){
  vecteur_lambda<-exp(1/k * log(-t_star^k/log(1-prob_prior)))
  df$temps<-rep(NA,nrow(df))
  data_returns<-as.data.frame(matrix(NA,nb_doses,4))
  colnames(data_returns)<-c("estimateur_bernoulli","estimateur_survie","estimateur_guerison","p")
  for (j in c(1:length(vecteur_lambda))){
    indice_obs_dosej<-which(df$label_dose==j & df$observed==1)
    index_dosek<-which(df$label_dose==j)
    data_returns[j,"estimateur_bernoulli"]<-fonction_Bern(df[index_dosek,])
    df$temps[indice_obs_dosej]<-simul_weibull(length(indice_obs_dosej),vecteur_lambda[j],k)
  }
  #fin creation des delais
  return(df)
}
generation_estim<-function(df,t_star,prob_prior,k=0.01){
  df<-generation_temps_fictif(df,t_star,prob_prior,k=0.01)
  df[which(df$observed==0),"temps"]<-t_star+1
  estim_bern<-mean(df$observed)
  fit_surv<-survfit(Surv(temps,event=observed)~label_dose,data=df)
  fit_cure<-flexsurvcure(Surv(temps,event=observed)~label_dose, data = df, link="logistic", dist="weibullPH", mixture=T) 
  Predicted_survival_prob<-summary(fit_cure, t=t_star, type="survival", tidy=T)
  estimation_cure<-rep(NA,length(vecteur_lambda))
  estimation_surv<-rep(NA,length(vecteur_lambda))
  for (j in c(1:length(vecteur_lambda))){
    indice<-which(Predicted_survival_prob$label_dose==j)
    estimation_cure[j]<-1-Predicted_survival_prob[indice,"est"]
    estimation_surv[j]<-1-tp.surv(fit_surv,t_star)[[j]][1,][["surv"]]}
  data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimation_surv,estimation_cure)
  return(data_returns)
}
test_first<-generation_temps_fictif(df,t_star=t_star,prob_prior = prob_prior)
test_j<-estimateur_cure_mult(df,t_star=6,nb_doses = 5)
