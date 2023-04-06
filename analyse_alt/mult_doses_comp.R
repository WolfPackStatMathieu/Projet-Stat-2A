source("generation_echantillon/generation_ech_comp.R")
source("estimateurs/mod_bernoulli.R")
function_estim_doses_comp<-function(n,probabilite_a_priori,t_star,type1,graine=133){
  nb_doses<-length(probabilite_a_priori)
  require(dfcrm)
  df<-matrix(NA,n,4)
  df<-as.data.frame(df)
  data_returns<-as.data.frame(matrix(NA,nb_doses,4))
  colnames(data_returns)<-c("estimateur_bernoulli","estimateur_survie","estimateur_guerison","p")
  colnames(df)<-c("dose","statut","tox_time","is_observed")
  df$dose<-sample(c(1:nb_doses),n,replace=TRUE)
  for (k in c(1:nb_doses)){
    index_dosek<-which(df$dose==k)
    p<-probabilite_a_priori[k]
    n_k<-length(index_dosek)
    df[index_dosek,]<-cbind(rep(k,n_k),generation_comp(p_cause1=p,p_cause2=1-p,t_star,nombre_obs=n_k,type1,type2=type1,graine=graine))
    df$tox_time<-ifelse(df$statut==2,t_star+1,df$tox_time)
    df$is_observed<-ifelse(df$tox_time>t_star,0,1)
    data_returns[k,"estimateur_bernoulli"]<-fonction_Bern(df[index_dosek,])
    data_returns[k,"p"]<-p
  }
  dose_recalibree<-crm(prior=data_returns$p,target=0.50, tox =df$is_observed, level =df$dose,n=n, model="logistic")$dosescaled
  fonction_surv<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  indice_cens<-which(df$is_observed==0)
  if(length(indice_cens)==0){
    estimateur_cure<-rep(1,nb_doses)
    estimateur_surv<-rep(1,nb_doses)
    data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimateur_surv,estimateur_cure)
  }
  else{
    df$factdose<-as.factor(dose_recalibree[df$dose])
    fit_surv <- survfit(fonction_surv ~factdose, data = df)
    fit_cure<-flexsurvcure(Surv(tox_time,event=is_observed)~factdose, data = df, link="logistic", dist="weibullPH", mixture=T)
    Predicted_survival_prob<-summary(fit_cure, t=t_star, type="survival", tidy=T)
    colnames(Predicted_survival_prob)<-c("time","est","lcl","ucl","categorie")
    estimation_cure<-rep(NA,nb_doses)
    estimation_surv<-rep(NA,nb_doses)
    for (j in c(1:nb_doses)){
      indice<-which(Predicted_survival_prob$categorie==dose_recalibree[j])
      estimation_cure[j]<-1-Predicted_survival_prob[indice,"est"]
      estimation_surv[j]<-1-tp.surv(fit_surv,t_star)[[j]][1,][["surv"]]}
  }
  data_returns[,c("estimateur_survie","estimateur_guerison")]<-c(estimation_surv,estimation_cure)
  return(data_returns)
}
p<-0.25
p2<-0.50
p3<-0.7
prob_priori<-c(p,p2,p3)
test_mult_doses<-function_estim_doses_comp(n=25,probabilite_a_priori = prob_priori,t_star=6,type1 = "decreasing",graine=145)

#############MEAN####
generation_comp_mean<-function(K,n,probabilite_a_priori,t_star,type1,graine_depart){
  graine_modif<-graine_depart+c(1:K)
  result<-lapply(graine_modif,function_estim_doses_comp,probabilite_a_priori=probabilite_a_priori,t_star=t_star,type1=type1,n=n)
  nb_doses<-length(prob_priori)
  matrice<-as.data.frame(matrix(NA,nb_doses,5))
  colnames(matrice)<-c("numero_dose","moyenne_estimateur_bernoulli","moyenne_estimateur_survie","moyenne_estimateur_guerison","p")
  matrice$numero_dose<-c(1:nb_doses)
  for(j in c(1:nb_doses)){
    ensemble_obs_dosek<-t(cbind.data.frame(sapply(result,function(x,indice){return(x[indice,])},indice=j)))
    ensemble_obs_dosek<-as.data.frame(ensemble_obs_dosek)
    ensemble_obs_dosek$estimateur_bernoulli<-as.numeric(ensemble_obs_dosek$estimateur_bernoulli)
    ensemble_obs_dosek$estimateur_guerison<-as.numeric(ensemble_obs_dosek$estimateur_guerison)
    ensemble_obs_dosek$estimateur_survie<-as.numeric(unlist(ensemble_obs_dosek$estimateur_survie))
    ensemble_obs_dosek$p<-as.numeric(ensemble_obs_dosek$p)
    matrice[j,c("moyenne_estimateur_bernoulli","moyenne_estimateur_survie","moyenne_estimateur_guerison","p")]<-colMeans(ensemble_obs_dosek)
  }
  return(matrice)
}
test_mult_Ktimes<-generation_comp_mean(K=20,n=25,probabilite_a_priori = prob_priori,t_star=t_star,type1="decreasing",graine_depart = 133)
