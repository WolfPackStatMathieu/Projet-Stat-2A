
######################## IMPORT #####
library(survival)
library(flexsurvcure)
library(npcure)

####### Fonction ######
fonction_cure<-function(df,t_star){
  require(npcure)
  require(smcure)
  # retourne la probabilite de ne pas avoir fait de DLT a T_star
  indice_observed<-which(df$is_observed==1)
  indice_censored<-which(df$is_observed==0)
  df$covar<-rep(1,nrow(df))
  df2<-df[,c("covar","is_observed","tox_time")]
  #hb <- probcurehboot(covar, tox_time, is_observed, df2, x0 = 1, bootpars = controlpars(B =
   #                                                                   1999, hsmooth = 3))
  #prob_gueri<-probcure(x=covar,t=tox_time,dataset = df2,d=is_observed,x0=1, h =c(1,1.5,2),local=FALSE)
  #estimateur_tox<-1-prob_gueri[["q"]]$h1
  #result<-flexsurvcure(Surv(tox_time,event=is_observed)~1,data=df,
#link="logistic", dist="weibullPH", mixture=T)
  ### the theta in the print is the real value of "theta", 
  ### the theta in the result is the qlogis of the first one. 
  ## plogis of the second one to get the real value back. 
  #Prob_cure<-plogis(coef(result)[1])
  #estimateur_tox<-1-Prob_cure
  #df2<-data.frame(df2)
  #pd <- smcure(Surv(tox_time,is_observed)~1+covar2,offset=NULL,
             #cureform=~1+covar2,data=df2,model="ph",
            #  Var = TRUE)
  #print("test2")
  #estimateur_tox<-1-plogis(coef(pd))
  require(cuRe)
  summary(df2)
  fit.wei <- fit.cure.model(Surv(tox_time,is_observed) ~ 1, data =df2,
                            dist="weibull",link="logit")
  prob_cure<-plogis(as.numeric(fit.wei$coefs[1]))
  estimateur_tox<-1-prob_cure
  return(estimateur_tox)
}

estimateur_cure_mult<-function(df,t_star,nb_doses){
  data_return<-rep(NA,nb_doses)
  somme<-as.data.frame(sapply(c(1:nb_doses),function(x,df)return(c(x,sum(df[which(df$label_dose==x),"observed"]))),df=df))
  return(somme)
}
###### Test###
#df<-Generation_un_ech(n=10,lambda=0.1,p=0.5,k=1,t_star=6)


# on cree un surv_object a partir du dataframe
#surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
# on estime la probabilite d avoir fait une DTL avant t_star avec la fonction flexsurvecure
result<-flexsurvcure(Surv(rectime,censrec)~1, data=bc, dist="weibullPH")
coeff<-coef(result)
# on recupere l estimation en t_star
#appel_cure<-fonction_cure(df,t_star=6)
#mean(df$is_observed)

#surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
library(nltm)
data(melanoma, package="nltm")
fit <- nltm(Surv(time,status) ~ size + age, data=melanoma, nlt.model="PH")
data("e1684")
pd <- smcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,
             cureform=~TRT+SEX+AGE,data=e1684,model="ph",
             Var = FALSE)
library(smcure)
colonDC <- colonDC[sample(1:nrow(colonDC), 400), ]
summary(colonDC)

##Extract general population hazards
colonDC$bhaz <- general.haz(time = "FU", rmap = list(age = "agedays", sex = "sex", year= "dx"),
                            data = colonDC, ratetable = survexp.dk)
fit <- fit.cure.model(Surv(FUyear, status) ~ 1, data = colonDC,type="mixture",link="logit")
plot(fit, type = "probcure")
summary(fit)
library(flexsurvcure)
result<-flexsurvcure(Surv(rectime,censrec)~group+0, data=bc, dist="weibullPH",anc=list(scale=~group+0))
plogis(result$coefficients)
result$all.formulae$theta
result$dlist
