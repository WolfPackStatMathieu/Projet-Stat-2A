# Load required packages
library(survival)
#install.packages("survminer")
library(survminer)
library(dplyr)
library(dfcrm)


N=100 #Nombre de patients simulés
p<-0.33 # Valeur limite de toxicité
#Simulation des données par la fonction titesim
res <- titesim(PI=c(0.05, 0.1, 0.15, 0.33, 0.50), 
               prior=getprior(0.05, 0.25, 2, 5), 
               0.25, N, 1,
               obswin=6,
               restrict=1,
               rate=3,
               accrual = "poisson", seed=1234)

#Création d'un dataframe avec ces valeurs
base_tox <- data.frame(id=1:N, dose=res$level, time_arrival=res$arrival, toxicity.study.time=res$toxicity.study.time, toxicity.time=res$toxicity.time)
head(base_tox)
#Transformation des valeurs de la variable toxicity.study.time en NA si Inf
base_tox$toxicity.study.time[base_tox$toxicity.study.time==Inf] <- NA
#idem pour toxicity.time
base_tox$toxicity.time[base_tox$toxicity.time==Inf] <- NA
#On arrondit les valeurs à 2 chiffres après la virgule
base_tox$toxicity.study.time <- round(base_tox$toxicity.study.time, 2)
base_tox$toxicity.time <- round(base_tox$toxicity.time, 2)
base_tox$time_arrival <- round(base_tox$time_arrival, 2)


essai_n18 <- base_tox
essai_n18
plot(essai_n18)
#####On écrit une base de données. 
write.table(essai_n18, file="essai_n18.txt", sep="\t", row.names=F)
donnees<-read.table("essai_n18.txt",header=TRUE)
head(donnees)


donnees$temps <- donnees$toxicity.study.time - donnees$time_arrival
head(donnees)



#time_arrival  : quand est-ce que le patient arrive
# toxicity.study.time : le temps mis pour développer la toxicité
# toxicity.time = toxicity.study.time - time_arrival

#on crée une variable indiquant si l'observation a été 
#censurée ou non. 0= non censurée ; 1 = a été censurée.
donnees$isobserved <- ifelse(is.na(donnees$toxicity.study.time), 0, 1)
#quand il voit iscensored = 1, il ne met pas de +
head(donnees)
summary(donnees)
donnees$toxicity.time <- ifelse(is.na(donnees$toxicity.time), 6, donnees$toxicity.time)
head(donnees)
table(donnees$isobserved)
# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = donnees$toxicity.time, event = donnees$isobserved)
surv_object


fit <- survfit(surv_object ~ dose, data = donnees)
summary(fit)

# on cherche à récupérer les données au temps T=6
#afin de pouvoir tracer la droite Toxicité =f(dose)




ggsurvplot(fit, data= donnees, pval = TRUE)


# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ dose , 
                   data = donnees)
ggforest(fit.coxph, data = donnees)

coefficient <- fit.coxph$coefficients[["dose"]]
coefficient



y = 1 - exp(-c(1:5) * coefficient * 6 )
y
  

fit$surv   
fit$n
table(donnees$dose, donnees$isobserved)
valeur_dose4 <- fit$surv[29]
valeur_dose5 <- fit$surv[33]
fit$n
  