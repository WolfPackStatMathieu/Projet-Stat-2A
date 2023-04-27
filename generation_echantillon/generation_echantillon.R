library(survival)
require(flexsurvcure)
source("weibull.R")
Generation_un_ech<-function(n,lambda,t_star,p,k){
  vecteur_censure<-rbinom(n,1,p)
  vecteur_temp<-rep(NA,n) # cree un vecteur des temps associ?s
  # l'estimateur du modele de Bernoulli est la moyenne des 1 du vecteur_censure
  # on cree un dataframe qui acolle la DLT au vecteur_temps
  df<-cbind.data.frame(vecteur_censure,vecteur_temp)
  #on les renomme
  colnames(df)<-c("sensible","temps")
  #recuperation des numeros de lignes des individus censures
  id_non_sensibles<-which(df$sensible==0)
  #recuperation des numeros de lignes des individus a risque de DLT
  id_sensibles<-which(df$sensible==1)
  #tous les individu censure se voient attribues comme temps la limite de la 
  #fenetre d observation
  df[id_non_sensibles,2]<-t_star+1
  # les autres individus se voient attribuer un temps simule a partir d une 
  # loi de Weibull (qui peut etre une loi exponentielle si k=1)
  df[id_sensibles,2]<-simul_weibull(length(id_sensibles),lambda,k)
  # bien sur, si le temps observe est superieur a la fenetre d observation, alors
  # on le remplace par la fin de fenetre d observation
  # on renomme les colonnes pour une meilleure interpretation
  # on remplit la colonne isobserved avec des 1 si on observe une toxicite avant
  #la fin de la fenetre d observation, sinon on met des 0
  colnames(df)<-c("sensible","tox_time")
  df$is_observed<-ifelse(df$tox_time<t_star,1,0)
  return(df)
}



############ TEST #####
generat<-Generation_un_ech(n=100,lambda=0.5,t_star=6,p=0.33,k=1)
table(se=generat$sensible, obs=generat$is_observed)
so <- survfit(Surv(tox_time, is_observed)~1, data=generat)
plot(so)
tp.surv(so,6)
f <- flexsurvcure(Surv(tox_time, is_observed)~1, data=generat, dist = "weibull")

est_p = 1 - exp(f$coefficients[1])/(1+exp(f$coefficients[1]))

est_p


####### calcul pourcentage censure moyen ####
## construction du code sur un exemple
ech <-Generation_un_ech(n=100,lambda=0.65,t_star=6,p=0.33,k=1)
ech

table(ech$sensible)
table(ech$is_observed)
prop_censure_total <- as.numeric(table(ech$is_observed==0)["TRUE"])
typeof(prop_censure_total)
prop_censure_total
prop_censure_gueris <- as.numeric(table(ech$sensible == 0)["TRUE"])/100
prop_censure_gueris
result_censures<- as.data.frame(matrix(NA, nrow = length(1), 3))
colnames(result_censures)<-c("prop_censure_totale", "prop_censure_gueris", "prop_censure_additionnelle")
result_censures
#pourcentage de censure totale dans l echantillon
result_censures[1,"prop_censure_totale"] <-as.numeric(table(ech$is_observed==0)["TRUE"])/100
# censure due au pourcentage de gueris, au debut de la generation de l echantillon
result_censures[1,"prop_censure_gueris"] <- as.numeric(table(ech$sensible == 0)["TRUE"])/100
# calcul de la censure additionnelle= censure_totale - censure_gueris
result_censures[1,"prop_censure_additionnelle"]<-result_censures[1,"prop_censure_totale"] - result_censures[1,"prop_censure_gueris"] 
result_censures


calcule_prop_censure<-function(N, n, lambda, t_star, p, k){
  #initialisation du dataframe
  result_censures<- as.data.frame(matrix(NA, nrow = length(N), 3))
  colnames(result_censures)<-c("prop_censure_totale", "prop_censure_gueris", "prop_censure_additionnelle")
  j<-1 #compteur de ligne
  for (i in 1:N){
    ech <-Generation_un_ech(n,lambda,t_star,p,k) #genere un echantillon
    #pourcentage de censure totale dans l echantillon
    result_censures[j,"prop_censure_totale"] <-as.numeric(table(ech$is_observed == 0)["TRUE"])/n 
    # censure due au pourcentage de gueris, au debut de la generation de l echantillon
    result_censures[j,"prop_censure_gueris"] <- as.numeric(table(ech$sensible == 0)["TRUE"])/n
    # calcul de la censure additionnelle= censure_totale - censure_gueris
    result_censures[j,"prop_censure_additionnelle"]<-result_censures[j,"prop_censure_totale"] - result_censures[j,"prop_censure_gueris"] 
    
    j<- j+1
  }
  
  # boxplot_plot_censures<-ggplot(result_censures) +
  violin_plot <- result_censures %>% 
    gather(key="Type_de_censure", value="Val") %>%
    ggplot( aes(x=Type_de_censure, y=Val, fill=Type_de_censure) ) +
    geom_violin() +
    ggtitle("Distribution des types de censure, modèle de géneration 1")+
    ylab("Pourcentage de censure") + xlab("Type de censure")+ 
    theme(axis.text=element_text(family = "Helvetica", size=18),
          axis.title=element_text(family = "Helvetica", size=18),
          plot.title = element_text(family = "Helvetica", size = 25)) +
    labs(caption = sprintf("N = %s, n = %s, lambda= %s,t_star= %s, p= %s, k= %s", as.character(N),as.character(n), as.character(lambda), as.character(t_star),as.character(p), as.character(k)))
  print(violin_plot)
  return(result_censures)
}

#### Test ####
set.seed(133)
res_censures <- calcule_prop_censure(N= 1900, n=25,lambda=0.5,t_star=6,p=0.33,k=1)
res_censures
summary(res_censures)
head(res_censures)
