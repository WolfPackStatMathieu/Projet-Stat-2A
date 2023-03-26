source("estimateurs/estimateur_KM.R")
source("estimateurs/estimateur_cure.R")
fonction_Bern<-function(df){
  return(mean(df$is_observed))
}


###### TEST #####
df<-Generation_un_ech(n=50,lambda=0.5,p=0.5,k=1,t_star=6)
test_bern<-fonction_Bern(df)
