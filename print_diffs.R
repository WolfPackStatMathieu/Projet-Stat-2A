##### Import fonctions.#####
source("generation_mean.R")
set.seed(133)

fonction_compar_plots<-function(limit_inf,limit_sup,N,p,lambda,t_star,K){
  #### N corresponds to the number of sizes. K correspond to the number of samples for each size. 
  require(gridExtra)
  require(ggplot2)
  vector_size<-sample(c(limit_inf:limit_sup),N)
  vector_size<-vector_size[order(vector_size)]
  liste_param1<-list(p)
  names(liste_param1)<-c("p")
  modele_bern<-"bernoulli"
  result1<-fonction_generation_taille_mean(vector_size,modele_bern,liste_param1,K)
  liste_param2<-list(lambda,t_star)
  names(liste_param2)<-c("lambda","t_star")
  modele_exp<-"surv"
  result2<-fonction_generation_taille_mean(vector_size,modele_exp,liste_param2,K)
  whole_data_expbern<-cbind.data.frame(vector_size,result1,result2)
  colnames(whole_data_expbern)<-c("Size","Mean_Bias_Bern","Mean_Bias_Surv")
  ####plot 
  gg1<-ggplot(data=whole_data_expbern,aes(x=Size,y=Mean_Bias_Bern))+
    geom_line(colour="red")+
    labs(y="Mean Bias with cure model")
  
  gg2<-ggplot(data=whole_data_expbern,aes(x=Size,y=Mean_Bias_Surv))+
    geom_line(colour="blue")+
    labs(y="Mean Bias with Surv model")
  
  whole_g<-grid.arrange(gg1,gg2,ncol=2,top="Comparison of the two methods")
  return(whole_g)
}
p2<-0.33
k<-20
lambda7<-0.33
t_star<-6
lmoins<-1
l_plus<-1000
N<-20
test_plot<-fonction_compar_plots(limit_sup = l_plus,limit_inf = lmoins,N=N,p=p2,lambda=lambda7,t_star=t_star,K=k)
