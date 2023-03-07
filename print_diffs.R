##### Import fonctions.#####
source("generation_mean.R")
set.seed(133)
######Ne doit plus dépendre de modele, tout mettre dans une unique liste. gg1 avec une colonne de biais pour survie et une autre pour bern.#####???
fonction_compar_plots<-function(limit_inf,limit_sup,N,p,lambda,t_star,K,sh){
  #### N corresponds to the number of sizes. K correspond to the number of samples for each size. 
  require(gridExtra)
  vector_size<-sample(c(limit_inf:limit_sup),N)
  vector_size<-vector_size[order(vector_size)]
  liste_param<-list(lambda,t_star,sh,p)
  names(liste_param2)<-c("lambda","t_star","k","p")
  result<-fonction_generation_taille_mean(vector_size,modele_exp,liste_param2,K)
  result$taille<-vector_size
  colnames(result)<-c("Mean_Bias_Surv","Mean_Bias_Bern","Size")
  ####plot 
  gg1<-ggplot(data=result,aes(x=Size,y=Mean_Bias_Bern))+
    geom_point(colour="red")+
    labs(y="Mean Bias with Bern model")
  
  gg2<-ggplot(data=result,aes(x=Size,y=Mean_Bias_Surv))+
    geom_point(colour="blue")+
    labs(y="Mean Bias with Surv model")
  
  whole_g<-grid.arrange(gg1,gg2,ncol=2,top="Comparison of the two methods")
  return(whole_g)
}
######Test ######
p2<-0.3
k<-50
t_star<-6
lambda7<-(-1)*log(1-p2)/t_star
print(pexp(t_star,beta=1/lambda7))
lmoins<-1
l_plus<-1000
shape<-1
N<-50
test_plot<-fonction_compar_plots(limit_sup = l_plus,limit_inf = lmoins,N=N,p=p2,lambda=lambda7,t_star=t_star,K=k,sh=shape)
shape2<-3
lambdaweibull<-(-log(1-p2))^(1/shape2)/t_star
test2_plot<-fonction_compar_plots(limit_sup = l_plus,limit_inf = lmoins,N=N,p=p2,lambda=lambdaweibull,t_star=t_star,K=k,sh=shape2)

NSimulations.selon.n<-function(N,lambda,t_star,p,k){
  #' Matrice composee des biais moyens associes a la taille de l'echantillon de n=20 a n=200 par saut de 20.
  #'
  #' @param N nombre de tailles d'echantillon differents.
  #' @param lambda parametre de la loi exponentielle.
  #' @param t_star fin de la fenetre d'observation
  #'
  #' @return Valeur du biais moyen selon n dans l'intervalle (20,200).
  #' @export
  #'
  #' @examples
  #' ######Test ######
  #' t_star<-3
  #' N<-10
  #' lambda<-c(0.2)
  #' result<-NSimulations.selon.n(N,lambda,t_star)
  
  results<- NULL
  n<- 20
  while (n<200)
  {
    vecteur_biais<-rep(NA,N)
    biais<-  Simuler_biais_taillen(N,n,lambda,t_star,k=k,p=p)
    results<-rbind(results,c(n,mean(biais[["Biais survie"]])  ))
    n<- n+20
  }
  print(results)
  return(results)
}
fonction_compar_plotsn_lambda<-function(N,window_lambda,t_star,p,k){
  #' Plot des valeurs des biais moyens selon la taille des echantillons et du lambda.
  #'
  #' @param N nombre de tailles d'echantillon differents.
  #' @param window_lambda
  #' @param t_star fin de la fenetre d'observation
  #'
  #' @return Plot des valeurs des biais moyens en fonction du lambda et de la taille des echantillons.
  #' @export
  #'
  #' @examples
  #' ######Test ######
  #' t_star<-3
  #' N<-10
  #' window_lambda<-c(0.2,0.5,0.1)
  #' result<-fonction_compar_plotsn_lambda(N,window_lambda,t_star)
  
  set.seed(12345)
  RES<- NULL
  RES<- NSimulations.selon.n(N,window_lambda[1],t_star,k=k,p=p)
  RES0.2.3<-data.frame(RES)
  colnames( RES0.2.3)<- c("n","mean.bias")
  set.seed(12345)
  RES<- NULL
  RES<- NSimulations.selon.n(N,window_lambda[2],t_star,k=k,p=p)
  RES0.5.3<-data.frame(RES)
  colnames( RES0.5.3)<- c("n","mean.bias")
  
  set.seed(12345)
  RES<- NULL
  RES<- NSimulations.selon.n(N,window_lambda[3],t_star,k=k,p=p)
  RES0.1.3<-data.frame(RES)
  colnames( RES0.1.3)<- c("n","mean.bias")
  
  plot(RES0.2.3$n,RES0.2.3$mean.bias,title=paste("Influence of n"),
       ylim=c(-0.05,0.05),type='b',bty="n",xlab="nbre sujets",ylab="biais moyen")
  title("Influence de n et lambda")
  lines(RES0.5.3$n,RES0.5.3$mean.bias,type="b",col="blue")
  lines(RES0.1.3$n,RES0.1.3$mean.bias,type="b",col="red")
  abline(h=0)
  legend("topright",c("0.1","0.2","0.5"),col=c("red","black","blue"),lty=1,bty="n")
}

window_lambda<-c(0.2,0.5,0.1)
N<-50
t_star<-6
p<-0.33
k<-1
test_compar_lambda<-fonction_compar_plotsn_lambda(N,window_lambda,t_star,p=p,k=k)
