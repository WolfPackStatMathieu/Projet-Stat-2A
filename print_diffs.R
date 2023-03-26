##### Import fonctions.#####
source("generation_mean.R")
set.seed(133)
######Ne doit plus dependre de modele, tout mettre dans une unique liste. gg1 avec une colonne de biais pour survie et une autre pour bern.#####???

fonction_compar_plots<-function(limit_inf,limit_sup,N,p,lambda,t_star,K,sh){
  #### N corresponds to the number of sizes. K correspond to the number of samples for each size. 
  require(gridExtra)
  vector_size<-sample(c(limit_inf:limit_sup),N)
  vector_size<-vector_size[order(vector_size)]
  liste_param<-list(lambda,t_star,sh,p)
  names(liste_param)<-c("lambda","t_star","k","p")
  result<-fonction_generation_taille_mean(vector_size,liste_param,K)
  result$taille<-vector_size
  colnames(result)<-c("Mean_Bias_Bernoulli","Mean_Bias_Surv","Mean_Bias_Cure","Size")
  ####plot 
  borne_min<-min(min(result$Mean_Bias_Cure),min(result$Mean_Bias_Surv))
  borne_max<-max(max(result$Mean_Bias_Cure),max(result$Mean_Bias_Surv))
  gg1<-ggplot(data=result,aes(x=Size,y=Mean_Bias_Cure))+
    geom_point(colour="red")+
    labs(y="Mean Bias with cure model")+ylim(borne_min,borne_max)
  
  gg2<-ggplot(data=result,aes(x=Size,y=Mean_Bias_Surv))+
    geom_point(colour="blue")+
    labs(y="Mean Bias with Surv model")+ylim(borne_min,borne_max)
  
  whole_g<-grid.arrange(gg1,gg2,ncol=2,top="Comparison of the two methods")
  return(whole_g)
}
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
    mean_value<-mean(biais$Modele_survie)-p
    results<-rbind(results,c(n,mean_value))
    n<- n+20
  }
  return(results)
}
NSimulations.selon.n(K, lambda, t_star, p, k)

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
  borne_min<-min(min(RES0.5.3$mean.bias),min(RES0.1.3$mean.bias),min(RES0.2.3$mean.bias))
  borne_max<-max(max(RES0.5.3$mean.bias),max(RES0.1.3$mean.bias),max(RES0.5.3$mean.bias))
  plot(RES0.2.3$n,RES0.2.3$mean.bias,title=paste("Influence of n"),
       ylim=c(-0.1+borne_min,borne_max+0.1),
       type='b',bty="n",xlab="nbre sujets",ylab="biais moyen")
  title("Influence de n et lambda")
  lines(RES0.5.3$n,RES0.5.3$mean.bias,type="b",col="blue")
  lines(RES0.1.3$n,RES0.1.3$mean.bias,type="b",col="red")
  abline(h=0)
  legend("topright",c("0.1","0.2","0.5"),col=c("red","black","blue"),lty=1,bty="n")
}
print_eqm_mult_doses<-function(N,liste_parameter,limit_inf,limit_sup,nombre_doses)
{
  require(gridExtra)
  vector_size<-sample(c(limit_inf:limit_sup),N,replace=TRUE)
  vector_size<-vector_size[order(vector_size)]
  EQM<-fonction_simul_doses_eqm(vector_size=vector_size,nombre_doses=nombre_doses,vecteur_parametres=liste_parameter,K=N)
  vecteur_gg<-rep(NA,nombre_doses)
  result<-rep(NA,nombre_doses)
  collage<-""
  final_print<-c()
  collection_1<-c()
  for (j in c(1:nombre_doses)){
    data<-EQM[[j]]
    minimum<-min(data)
    maximum<-max(data)
    k<-liste_parameter[[j]][["k"]]
    lambda<-liste_parameter[[j]][["lambda"]]
    p<-liste_parameter[[j]][["p"]]
    essai<-{ggplot(data=data,aes(x=vector_size,y=Modele_guerison,col="Cure"))+
    geom_line()+
    geom_line(data=data,aes(x=vector_size,y=Modele_bernoulli,col="Bernoulli"))+
    geom_line(data=data,aes(x=vector_size,y=Modele_survie,col="Survival"))+
    ylim(minimum,maximum)+
    xlab("Taille echantillon") + ylab("EQM")+
    labs(caption = sprintf("lambda = %s, k = %s, p=%s" , 
                      as.character(lambda), 
                      as.character(k),
                      as.character(p)))}
    print(essai)
  }
}
######Test ######
p2<-0.3
k<-50
t_star<-6
lambda7<-(-1)*log(1-p2)/t_star
print(pexp(t_star,beta=1/lambda7))
lmoins<-1
l_plus<-100
shape<-1
N<-2
test_plot<-fonction_compar_plots(limit_sup = l_plus,limit_inf = lmoins,N=N,p=p2,lambda=0.7,t_star=t_star,K=k,sh=shape)
shape2<-3
lambdaweibull<-(-log(1-p2))^(1/shape2)/t_star
test2_plot<-fonction_compar_plots(limit_sup = l_plus,limit_inf = lmoins,N=N,p=p2,lambda=lambdaweibull,t_star=t_star,K=k,sh=shape2)


window_lambda<-c(0.7,0.5,0.1)
N<-50
t_star<-6
p<-0.33
k<-1
test_compar_lambda<-fonction_compar_plotsn_lambda(N,window_lambda,t_star,p=p,k=k)


##### test, print avec plusieurs doses. #####
N<-20
p<-0.33
lamdba_test<-0.33
t_star<-6
k1<-1
liste_parameter<-list(lambda_test,t_star,p,k1)
names(liste_parameter)<-c("lambda","t_star","p","k")
lb_test2<-0.2
t_star2<-7
p2<-0.5
k2<-1
liste_2<-list(lb_test2,t_star2,p2,k2)
names(liste_2)<-c("lambda","t_star","p","k")
vecteur_param<-list(liste_parameter,liste_2)
nb_doses<-2
lmoins<-1
l_plus<-100
test_print_mult_doses<-print_eqm_mult_doses(N=N,liste_parameter=vecteur_param,limit_inf =lmoins,limit_sup =l_plus,nombre_doses = nb_doses)
plot(test_print_mult_doses[1])
