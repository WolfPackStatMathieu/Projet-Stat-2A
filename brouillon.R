N<-10
vecteur_size<-sample(c(1:100),N)
lambda_test<-3
t_star<-6
p<-0.33
k<-1
liste_parameter<-list(lambda_test,t_star,p=p,k)
names(liste_parameter)<-c("lambda","t_star","p","k")
K2<-20
test_exp_eqm<-fonction_generation_eqm(vector_size=vecteur_size
                                      ,K=K2
                                      ,liste_parameter = liste_parameter
                                      )
##############################################################
fonction_generation_eqm<-function(vector_size,liste_parameter,K){
  #vector_size: un vecteur de N tailles d echantillons chacun de taille n_i
  # liste_parameter: la liste des parametres du modele
  # K: le nombre d echantillons (pour chaque taille n_i, il y aura K echantillons)
  
  ### renvoie la generation avec des tailles differentes avec un lambda,k,t_star,p. 
  # on classe les échantillons par ordre croissant
  vector_size<-vector_size[order(vector_size)]
  # on calcule la valeur du biais pour chacun  echantillon de chaque taille
  Value_bias<-lapply(vector_size,Simuler_biais_taillen,K=K,lambda=liste_parameter[['lambda']],t_star=liste_parameter[["t_star"]],
                     p=liste_parameter[["p"]],k=liste_parameter[["k"]])
  # on calcule la valeur moyenne du biais pour chaque taille d echantillon
  value_means<-as.data.frame(t(sapply(Value_bias,colMeans)))
  
  colMeans((value_means-p)^2)
  # on calcule la variance du biais pour chaque taille d echantillon
  value_variance<-as.data.frame(t(sapply(Value_bias,fonction_sapply)))
  # on calcule l erreur quadratique moyenne
  # le parametre p est le meme partout car on travaille avec une seule dose.
  #soustraction p^hat - p
  typeof(Value_bias)
  value_diff <- t(sapply(Value_bias,"-" , liste_parameter["p"]))
  str(value_diff)
  typeof(value_diff)
  # passage au carré
  
  func_square<- function(value_diff){return((value_diff)^2)}
  
  value_square <- sapply(value_diff, func_square)
  # moyenne pour chaque taille d echantillon
  new_value_mean2<-as.data.frame(t(sapply(new_value_eqm,means)))
  
  value_eqm<-(value_means-p)^(2)+value_variance
  return(value_eqm)