# Nous réalisons $N$ simulations en prenant en considération 1 seule dose, 1 seule
# proportion de DLT et une seule valeur de paramètre dans la loi exponentielle 
# générant les données. Nous calculons le biais selon le nombre de sujets grâce au
# Kaplan-Meier ou avec le modèle de guérison.
library(CurePkg) #permet d'utiliser les fonctions
set.seed(1234) 

####### Modèle de guérison: loi de Bernoulli B(p)#####
vecteur_size<-sample(c(1:100),10)
t_star<-6
prop<-0.33
list_param<-list(prop)
names(list_param)<-c("p")
modele2<-"bernoulli"
test_generation_taillediff_bern<-fonction_generation_taille_differente(
  vector_size = vecteur_size,
  model=modele2,
  liste_parameter = list_param)
