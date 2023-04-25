
source("weibull.R")
simul_weibull(10,0.5,1)
# on veut 5 courbes: une pour chaque alpha de 1 ? 5
# en abscisse: lambda, de 0.1 ? 1
# en ordonn?e: le taux de ? t*=6

# 

table(simul_weibull(n=100,lambda= 0.5, k=1)<6)
tp <- simul_weibull(n=1000,lambda= 0.5, k=1.7)
tp_t <- table(tp > 6)
as.numeric(tp_t["TRUE"])



#####recherche sur la densité de weibull #####

#create density plots
curve(dweibull(x, shape=1, scale = 10), from=0, to=4, col='red')
curve(dweibull(x, shape=1.5, scale = 1), from=0, to=4, col='blue', add=TRUE)
curve(dweibull(x, shape=1, scale = 1), from=0, to=4, col='blue', add=TRUE)
curve(dweibull(x, shape=1.5, scale = 3), from=0, to=4, col='blue', add=TRUE)
curve(dweibull(x, shape=1.5, scale = 3), col='blue')




#add legend
legend(2, .7, legend=c("shape=2, scale=1", "shape=1.5, scale=1"),
       col=c("red", "blue"), lty=1, cex=1.2)


# 

######### FONCTION proportion de censure selon alpha et lambda ########"
set.seed(123)
proportion_censure_selon_lambda_pour_1_alpha <- function(n, lambda_min, lambda_max, pas_lambda, alpha, t_star){
  lambda_seq <- seq(lambda_min, lambda_max, pas_lambda)
  # print(lambda_seq)
  j=1
  result_lambda<- as.data.frame(matrix(NA, nrow = length(lambda_seq), 2))
  colnames(result_lambda)<-c("lambda", paste("alpha = ", as.character(alpha)))
  for (lambda in lambda_seq){
      vecteur_temps <- simul_weibull(n = n, lambda = lambda, k = alpha )
      # print(vecteur_temps)
      table_vecteur_tps <- table(vecteur_temps > t_star)
      # print(table_vecteur_tps)
      prop_censure <- as.numeric(table_vecteur_tps["TRUE"])/n*100
    
      result_lambda[j,]<- c(lambda, prop_censure)
      j<-j+1
      
  }
  return(result_lambda)
}



######## TEST
proportion_censure_selon_lambda_pour_1_alpha(n=100, lambda_min=0.1, lambda_max=2, pas_lambda=0.1, alpha=1, t_star=6)



######### FONCTION proportion de censure selon alpha et lambda ########"
proportions_censures<-function(n, lambda_min, lambda_max, pas_lambda, alpha_min, alpha_max, pas_alpha, t_star){
  alpha_seq <- seq(alpha_min, alpha_max, pas_alpha)
  lambda_seq <- seq(lambda_min, lambda_max, pas_lambda)
  result<-as.data.frame(matrix(NA, nrow = length(lambda_seq), ncol = length(alpha_seq)))
  j<-1
  for (alpha in alpha_seq){
    
    result_alpha <- proportion_censure_selon_lambda_pour_1_alpha(n, lambda_min, lambda_max, pas_lambda, alpha, t_star)
    
    if(j==1){
      result[,c(1,2)]<-result_alpha
      colnames(result)[c(1,2)]<-colnames(result_alpha)
    }
    else{
      result[,j+1] <- result_alpha[,2]
      colnames(result)[j+1]<-colnames(result_alpha)[2]
    }
    j<-j+1
    
  }
  return(result)
}

n<-100
tableau_censures <-proportions_censures(n=n, lambda_min=0.1, lambda_max=2, pas_lambda=0.05, alpha_min=0.5, alpha_max = 2, pas_alpha = 0.1, t_star=6)

######## Traçage des courbes ##########
abscisses <- tableau_censures$lambda
courbe1 <- tableau_censures$`alpha =  0.8`
# p <-plot(x = abscisses, y = courbe1, type = "l", col = "red", xlab="lambda", ylab = "Proportion censure chez sensibles" ,
#      main = "Proportion de censurés parmi les sensibles selon lambda et alpha",
#      )
# for (j in 3:ncol(tableau_censures)){
#   lines(abscisses, tableau_censures[,j])
# }

p <- ggplot(data = tableau_censures, aes(x=lambda))
# loop
for (i in 2:length(tableau_censures)) {
  # use aes_string with names of the data.frame
  p <- p + geom_line(aes_string(y = tableau_censures[,i] 
                                , col = colnames(tableau_censures)[i]
                                )
                     ) 
}
p<- p + xlab("lambda") + ylab("proportion de censure chez les sensibles")
p <- p + labs(color='alpha', caption = sprintf("n = %s", as.character(n))) +
  ggtitle("Proportion de censurés parmi les sensibles selon lambda et alpha")+
  theme(legend.title=element_blank(),
        axis.text=element_text(family = "Helvetica", size=12),
        axis.title=element_text(family = "Helvetica", size=12),
        plot.title = element_text(family = "Helvetica", size = 14)) 

# print the result
print(p)





