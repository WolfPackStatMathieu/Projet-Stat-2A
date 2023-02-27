rm(list=ls())
source("surv.R")
source("bernoulli.R")
library("ggplot2")

lambda_func <- function(lambda_vec, list_params,N){
  vec_biais_surv <- cbind.data.frame(sapply(lambda_vec,Simuler_Nfois_n_echantillons,n=list_params[["n"]],
                           t_star=list_params[["t_star"]],N=N))
  return(colMeans(vec_biais_surv))
}

pbinom_func <- function(p_vec, n,N){
  vec_biais_binom <- cbind.data.frame(sapply(p_vec,Simuler_Nfois_n_echantillons_bern,n=n,N=N))
  return(colMeans(vec_biais_binom))
}


# test binom
vec_p <- seq.int(from=0.1,to=0.9,by=0.07)
N<-100
n<-100
test1 <- pbinom_func(vec_p, n,N)


ggplot(data.frame(x = vec_p, y = test1), aes(x = x, y = y)) +
  geom_line(color = "Sky blue", size = 1.5) +
  ggtitle("Bias with different p") +
  xlab("p") + ylab("Biais") +
  theme_minimal()

test1


# test surv
N<-100
n<-100
t_star<-6
lambda_vec <- c(1.8,1.41, 1.28, 1.25,1.42, 1.04)
lambda_vec<-lambda_vec[order(lambda_vec)]
liste_parameter<-list(n,t_star)
names(liste_parameter)<-c("n","t_star")
test <-lambda_func(lambda_vec=lambda_vec,list_params =  liste_parameter,N)
ggplot(data.frame(x = lambda_vec, y = test), aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  ggtitle("Bias with different lambdas") +
  xlab("lambda") + ylab("Biais") +
  theme_minimal()
test
rm(test)

