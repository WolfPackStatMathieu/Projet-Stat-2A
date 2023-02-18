source("surv.R")
source("bernoulli.R")

library("ggplot2")

lambda_func <- function(lambda_vec, list_params){
  vec_biais_surv <- sapply(lambda_vec, function(h){
    fonction_biais_survie(list_params[["n"]], lambda_vec[h], list_params[["t_star"]])
  })
  return(vec_biais_surv)
}

pbinom_func <- function(p_vec, n){
  vec_biais_binom <- sapply(p_vec, function(h){
    biais_pi(n, p_vec[h])
  })
  return(vec_biais_binom)
}


# test binom
vec_p <- c(0.2,0.33,0.24,0.12,0.18)
test1 <- pbinom_func(vec_p, n)



ggplot(data.frame(x = vec_p, y = test1), aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  ggtitle("Biais with different p") +
  xlab("p") + ylab("Biais") +
  theme_minimal()

test1


# test surv
n<-100
t_star<-6
lambda_vec <- c(1.8,1.41, 1.28, 1.25,1.42, 1.04)
liste_parameter<-list(n,t_star)

test <-lambda_func(lambda_vec,list_params =  liste_parameter)

ggplot(data.frame(x = lambda_vec, y = test), aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  ggtitle("Biais with different lambdas") +
  xlab("lambda") + ylab("Biais") +
  theme_minimal()
test
rm(test)

