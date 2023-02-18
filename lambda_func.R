source("surv.R")

lambda_func <- function(lambda_vec, list_params){
  vec_biais_surv <- sapply(lambda_vec, function(h){
    fonction_biais_survie(list_params[["n"]], lambda_vec[h], list_params[["t_star"]])
  })
  return(vec_biais_surv)
}


# test
n<-100
t_star<-6
lambda_vec <- c(1.2,1.26, 1.18, 1.4,1.34)
liste_parameter<-list(n,t_star)

names(liste_parameter)<-c("n","t_star")




test <-lambda_func(lambda_vec,list_params =  liste_parameter)

ggplot(data.frame(x = lambda_vec, y = test), aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  ggtitle("Biais with different lambdas") +
  xlab("lambda") + ylab("Biais") +
  theme_minimal()



