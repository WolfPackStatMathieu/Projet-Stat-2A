
source("weibull.R")
simul_weibull(10,0.5,1)
# on veut 5 courbes: une pour chaque alpha de 1 à 5
# en abscisse: lambda, de 0.1 à 1
# en ordonnée: le taux de à t*=6

# 

table(simul_weibull(n=100,lambda= 0.5, k=1)<6)
tp <- simul_weibull(n=100,lambda= 0.5, k=1)
tp_t <- table(tp > 6)

as.numeric(tp_t["TRUE"])



proportion_censure <- function(n, lambda_min, lambda_max, pas_lambda, alpha, t_star){
  result <- NULL
  lambda_seq <- seq(lambda_min, lambda_max, pas_lambda)
  
  for (lambda in seq_along(lambda_seq)){
      vecteur_temps <- simul_weibull(n = n, lambda = lambda, k = alpha )
      table_vecteur_tps <- table(vecteur_temps > t_star)
      prop_censure <- as.numeric(table_vecteur_tps["TRUE"])/n
      result_lambda <- c(result_lambda, prop_censure)
    # result <- result_final
  }
  
  return(prop_censure)
}
proportion_censure(100, 0.1, 1, 0.1, 0.1,6)
