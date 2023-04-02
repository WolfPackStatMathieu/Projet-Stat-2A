
simul_tps_hht <- function(lambda, t_star, k){
  vec_res <- c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)
  vec_tps <- rep(NA, length(vec_res))
  vec_dose <- c(1,2,3,4,5)
  
  for (i in 1:length(vec_res)){
    if vec_res[i] == 0:
      tps_simul <- simul_weibull(length(vec_dose), lambda, k)
      while(tps_simul < )
  }
}