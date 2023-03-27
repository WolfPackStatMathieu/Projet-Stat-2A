simul_weibull<-function(n,lambda,k){
  return(rweibull(n,shape=k,scale=1/lambda))
}

test_simul_weibull<-simul_weibull(50, 0.5, 1)

###note : the bias decreased when k increased. The reason is that the hazard function becomes 
## IFR so the probability that the event happens with time. It is thus more likely
## that we don't have NA values. 