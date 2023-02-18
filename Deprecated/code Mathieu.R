# Code Mathieu
N=18
res <- titesim(PI=c(0.05, 0.1, 0.15, 0.33, 0.50), 
               prior=getprior(0.05, 0.25, 2, 5), 
               0.25, N, 1,
               obswin=6,restrict=1,
               rate=3,
               accrual = "poisson", seed=1234)

str(res)
observations_time <- ifelse(!is.na(res$toxicity.time), res$toxicity.time, 6)

