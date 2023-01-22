library(dfcrm)

# Fenêtre d'observation DLT : obswin=6 unités
# Rythme d'inclusion : accrual = 3 patients en moyenne par fenêtre suivant une distribution de Poisson
# cible prob. DLT = 25%
# début à la dose 1
# scénario de simulation, 5 doses avec PI=c(0.05, 0.1, 0.25, 0.4, 0.55)
# a priori: prior, méthode intervalle d'indifférence

N=18
res <- titesim(# PI A vector of the true toxicity probabilites associated with the doses.
                PI=c(0.05, 0.1, 0.15, 0.33, 0.50),
               # prior A vector of initial guesses of toxicity probabilities associated with the doses. Must be of same length as PI.
               prior=getprior(0.05, 0.25, 2, 5),
               # target The target DLT rate.
               0.25, 
               # n Sample size of the trial.
               N,
               # x0 The initial design. For one-stage TITE-CRM, it is a single numeric value indicating the starting dose. For two-stage TITE-CRM, it is a non-decreasing sequence of dose levels of length n.
               1,
               # obswin The observation window with respect to which the MTD is defined.
               obswin=6,
               # restrict If TRUE, restrictions apply during the trials to avoid
               #(1) skipping doses in escalation and (2) escalation immediately 
               # after a toxic outcome (i.e., incoherent escalation). If FALSE, 
               # dose assignments are purely model-based.
               restrict=1,
               # rate Patient arrival rate: Expected number of arrivals per 
               #observation window. Example: obswin=6 and rate=3 means expecting 
               #3 patients arrive in 6 time units.
               rate=3,
               # accrual Patient accrual scheme. Default is "fixed" whereby 
               # inter-patient arrival is fixed. Alternatively, use "poisson" to 
               # simulate patient arrivals by the Poisson process.
               accrual = "poisson", 
               # seed	Seed of the random number generator
               seed=1234)


base_tox <- data.frame(id=1:N, dose=res$level, time_arrival=res$arrival, toxicity.study.time=res$toxicity.study.time, toxicity.time=res$toxicity.time)
head(base_tox)
base_tox$toxicity.study.time[base_tox$toxicity.study.time==Inf] <- NA
base_tox$toxicity.time[base_tox$toxicity.time==Inf] <- NA
base_tox$toxicity.study.time <- round(base_tox$toxicity.study.time, 2)
base_tox$toxicity.time <- round(base_tox$toxicity.time, 2)
base_tox$time_arrival <- round(base_tox$time_arrival, 2)
essai_n18 <- base_tox
essai_n18
plot(essai_n18)



N=24
res <- titesim(PI=c(0.05, 0.1, 0.25, 0.4, 0.55), 
               prior=getprior(0.05, 0.25, 2, 5), 
               0.25, N, 1,
               obswin=6,
               rate=3,
               accrual = "poisson", seed=1234)


base_tox <- data.frame(id=1:N, dose=res$level, time_arrival=res$arrival, toxicity.study.time=res$toxicity.study.time, toxicity.time=res$toxicity.time)
head(base_tox)
base_tox$toxicity.study.time[base_tox$toxicity.study.time==Inf] <- NA
base_tox$toxicity.time[base_tox$toxicity.time==Inf] <- NA
base_tox$toxicity.study.time <- round(base_tox$toxicity.study.time, 2)
base_tox$toxicity.time <- round(base_tox$toxicity.time, 2)
base_tox$time_arrival <- round(base_tox$time_arrival, 2)
essai_n24 <- base_tox


N=30
res <- titesim(PI=c(0.05, 0.1, 0.25, 0.4, 0.55), 
               prior=getprior(0.05, 0.25, 2, 5), 
               0.25, N, 1,
               obswin=6,
               rate=3,
               accrual = "poisson", seed=1234)


base_tox <- data.frame(id=1:N, dose=res$level, time_arrival=res$arrival, toxicity.study.time=res$toxicity.study.time, toxicity.time=res$toxicity.time)
head(base_tox)
base_tox$toxicity.study.time[base_tox$toxicity.study.time==Inf] <- NA
base_tox$toxicity.time[base_tox$toxicity.time==Inf] <- NA
base_tox$toxicity.study.time <- round(base_tox$toxicity.study.time, 2)
base_tox$toxicity.time <- round(base_tox$toxicity.time, 2)
base_tox$time_arrival <- round(base_tox$time_arrival, 2)
essai_n30 <- base_tox

write.table(essai_n18, file="Z:/STAGES/Stages_ENSAI/essai_n18.txt", sep="\t", row.names=F)
write.table(essai_n30, file="Z:/STAGES/Stages_ENSAI/essai_n24.txt", sep="\t", row.names=F)
write.table(essai_n30, file="Z:/STAGES/Stages_ENSAI/essai_n30.txt", sep="\t", row.names=F)
