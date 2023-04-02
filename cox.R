# Import package
library("dfcrm")
library(visreg)


# construction de la base de donn√©es
dose <- c(rep(0.5,3),rep(3,3), rep(5,3), rep(5,3), rep(5,3),rep(5,3))
dosesWm <- c(0.05, 0.1, 0.15, 0.33, 0.50)
cibleP <- 0.33
reponse <- c(rep(0,3), 0,0,1, 0,0,1, rep(0,3), 0,1,0, 1,0,1)
N <- 18

df <- cbind.data.frame(dose, reponse)

crm(prior = dosesWm, 
    target = cibleP,
    tox = reponse,
    level = dose,
    n = N,
    model = "empiric")

# On remarque que la probabilit√© de toxicit√© augmente au fur et √† m√©sure que
# le niveau de dose augmente.Et que la prochaine dose recommand√©e est de 5.
# La variance post√©rieure de b√™ta √©tant faible indique que les donn√©es 
# sont informatives sur le param√®tre b√™ta. Ce qui semble sugg√©rer que le
# mod√®le est adapt√© aux donn√©es.



library(dfcrm)

# FenÍtre d'observation DLT : obswin=6 unitÈs
# Rythme d'inclusion : accrual = 3 patients en moyenne par fenÍtre suivant une distribution de Poisson
# cible prob. DLT = 25%
# dÈbut ‡ la dose 1
# scÈnario de simulation, 5 doses avec PI=c(0.05, 0.1, 0.25, 0.4, 0.55)
# a priori: prior, mÈthode intervalle d'indiffÈrence

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


# On veut ajuster un modËle de Cox en utilisant la dose comme variable explicative
# et la durÈe de survie (ici toxicity.study.time) comme variable dÈpendante
# On commence ‡ crÈer une variable "event" qui indique toxicitÈ (1) ou non (0)
# et le utiliser le package "Survival" pour la modÈlisation
# le modËle s'Ècrit de la forme :

#         h(t|dose) = h0(t) exp(beta1dose)

essai_n18$event <- ifelse(is.na(essai_n18$toxicity.study.time), 0, 1)

# modÈlisation 
library("survival")

fit <- coxph(formula = Surv(toxicity.study.time, event == 1) ~ dose, data = essai_n18)

# l'attente de temps que le patient aura survÈcu en fonction des facteurs prÈdictifs.(ici la dose)
predict(fit, type = "expected")

# probabilitÈs cumulÈes de dÈcËs pour chaque individu pour les doses spÈcifiÈes
predict(fit, type = "risk")

# probabilitÈ de survie pour chaque individu
predict(fit, type = "survival")

# objectif : estimer l'effet de chaque dose sur l'incidence de "guÈrison"
# idÈe peut Ítre : On peut soustraire les probabilitÈs cumulÈes de dÈcËs des probabilitÈs totales(Kaplan-Meier.),
# ensuite comparer ces probabilitÈs de chaque ???