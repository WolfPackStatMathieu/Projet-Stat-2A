# Import package
library("dfcrm")

# construction de la base de données
dose <- c(rep(0.5,3),rep(3,3), rep(5,3), rep(5,3), rep(5,3),rep(5,3))
dosesWm <- c(0.05, 0.1, 0.15, 0.33, 0.50)
cibleP <- 0.33
reponse <- c(rep(0,3), 0,0,1, 0,0,1, rep(0,3), 0,1,0, 1,0,1)
N <- 18

df <- cbind.data.frame(dose, reponse)

crm(prior = dosesWm, target = cibleP, tox = reponse, level = dose, n = N)

?crm()
