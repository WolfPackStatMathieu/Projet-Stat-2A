source("/users/schevret/documents/bibli/utils.r",encoding ="latin1")
require(survival)
require(survminer)
require(ggplot2)
require(flexsurvcure)

dose<-c(rep(1,3),rep(3,3),rep(4,4*3))
tdl <- c(rep(0,3),0,0,1,0,0,1,rep(0,3),0,1,0,1,0,1)

 
hht<- data.frame(cbind(dose,tdl)) 
hht$id<- 1:18
hht$temps <- 6
set.seed(123459)
hht$temps[hht$tdl==1]<- rexp(sum(hht$tdl==0),1)
table(hht$dose,hht$tdl)
table(hht$temps<6,hht$tdl)
head(hht)


sv <- survfit(Surv(temps,tdl)~1, data=hht)
sv.d <- survfit(Surv(temps,tdl)~dose, data=hht)
ggsurvplot(sv,rsk.table=T, break.times=1, , risk.table=T, conf.int = T)
ggsurvplot(sv.d,rsk.table=T, break.times=1, risk.table=T, conf.int = T)

est.brn <- sum(hht$tdl)/length(hht$tdl)
binom.test(x=sum(hht$tdl),n=length(hht$tdl))

est.km  <- 1-as.numeric(tp.surv(sv,6)[3])
est.km

est.km2  <- tp.surv(sv.d,6)
est.km2

f       <-flexsurvcure(Surv(temps,tdl)~1,data=hht, dist="weibull", mixture=T)
est.cure<- 1-exp(f$coefficients[1])/(1+exp(f$coefficients[1]))
est.cure
# c'est aussi donné  par 1- theta, qui rend la proortion de gueris directement
# donc l'ic est donné par  1-0.874, 


require(cuRe)
fit1<- fit.cure.model(Surv(temps,tdl)~1,
               formula.surv=list( ~1),
               type="mixture",dist="weibull",link="logit",
               data=hht)
summary(fit1)
predict(fit1,type="curerate")


fit.weib <- fit.cure.model(Surv(temps,tdl)~dose,
                           formula.surv=list(~dose, ~1),
                           type="mixture",dist="weibull",link="logit",
                           data=hht)

summary(fit.weib)
# The prefixes in the coefficient names indicate which term the coefficients are related to. For instance, the prefix pi indicates that the
# coefficient is related to the cure proportion (differences on the link-transformed scale)
# To obtain the cure proportion for a patient with specific covariates, 
# the predict function can be used. 
predict(fit.weib,data.frame(dose=c(1,2,3)),type="curerate")
beta<-as.numeric(unlist(fit.weib$coefs[1]))
1-plogis(beta[2]*c(1,2,3)+beta[1])
## pour avoir Pr TDL, on utilise le complementaire à 1
