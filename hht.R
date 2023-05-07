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
glm_pred<-glm(data=hht,tdl~dose)
confint(glm_pred)
fit.weib <- fit.cure.model(Surv(temps,tdl)~dose,
                           formula.surv=list(~dose, ~1),
                           type="mixture",dist="weibull",link="logit",
                           data=hht)

summary(fit.weib)

1-predict(fit_weib2,type="curerate",data.frame(dose=c(1,3,4)))
# The prefixes in the coefficient names indicate which term the coefficients are related to. For instance, the prefix pi indicates that the
# coefficient is related to the cure proportion (differences on the link-transformed scale)
# To obtain the cure proportion for a patient with specific covariates, 
# the predict function can be used. 
gueri_pred<-predict(fit.weib,data.frame(dose=c(1,3,4)),type="curerate")
beta<-as.numeric(unlist(fit.weib$coefs[1]))



######### dose en tant qu'indicatrice.#####
fit_weib2<-fit.cure.model(Surv(temps,tdl)~as.factor(dose),
                          formula.surv=list(~as.factor(dose), ~1),
                          type="mixture",dist="weibull",link="logit",
                          data=hht)
fit_weib2$coefs
#beta<-as.numeric(unlist(fit.weib$coefs))
beta0<-as.numeric(fit_weib2$coefs[1]$'1')[1]
nb_doses<-3
reste_beta<-as.numeric(fit_weib2$coefs[1]$'1')[c(2:nb_doses)]
coeffs<-beta0+c(0,reste_beta)
pred_cure_indicator<-1-plogis(coeffs)

fit_glm_ind<-glm(data=hht,tdl~as.factor(dose))
pred_glm_ind<-fitted(glm_pred)[c(1,4,7)]

## pour avoir Pr TDL, on utilise le complementaire à 1
#Estimateurs##
#sans dose#
pred_km_s<-est.km


#avec dose en numérique
pred_km<-c(0,0.33,0.33)
pred_cure<-1-plogis(beta[2]*c(1,3,4)+beta[1])
pred_glm<-fitted(glm_pred)[c(1,4,7)]

####
vect<-c(1,2,3)
data_graph<-cbind.data.frame(vect,pred_cure,pred_glm,pred_km)
colnames(data_graph)<-c("indice_dose","cure","glm","KM")
require(ggplot2)
ggplot(data = data_graph,aes(x=indice_dose,y=cure,col="guerison"))+geom_point(shape=15,size=7)+
  geom_point(shape=16,size=7,aes(y=glm,col="glm"))+
  geom_point(shape=17,size=7,aes(y=KM,col="KM"))+
  scale_color_manual(name = "Modeles", values = c("guerison" = "red1", "KM" = "darkgreen","glm"="blue")) +
  xlab("Indice de la dose ")+ylab("Proportion de gueris prédite")+ggtitle("Prédictions selon les différents modèles")

#avec dose en indicatrice. 
vect<-c(1,2,3)
data_graph_ind<-cbind.data.frame(vect,pred_cure_indicator,pred_glm_ind,pred_km)
colnames(data_graph_ind)<-c("indice_dose","cure","glm","KM")
ggplot(data = data_graph_ind,aes(x=indice_dose,y=cure,col="guerison"))+geom_point(shape=15,size=7)+
  geom_point(shape=16,size=7,aes(y=glm,col="glm"))+
  geom_point(shape=17,size=7,aes(y=KM,col="KM"))+
  scale_color_manual(name = "Modeles", values = c("guerison" = "red1", "KM" = "darkgreen","glm"="blue")) +
  xlab("Indice de la dose ")+ylab("Proportion de gueris prédite")+ggtitle("Prédictions selon les différents modèles (avec indicatrices)")
