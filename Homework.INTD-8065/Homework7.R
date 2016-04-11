#Homework 7
## Interpret the coefficients of the selected model for the prostate cancer data 
## in terms of odds. You can follow the analysis in the rotavirus vaccine example.

# Rotavirus

rota.pos=c(22,48,39,94)
totals=c(382,721,407,675)
rota.neg=totals-rota.pos
vaccine.fac=gl(2,2,labels=c("vaccine","placebo"))
graffar.fac = gl(2,1,4,labels=c("2-3","4-5"))

vaccine.mod1=glm(cbind(rota.pos,rota.neg)~vaccine.fac+graffar.fac,family=binomial)
summary(vaccine.mod1)

cbind(as.character(vaccine.fac),as.character(graffar.fac),
      + round(predict(vaccine.mod1,type="response"),4))

# Prostate 

prostate.frm=read.table("prostate.txt",header=T)
attach(prostate.frm)
prostate.mod1=glm(Nodes~Xray+Grade+Stage+Age+Acid,family=binomial)
summary(prostate.mod1)

1-pchisq(48.126,47)

drop1(prostate.mod1,test="Chisq")

prostate.mod2=glm(Nodes~Xray+Stage+Age+Acid,family=binomial)
summary(prostate.mod2)

1-pchisq(49.097,48)

drop1(prostate.mod2,test="Chisq")

prostate.mod3=glm(Nodes~Xray+Stage+Acid,family=binomial)
summary(prostate.mod3)


1-pchisq(50.660,49)

drop1(prostate.mod3,test="Chisq")

prostate.mod4=glm(Nodes~Xray+Stage,family=binomial)
summary(prostate.mod4)

1-pchisq(53.353,50)

anova(prostate.mod4,prostate.mod1,test="Chisq")

predict(prostate.mod4,data.frame(Xray=c(0,0,1,1),
                                 + Stage=c(0,1,0,1)),type="response")

