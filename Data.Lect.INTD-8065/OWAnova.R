
cuckoos.frm <- read.csv("cuckoosred.csv")
names(cuckoos.frm)
attach(cuckoos.frm)
class(egg.length)
class(host.species)
summary(egg.length)
summary(host.species)
lapply(split(egg.length,host.species),summary)
lapply(split(egg.length,host.species),sd)
plot(host.species,egg.length)

cuckoos.mod1 = aov(egg.length~host.species)
summary(cuckoos.mod1)

anova(lm(egg.length~host.species))

levels(host.species)
coef(cuckoos.mod1)
coef(cuckoos.mod1)[2:5]+23.12142857

options(contrasts=c("contr.sum","contr.poly"))
cuckoos.mod2 = aov(egg.length~host.species)
coef(cuckoos.mod2)
coef(cuckoos.mod2)[2:5]+22.56395238
22.56395238-sum(coef(cuckoos.mod2)[2:5])

pairwise.t.test(egg.length,host.species,p.adjust.method="none")

pairwise.t.test(egg.length,host.species,p.adjust.method="bonferroni")

pairwise.t.test(egg.length,host.species,p.adjust.method="holm")

pairwise.t.test(egg.length,host.species,p.adjust.method="hochberg")

TukeyHSD(cuckoos.mod1)

plot(TukeyHSD(cuckoos.mod1))
