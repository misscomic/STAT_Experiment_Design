prob23 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0423.txt",header = TRUE)
attach(prob23)
summary(aov(Time~factor(Order)+factor(Operator)+Method))

(d)
summary(aov(Time~factor(Operator)+Method))
summary(aov(Time~factor(Order)+Method))

summary(aov(Time~Method))

(e)
par(mfcol=c(2,2))
plot(aov(Time~factor(Order)+factor(Operator)+Method))

lm.out=lm(Time~factor(Order)+factor(Operator)+Method)
nonadd=predict(lm.out)^2
anova(update(lm.out,Time~factor(Order)+factor(Operator)+Method+nonadd))

(f)LSD
library(lsmeans)
lsmeans(lm.out,pairwise~Method,adjust="none")

crit.val=qt(0.975,6)
SE.diff=sqrt(1.750*(1/4+1/4))
(LSD=crit.val*SE.diff)

(g)HSD
lsmeans(lm.out,pairwise~Method)

crit.val=sqrt(.5)*(qtukey(.95,4,6))
SE.diff=sqrt(1.750*(1/4+1/4))
(HSD=crit.val*SE.diff)

(h)random factors
library(lme4)
lmer.out=lmer(Time~(1|Order)+(1|Operator)+Method)
summary(lmer.out)

	(3)LSD
lsmeans(lmer.out,pairwise~Method,adjust="none")

	(4)HSD
lsmeans(lmer.out,pairwise~Method)

(i)
probCopy=prob23
probCopy["1","Time"]<-NA
probCopy["14","Time"]<-NA
attach(probCopy)
summary(aov(Time~factor(Order)+factor(Operator)+Method))

(j)
	(1)
df.ls=(4-1)*(4-2)
df.rcbd=(4-1)*(4-1)
df.crd=4*(4-1)
ms.order=6.167
ms.operator=17.167
mse=1.75
(var.rcbd=(ms.order+(4-1)*mse)/4)
(RE.ls.rcbd=((df.ls+1)/(df.ls+3))*((df.rcbd+3)/(df.rcbd+1))*(var.rcbd/mse))
	(2)
df.ls=(4-1)*(4-2)
df.rcbd=(4-1)*(4-1)
df.crd=4*(4-1)
ms.order=6.167
ms.operator=17.167
mse=1.75
(var.rcbd=(ms.operator+(4-1)*mse)/4)
(RE.ls.rcbd=((df.ls+1)/(df.ls+3))*((df.rcbd+3)/(df.rcbd+1))*(var.rcbd/mse))
	(3)
(var.crd=(ms.order+ms.operator+(4-1)*mse)/(4+1))
(RE.ls.crd=((df.ls+1)/(df.ls+3))*((df.crd+3)/(df.crd+1))*(var.crd/mse))

2
(b)
prob36 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0436.txt",header = TRUE)
attach(prob36)
summary(aov(Time~factor(Order)+factor(Operator)+Workplace+Method))


3
(a)
prob22 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0422.txt",header = TRUE)
attach(prob22)
summary(aov(Time~factor(Batch)+factor(Day)+Catalyst))

(c)
df.ls=(5-1)*(5-2)
df.crd=5*(5-1)
ms.batch=3.86
ms.day=3.06
mse=3.13
(var.crd=(ms.batch+ms.day+(5-1)*mse)/(5+1))
(re.ls.crd=((df.ls+1)/(df.ls+3))*((df.crd+3)/(df.crd+1))*(var.crd/mse))





