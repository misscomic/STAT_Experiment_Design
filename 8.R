1
prob20 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0620.txt",header = TRUE)
prob20$C<- ifelse(prob20$Carbonation == 10, -1, 1)
prob20$P<- ifelse(prob20$Pressure == 25, -1, 1)
prob20$S<- ifelse(prob20$Speed == 200, -1, 1)
attach(prob20)

(b)
summary(aov(Deviation~C*P*S))

(c)
par(mfrow=c(2,2))
plot(aov(Deviation~C*P*S))

coded <- function(x) ifelse(x == "-", -1, 1)
sapply(list(C,P,S,C*P,C*S,P*S,C*P*S),
function(x)
c(low=mean(Deviation[x==-1]),
high=mean(Deviation[x==1]),
effect=2*mean(x * Deviation)))

summary(lm(Deviation~C*P*S))



(d)half-normal

library(gplots)
qqnorm(aov(Deviation~C*P*S),label=TRUE)

(e)
summary(aov(Deviation~C+P+S+C:P))
summary(lm(Deviation~C+P+S+C:P))

summary(aov(Deviation~C+P+S+C:P+factor(C):factor(P):factor(S)))
par(mfrow=c(2,2))
plot(aov(Deviation~C+P+S+C:P))


2
prob22 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0622.txt",header = TRUE)
attach(prob22)
prob22$L=Laser
prob22$P=Pulse
prob22$C=Cell
prob22$W=Writing
attach(prob22)

lm.out=lm(UEC~L*P*C*W)
summary(lm.out)

(effs=2*coef(lm.out)[-1])

(d)
PSE= function(e) {
abseff = abs(e)
s0 = 1.5 * median(abseff)
1.5 * median(abseff[abseff < 2.5*s0])
}
PSE(effs)

cbind(effs, pseudo.t = effs/PSE(effs))

(e)
summary(aov(UEC~L+C+W))


3
prob23 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0623.txt",header = TRUE)
attach(prob23)
prob23$L=Laser
prob23$P=Pulse
prob23$C=Cell
prob23$W=Writing
attach(prob23)

(b)
lm.out2=lm(UEC~L*P*C*W)
summary(lm.out2)

( effs = 2*coef(lm.out2)[-1] )

PSE= function(e) {
abseff = abs(e)
s0 = 1.5 * median(abseff)
1.5 * median(abseff[abseff < 2.5*s0])
}
PSE(effs)
cbind(effs, pseudo.t = effs/PSE(effs))


(c)
lm.out2=lm(UEC~L*P*C*W+I(L^2)+I(P^2)+I(C^2)+I(W^2))


(d)
summary(aov(UEC~L+C+W+L:C+C:W))
