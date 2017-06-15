prob4 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0504.txt",header = TRUE)
attach(prob4)
lm.out=lm(Surface~factor(Depth)*factor(Feed))
summary(lm.out)
(a)
summary(aov(Surface~factor(Depth)*factor(Feed)))
(b)
par(mfcol=c(2,2))
plot(aov(Surface~factor(Depth)*factor(Feed)))
(c)
interaction.plot(Depth,Feed,Surface,type="b")
interaction.plot(Feed,Depth,Surface,type="b")

(d)HSD
1))))))
qt.val=qtukey(0.95,3,24)
se.diff=sqrt(28.7*2/3)
HSD=qt.val/sqrt(2)*se.diff
c(qt.val,se.diff,HSD)

TukeyHSD(aov(Surface~factor(Depth)*factor(Feed)))

3))))
qt.val=qtukey(0.95,4,24)
se.diff=sqrt(28.7*2/3)
HSD=qt.val/sqrt(2)*se.diff
c(qt.val,se.diff,HSD)

(f)power
ybars=c(84.77778,89.77778,97.88889,104.88889)
a=4
b=3
n=3
mse=28.7
df.num=a-1
df.den=a*b*(n-1)
(ncp=b*n*sum((ybars-mean(ybars))^2)/mse)
(fcrit=qf(0.05,df.num,df.den,lower.tail=FALSE))
(power=1-pf(fcrit,df.num,df.den,ncp))

(g) 
a=3
b=4
n=4
D=8
mse=28.7
df.num=b-1
df.den=a*b*(n-1)
ncp=a*n*D^2/(2*mse)
fcrit=qf(0.05,df.num,df.den,lower.tail=FALSE)
(power=1-pf(fcrit,df.num,df.den,ncp))


****************2***********************
prob16 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0516.txt",header = TRUE)
attach(prob16)

fit.stre=aov(Strength~factor(Pressure)+factor(Temperature))
summary(fit.stre)


nonadd=predict(fit.stre)^2
anova(update(fit.stre,Strength~factor(Pressure)+factor(Temperature)+nonadd))

(d)
interaction.plot(Pressure,Temperature,Strength)

(e)
par(mfcol=c(2,2))
plot(aov(fit.stre))

(f)
TukeyHSD(aov(Strength~factor(Pressure)+factor(Temperature)))

TukeyHSD(aov(Strength~factor(Temperature)+factor(Pressure)))



****************3***********************
prob18 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0518.txt",header = TRUE)
attach(prob18)

summary(aov(Strength~factor(Hardwood)*factor(Cooking)*factor(Pressure)))

(b)
par(mfcol=c(2,2))
plot(aov(Strength~factor(Hardwood)*factor(Cooking)*factor(Pressure)))


(d)
with(prob18,interaction.plot(Hardwood,Pressure, Strength))