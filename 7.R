1
prob10 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0510.txt",header = TRUE)
attach(prob10)
(b)
interaction.plot(Temperature,Glass,Light,type="b")
summary(aov(Light~factor(Temperature)*factor(Glass)))

(d)
1
summary(aov(Light~factor(Glass)*(Temperature+I(Temperature^2))))

2
lm.out=lm(Light~factor(Glass)*(Temperature+I(Temperature^2)))
summary(lm.out)

3
ngrid=20
tg=seq(min(Temperature), max(Temperature),length = ngrid)
grid=expand.grid(Temperature=tg,Glass=levels(factor(Glass)))
yhat<-predict(lm.out,grid)
yhat<-matrix(yhat,nrow=length(tg))
matplot(tg,yhat,type="l",xlab="Temperature",lwd=3)
abline(v=c(100,125,150),lty=2)
legend("bottomright",legend=paste("Glass",levels(factor(Glass))),lty=1:3,col=1:3,lwd=3)

************************************
2
prob25 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0525.txt",header = TRUE)
attach(prob25)

(b)
interaction.plot(Temperature,Doping,Current,type="b")

(c)
summary(aov(Current~factor(Temperature)*factor(Doping)))

(d)
1
lm.out=lm(Current~Doping*Temperature)
summary(lm.out)

2.lack of fit
summary(aov(Current~Doping+Temperature+Doping:Temperature+
factor(Doping):factor(Temperature)))


(e)
1
(c)(   summary(aov(Current~factor(Temperature)*factor(Doping)))       )

summary(aov(Current~Doping+Temperature+I(Temperature^2)+
Doping:(Temperature+I(Temperature^2))))


2
lm.out=lm(Current~Doping+Temperature+I(Temperature^2)+
Doping:(Temperature+I(Temperature^2)))
summary(lm.out)


3

ngrid=20
tp=seq(min(Temperature), max(Temperature),length = ngrid)
dp=seq(min(Doping), max(Doping),length = ngrid)
grid=expand.grid(Temperature=tp,Doping=dp)
yhat<-predict(lm.out,grid)
yhat<-matrix(yhat,length(tp),length(dp))
persp(tp,dp,yhat,theta=-45,expand=0.75,ticktype="detailed")


***********************
3

prob5 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0605.txt",header = TRUE)
attach(prob5)

summary(aov(Vibration~Bit*Speed))

c
interaction.plot(Bit, Speed, Vibration)

d
Vib=Vibration
coded <- function(x) ifelse(x == "-", -1, 1)
sapply(list(Bit,Speed,Bit*Speed),function(x)
c(low = mean(Vib[x==-1]),
high = mean(Vib[x==1]),
effect = mean(Vib[x==1])
- mean(Vib[x==-1]) ) )


e
viblm <- lm(Vibration ~ Bit * Speed)
par(mfcol = c(1, 2))
library(rsm)
persp(viblm,Speed~Bit,
theta = -40, phi = 30, ticktype = "d",
col=rainbow(50), contour="color")
contour(viblm,Speed~Bit, image=TRUE)
















