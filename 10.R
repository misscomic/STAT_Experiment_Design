#####################1

#1
p0815 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0815.txt",
header = TRUE)
p0815$A <- ifelse(p0815$Time == 2.5, -1, 1)
p0815$B <- ifelse(p0815$Concentration == 14, -1, 1)
p0815$C <- ifelse(p0815$Pressure == 60, -1, 1)
p0815$D <- ifelse(p0815$Temperature == 225, -1, 1)
p0815

summary(lm(Yield~A*B*C*D,p0815))

alias(lm(Yield~A*B*C*D,p0815))

library(gplots)
qqnorm(aov(Yield~A*B*C*D,p0815),label=TRUE)

#2
p0809 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0809.txt",
header = TRUE)
p0809$A=p0809$Solvent
p0809$B=p0809$Catalyst
p0809$C=p0809$Temperature
p0809$D=p0809$Purity
p0809$E=p0809$pH

summary(lm(Color~A*B*C*D*E,p0809))

alias(lm(Color~A*B*C*D*E,p0809))

qqnorm(aov(Color~A*B*C*D*E,p0809),label=TRUE)


#3
p0852 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0852.txt",
header = TRUE)
alias(lm(Gain~A*B*C*D*E*F,p0852))
