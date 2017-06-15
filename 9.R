p0704 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0704.txt", header = TRUE)
 p0704
 summary(lm(Life~Block+Speed*Geometry*Angle,data=p0704))
 
 c(tapply(p0704$Life, p0704$Speed, mean),
eff = diff(tapply(p0704$Life, p0704$Speed, mean)))
 
 c(tapply(p0704$Life, p0704$Speed*p0704$Geometry, mean),
   eff = diff(tapply(p0704$Life, p0704$Speed*p0704$Geometry, mean)))
 
 
 library(gplots)
 qqnorm(aov(Life~Block+Speed*Geometry*Angle,data=p0704), label = TRUE)
 
 p0724 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/bookproblem0724.txt", header = TRUE)
 p0724
 # Use ABC to determine the 2 blocks in Rep1
  p0724Rep1 <- p0724[p0724$Rep == 1,]
  A <- p0724Rep1$Speed
  B <- p0724Rep1$Geometry
  C <- p0724Rep1$Angle
 p0724Rep1$Block <- ifelse(A * B * C < 0, 1, 2)
 p0724Rep1 <- p0724Rep1[order(p0724Rep1$Block),]
 # Use AB to determine the 2 blocks in Rep2
  p0724Rep2 <- p0724[p0724$Rep == 2,]
  A <- p0724Rep2$Speed
  B <- p0724Rep2$Geometry
  p0724Rep2$Block <- ifelse(A * B > 0, 1, 2)
  p0724Rep2 <- p0724Rep2[order(p0724Rep2$Block),]
  # Use BC to determine the 2 blocks in Rep3  
  p0724Rep3 <- p0724[p0724$Rep == 3,]

  B <- p0724Rep3$Geometry
  C <- p0724Rep3$Angle
  p0724Rep3$Block <- ifelse(C* B > 0, 1, 2)
  p0724Rep3 <- p0724Rep3[order(p0724Rep3$Block),]
 partialConfounding <- rbind(p0724Rep1, p0724Rep2,p0724Rep3)
  partialConfounding
  
  summary(aov(Life ~ factor(Block):factor(Rep) + Speed* Geometry* Angle,
              partialConfounding))
  
  partialConfounding$Blocks <- factor(paste(partialConfounding$Rep,partialConfounding$Block, sep = "-"))
  summary(lm(Life ~ Blocks + Speed* Geometry* Angle,
             partialConfounding))

   library(car)
   vif(lm(Life ~ Blocks + Speed* Geometry* Angle,
           partialConfounding))
   