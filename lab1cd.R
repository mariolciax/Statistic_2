dane <- c(483, 705, 2623, 347, 620, 2719, 1035, 421)
esty<-1/mean(dane) #jesli pamietamy z wykladu 
library(MASS)

#ESTYMATOR NW! to druggie to odchylenie
fitdistr(dane, "exponential")
estymator_nw<-fitdistr(dane, densfun="exponential")$est

#ENW funkcja=funkcja ENW
#enw(EX)=mean(X)
e1<- mean(dane)
e1

 
#enw(P(X<1000))=1-exp(-enw(lambda)*1000)

e2<-1-exp(-(estymator_nw*1000))

e2


#6

?rgamma
(probka<- rgamma(10000,3,2))



fitdistr(probka,"gamma",lower=c(0,0))
fitdistr(probka,"gamma",lower=c(0,0))$est
