#1
#confidence interval for mean

kozy<-read.table(file="goats.txt", header = T)
waga <-kozy$WeightInitial
t.test(x=waga,conf.level=0.9)$conf.int
przedzial<-t.test(x=waga,conf.level=0.9)$conf
#2
n0<-length(waga)
wariancja<-var(waga)
odchylenie<- sd(waga)
q_t<-qt(0.95,n0-1)
d<-1/2
(q_t*odchylenie/d)^2
(ile_koz<-ceiling((q_t*odchylenie/d)^2)-n0)




#3 chi square

przedzial <- function(dane, poziom_ufnosci){
  dane<-na.omit(dane)
  n<-length(dane)
  s2<-var(dane)
  alfa <- 1-poziom_ufnosci
  q1<-qchisq(1-alfa/2,n-1)
  q2<-qchisq(alfa/2,n-1)
  prz<-(n-1)*s2/c(q1,q2)
  list(przedzial_war=prz,prz_odch=sqrt(prz))
}
przedzial(waga,0.99)

#4a

przedzial.sred<-function(dane,poziom.ufnosci){
  if(length(dane)<100) {print("BLAD za ma³o obserwacji")}
  else{
    na.omit(dane)
    alfa=1-poziom.ufnosci
    d<-qnorm(1-alfa/2)*sd(dane)/sqrt(length(dane))
    mean(dane)+c(-1,1)*d
  }
}
przedzial.sred(waga,0.99)



#4b
library(MASS)
duration<-geyser$duration
przedzial.sred(duration,0.95)

przedzial(duration,0.95)


#6 MODEL 1

#7
binom.test(21,25,conf.level = 0.9)$conf.int

?prop.test()
