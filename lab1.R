
#------------Zadanie 1-----------------
# x czas przejazdu
ex<- 17
varx<-4
pnorm(18,ex,sqrt(varx))
?pnorm
pnorm(17.5, ex,sqrt(varx), lower.tail=FALSE)
pnorm(17, ex,sqrt(varx)) - pnorm(16.5, ex,sqrt(varx))
#??nor

#b)
qnorm(0.9,ex, sqrt(varx))

#c)
rnorm(5,ex,sqrt(varx))

#--------------Zadanie 2 ----------------
#a) binom X liczba znalezionych stworkow typu A binom(5,0.2)
?qbinom

dbinom(0:5,5, 0.2)
pbinom(2, 5, 0.2, lower.tail=FALSE)
pbinom(3, 5, 0.2)

x=c()
for (i in 3:30){
  x[i]=pbinom(1,i,0.2, lower.tail=FALSE)
}

(n= min(which(x>=0.9)))
x


#c 100 typu C binom(100, 0.01)
pbinom(0, 100, 0.01, lower.tail=FALSE)
ppois(0,1,lower.tail=FALSE)



#--------------Zadanie 3 ----------------
lambda <- 8
dpois(7,lambda)
ppois(3,lambda)
ppois(6, lambda)
ppois(10,lambda, lower.tail=F)
ppois(9, lambda)-ppois(3, lambda)
qpois(0.95,lambda)

#--------------Zadanie 4 ----------------
library(MASS)
data(Cars93)
?Cars93

head(Cars93)
dim(Cars93)
Cars93$Type
#atach
attach(Cars93)
#b)
Cars83$zuzycie_paliwa

zp.highway=380/(MPG.highway*1.6)
zp.city=380/(MPG.city*1.6)
Cars93$zp.highway=380/(MPG.highway*1.6)
Cars93$zp.city=380/(MPG.city*1.6)
dolar<-4.44
zp.price=4.44*Min.Price
Cars93$zp.price=4.44*Min.Price
summary(zp.price)
boxplot(zp.price)
q95<-quantile(zp.price, 0.95)
?quantile
zp.price[zp.price>q95]
paste(Model[zp.price>q95], Manufacturer[zp.price>q95])
Model[zp.price>q95]
boxplot(zp.city~Origin)
