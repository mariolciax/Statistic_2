library(MASS)
data(Cars93)
attach(Cars93)

# a)
?Cars93

# b)
pali_miasto=380/(1.6*MPG.city)
pali_miasto

pali_autostrada=380/(1.6*MPG.highway)
pali_autostrada

waga=0.4536*Weight
waga

cena=4.47*Min.Price   # Trzeba zmienic cene USD na aktualn¹
cena

# c)
summary(cena)
sd(cena)
quantile(cena,0.95)   # 154.1256

# d)
cena[cena>quantile(cena,0.95)]
Model[cena>quantile(cena,0.95)]
Manufacturer[cena>quantile(cena,0.95)]
paste(Manufacturer[cena>quantile(cena,0.95)],Model[cena>quantile(cena,0.95)],cena[cena>quantile(cena,0.95)])

# e) Wykresy skrzynkowe spalania paliwa w miescie osobno dla amerykanskich i 
# nieamerykanskich.


# same obrazki:
boxplot(pali_miasto~Origin)
# obrazki plus opis pude³ek
b=boxplot(pali_miasto~Origin)$stats
b
# f)

# histogram czestosci
boxplot(Weight~Origin)
h <-hist(Weight, freq=F,br=10)
h
#br - sugerowana liczba klas histogramu, R jednak wie lepiej i faktyczna jest 
#tylko zbli¿ona do naszej sugestii
# jadrowy estymator gestosci
lines(density(Weight),col="red")

# g) 
Type
licznosci=summary(Type)

# Wykres slupkowy

barplot(licznosci,col=c("red","green","gray","blue","yellow","pink"))
barplot(licznosci,col=c("red","green","gray","blue","yellow","pink"),
        legend.text =licznosci)
?barplot
# Wykres kolowy
pie(licznosci,labels=paste(names(licznosci),",",licznosci))

detach(Cars93)
