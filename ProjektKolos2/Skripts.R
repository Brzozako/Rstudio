x=c(7.6,7.56,5.30,7.3,7.4,7.32,8.01,7.90,7.17,7.28,7.73,8,7.62,7.15,7.90,7.91,7.73,8.02,7.63,7.62)
y=c(1,1,1,1,1,2,1,2,2,2,2,2,2,1,1,1,2,1,2,1)
summary(x) #
sd(x) #odchylenie standardowe
skewness(x) #skosnosc
test=shapiro.test(x)
alfa=0.05
pval=shapiro.test(x)$p.value
ifelse(pval>alfa,"Nie mozna odrzucic H0, na poziomie istotnosci 0,05","Odrzucamy H0, na rzecz H1")
#jeżeli p-value>alfa, to brak podstaw do odrzucenia H0
#Wniosek: nie można odrzucić H0 na poziomie istotnosci 0.05, zmienne nie mają rozkładu normalnego



