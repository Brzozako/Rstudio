x=Ankieta2023$`Programowanie 1` #Ankieta2023$`Programowanie 1`=ifelse(is.na(Ankieta2023$`Programowanie 1`),2,Ankieta2023$`Programowanie 1`) do konsolki wkleić
srednia=mean(x)
mediana=median(x)
kwartyl1=quantile(x, 0.25)
kwartyl3=quantile(x, 0.75)
minimum=min(x)
maksimum=max(x)
rozstep_empiryczny=maksimum - minimum
rozstep_kwartylowy=kwartyl3 - kwartyl1
odchylenie_standardowe=sd(x)
wspolczynnik_zmiennosci=odchylenie_standardowe / srednia
wspolczynnik_asymetrii=skewness(x)
kurtoza=kurtosis(x)


srednia
mediana
kwartyl1
kwartyl3
minimum
maksimum
rozstep_empiryczny
rozstep_kwartylowy
odchylenie_standardowe
wspolczynnik_zmiennosci
wspolczynnik_asymetrii
kurtoza