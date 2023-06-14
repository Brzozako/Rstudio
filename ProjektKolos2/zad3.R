liczba_godzin=Ankieta2023$`Przeciętna liczba godzin spędzanych przy komputerze w ciągu doby (praca, nauka lub rozrywka)`
szkola_srednia=Ankieta2023$`Ukończona szkoła średnia`
#a) Sprawdzenie założenia o normalności rozkładu Liczba godzin testem Shapiro-Wilka
grupowane_dane <- split(liczba_godzin, szkola_srednia)

for (grupa in names(grupowane_dane)) {
  test_normalnosci <- shapiro.test(grupowane_dane[[grupa]])
  
  if (test_normalnosci$p.value < 0.01) {
    cat("Rozkład danych w grupie", grupa, "nie jest normalny na poziomie istotności 0,01\n")
  } else {
    cat("Rozkład danych w grupie", grupa, "jest normalny na poziomie istotności 0,01\n")
  }
}

by(liczba_godzin, szkola_srednia,shapiro.test)

#b) Test Bartletta dla jednorodności wariancji
test_bartletta <- bartlett.test(liczba_godzin, szkola_srednia)

if (test_bartletta$p.value < 0.02) {
  cat("Założenie o jednorodności wariancji jest spełnione na poziomie istotności 0,02\n")
} else {
  cat("Założenie o jednorodności wariancji nie jest spełnione na poziomie istotności 0,02\n")
}
#c) Analiza wariancji (ANOVA)
wyniki_anova <- aov(liczba_godzin ~ szkola_srednia)
test_hipotezy <- summary(wyniki_anova)
test_hipotezy

#p-wartość mniejsza od α wskazuje na istotność statystyczną.

#Analizując wyniki, warto zwrócić uwagę na p-wartość (Pr(>F))
#dla efektu szkoły średniej. W tym przypadku p-wartość wynosi 0.41,
#co oznacza, że nie ma istotnych statystycznie różnic w średniej
#liczbie godzin spędzanych przy komputerze między
#różnymi szkołami średnimi na poziomie istotności α = 0.01.
#d) Test Tukey'a i wykres różnic między średnimi
    posthoc_tukey = TukeyHSD(wyniki_anova)
    posthoc_tukey
    plot(posthoc_tukey)
    
    