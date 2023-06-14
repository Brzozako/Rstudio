#Zbadać współzależność zmiennych Liczba godzin spędzanych przy komputerze i Liczba punktów
#ECTS z danych AnkietaM:
#a) na poziomie istotności 0.05 sprawdzić za pomocą testu Shapiro-Wilka założenie o normalności
#rozkładu obu zmiennych;
#b) wyznaczyć współczynnik korelacji Pearsona lub rang Spearmana wraz z testem istotności tego
#współczynnika na poziomie istotności 0.05;
#c) jeżeli rozkłady obu zmiennych są normalne wyznaczyć równanie regresji liniowej.

liczba_godzin=Ankieta2023$`Przeciętna liczba godzin spędzanych przy komputerze w ciągu doby (praca, nauka lub rozrywka)`
liczba_punktow_ects = Ankieta2023$`Liczba punktów ECTS uzyskanych w pierwszym semestrze`

#Test Shapiro-Wilka dla zmiennej "Liczba godzin"
test_normalnosci_godziny <- shapiro.test(liczba_godzin)
if (test_normalnosci_godziny$p.value < 0.05) {
  cat("Rozkład zmiennej 'Liczba godzin' nie jest normalny na poziomie istotności 0.05\n")
} else {
  cat("Rozkład zmiennej 'Liczba godzin' jest normalny na poziomie istotności 0.05\n")
}

#Test Shapiro-Wilka dla zmiennej "Liczba punktów ECTS"
test_normalnosci_ects <- shapiro.test(liczba_punktow_ects)
if (test_normalnosci_ects$p.value < 0.05) {
  cat("Rozkład zmiennej 'Liczba punktów ECTS' nie jest normalny na poziomie istotności 0.05\n")
} else {
  cat("Rozkład zmiennej 'Liczba punktów ECTS' jest normalny na poziomie istotności 0.05\n")
}
#b)
#Korelacja Pearsona
korelacja_pearson <- cor(liczba_godzin, liczba_punktow_ects, method = "pearson")
cat("Współczynnik korelacji Pearsona:", korelacja_pearson, "\n")

#Test istotności współczynnika korelacji Pearsona
test_istotnosci_pearson <- cor.test(liczba_godzin, liczba_punktow_ects, method = "pearson")
if (test_istotnosci_pearson$p.value < 0.05) {
  cat("Współczynnik korelacji Pearsona jest istotny na poziomie istotności 0.05\n")
} else {
  cat("Współczynnik korelacji Pearsona nie jest istotny na poziomie istotności 0.05\n")
}

#Korelacja rang Spearmana
korelacja_spearman <- cor(liczba_godzin, liczba_punktow_ects, method = "spearman")
cat("Współczynnik korelacji rang Spearmana:", korelacja_spearman, "\n")

#Test istotności współczynnika korelacji rang Spearmana
test_istotnosci_spearman <- cor.test(liczba_godzin, liczba_punktow_ects, method = "spearman")
if (test_istotnosci_spearman$p.value < 0.05) {
  cat("Współczynnik korelacji rang Spearmana jest istotny na poziomie istotności 0.05\n")
} else {
  cat("Współczynnik korelacji rang Spearmana nie jest istotny na poziomie istotności 0.05\n")
}

#c)
#Sprawdzenie założenia o normalności rozkładu obu zmiennych
if (test_normalnosci_godziny$p.value >= 0.05 & test_normalnosci_ects$p.value >= 0.05) {
  # Wykonanie regresji liniowej
  model_regresji <- lm(liczba_punktow_ects ~ liczba_godzin)
  summary(model_regresji)  # Wyświetlenie podsumowania modelu
  
  #Wypisanie równania regresji liniowej
  a <- coef(model_regresji)[1]
  b <- coef(model_regresji)[2]
  cat("Równanie regresji liniowej: Liczba punktów ECTS =", a, "+", b, "* Liczba godzin\n")
} else {
  cat("Nie można wyznaczyć równania regresji liniowej, ponieważ rozkłady zmiennych nie są normalne.\n")
}

