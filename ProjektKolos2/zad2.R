x=Ankieta2023$`Przeciętna liczba godzin spędzanych przy komputerze w ciągu doby (praca, nauka lub rozrywka)`
#a) Test Shapiro-Wilka dla normalności rozkładu
test_normalnosci=shapiro.test(x)
#Wyświetlenie wyników testu Shapiro-Wilka
test_normalnosci$statistic
# Im większa wartość statystyki testowej, tym mocniejsze są
#dowody przemawiające przeciwko założeniu o normalności 
#rozkładu danych.
test_normalnosci$p.value
#Jeśli p-value jest mniejsze niż poziom istotności
# to można odrzucić hipotezę zerową na rzecz hipotezy 
#alternatywnej.
if (test_normalnosci$p.value < 0.01) {
  cat("Rozkład danych nie jest normalny na poziomie istotności 0,01\n")
} else {
  cat("Rozkład danych jest normalny na poziomie istotności 0,01\n")
}
#b) Przedziały ufności dla średniej
przedzial_ufnosci_srednia=t.test(x, conf.level = 0.95)$conf.int
przedzial_ufnosci_srednia


cat("\nPrzedział ufności dla średniej (poziom ufności 0,95):\n")
cat("Dolny przedział:", przedzial_ufnosci_srednia[1], "\n")
cat("Górny przedział:", przedzial_ufnosci_srednia[2], "\n")

#c) Przedział ufności dla odchylenia standardowego
przedzial_ufnosci_odchylenie=qchisq(c(0.05/2, 1-0.05/2), df = length(x)-1) * sqrt(var(x)) / sqrt(length(x))
przedzial_ufnosci_odchylenie

cat("\nPrzedział ufności dla odchylenia standardowego (poziom ufności 0,9):\n")
cat("Dolny przedział:", przedzial_ufnosci_odchylenie[1], "\n")
cat("Górny przedział:", przedzial_ufnosci_odchylenie[2], "\n")
#d) Test jednostronny dla średniej
test_srednia=t.test(x, alternative = "greater", mu = 8)
test_srednia$statistic
#Statystyka testowa
test_srednia$p.value
#P-wartość
if (test_srednia$p.value < 0.05) {
  cat("Hipoteza alternatywna: średnia jest większa niż 8\n")
} else {
  cat("Hipoteza zerowa: średnia nie jest większa niż 8\n")
}
#e) Test proporcji
test_proporcji=prop.test(sum(x < 4), n = length(x), p = 0.25, alternative = "two.sided", conf.level = 0.95)

test_proporcji$statistic
#Statystyka testowa

if (test_proporcji$p.value < 0.05) {
  cat("Hipoteza alternatywna: Odsetek studentów, dla których X jest mniejsza od 4, nie jest równy 25%\n")
} else {
  cat("Hipoteza zerowa: Odsetek studentów, dla których X jest mniejsza od 4, jest równy 25%\n")
}