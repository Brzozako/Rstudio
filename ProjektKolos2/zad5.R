#Dla danych z ramki danych AnkietaM zbadać na poziomie istotności α = 0.01 zależność pomiędzy
#ukończoną szkołą średnią a miejscem zamieszkania wśród studentów kierunku Informatyka.
#tablica_kontyngencji odnosi się do tablicy,
#która przedstawia rozkład lub liczbę przypadków
#dla zależności między ukończoną szkołą średnią
#a miejscem zamieszkania wśród studentów kierunku Informatyka.
#Tablica ta może zawierać liczby lub częstości wystąpień
#poszczególnych kombinacji kategorii obu zmiennych.

tablica_kontyngencji <- table(Ankieta2023$`Ukończona szkoła średnia`, Ankieta2023$`Miejsce zamieszkania podczas studiowania`)
wynik_testu <- chisq.test(tablica_kontyngencji)
wynik_testu
if (wynik_testu$p.value < 0.01) {
  cat("Istnieje istotna zależność pomiędzy ukończoną szkołą średnią a miejscem zamieszkania\n")
} else {
  cat("Brak istotnej zależności pomiędzy ukończoną szkołą średnią a miejscem zamieszkania\n")
}

