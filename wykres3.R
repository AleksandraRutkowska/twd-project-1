
# Instalacja pakietow (wystarczy zrobic raz)
install.packages("readxl")
install.packages("ggplot2")

# Wczytanie pakietow
library(readxl)
library(ggplot2)

# Wczytanie danych z pliku Excel
dane <- read_excel(
  "Astma_aktualizacja_2023.xlsx",
  sheet = "Wykres 2.3",
  skip = 2
)

# Nadanie nazw kolumnom
names(dane) <- c("Wojewodztwo", "Chorobowosc")

# Usuniecie pustych wierszy
dane <- na.omit(dane)

# Uporzadkowanie wojewodztw wedlug wartosci
dane$Wojewodztwo <- factor(
  dane$Wojewodztwo,
  levels = dane$Wojewodztwo[order(dane$Chorobowosc)]
)

# Utworzenie wykresu slupkowego
ggplot(dane, aes(x = Wojewodztwo, y = Chorobowosc)) +
  geom_col(fill = "lightpink") +
  coord_flip() +
  labs(
    title = "Chorobowość na astmę według województw w 2023 roku",
    x = "Województwo",
    y = "Chorobowość na 1 tys. ludności"
  ) +
  theme_minimal()

# Zapisanie wykresu do pliku PNG
ggsave(
  "wykres_astma_wojewodztwa.png",
  width = 10,
  height = 7,
  dpi = 300
)