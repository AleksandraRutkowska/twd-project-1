library(readxl)
library(dplyr)
library(ggplot2)

df_astma <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.3", skip = 1) %>%
  select(Wojewodztwo = 1, Chorobowosc = 2) %>%
  filter(!is.na(Chorobowosc), !grepl("Źródło", Wojewodztwo)) %>%
  mutate(Chorobowosc = as.numeric(Chorobowosc))

df_smog <- data.frame(
  Wojewodztwo = c("Dolnośląskie", "Kujawsko-pomorskie", "Lubelskie", "Lubuskie",
                  "Łódzkie", "Małopolskie", "Mazowieckie", "Opolskie",
                  "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie",
                  "Świętokrzyskie", "Warmińsko-mazurskie", "Wielkopolskie", "Zachodniopomorskie"),
  Stacje_Przekroczenia = c(7, 6, 0, 0, 16, 18, 1, 1, 2, 0, 2, 13, 7, 1, 3, 0)
)

df_wynik <- merge(df_astma, df_smog, by = "Wojewodztwo")

mnoznik <- 3

ggplot(df_wynik, aes(x = reorder(Wojewodztwo, Chorobowosc))) +
  geom_col(aes(y = Chorobowosc), fill = "lightpink", alpha = 0.8) +
  geom_point(aes(y = Stacje_Przekroczenia * mnoznik), color = "darkred", size = 4) +
  coord_flip() +
  scale_y_continuous(
    name = "Chorobowość na 1 tys. ludności (różowe słupki)",
    sec.axis = sec_axis(~ . / mnoznik, name = "Liczba stacji GIOŚ z przekroczeniami (czerwone punkty)")
  ) +
  labs(
    title = "Korelacja: Astma a zanieczyszczenia powietrza w Polsce (2023)",
    x = "Województwo"
  ) +
  theme_minimal()

