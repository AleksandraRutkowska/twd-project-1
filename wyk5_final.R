library(readxl)
library(dplyr)
library(ggplot2)

df_chor <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 1.4", skip = 2) %>%
  select(Kraj = 1, Chorobowosc = 2) %>%
  mutate(Chorobowosc = as.numeric(Chorobowosc))

df_hosp <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.6", skip = 2) %>%
  select(Kraj = 1, Hospitalizacje = 2) %>%
  mutate(Hospitalizacje = as.numeric(Hospitalizacje))

df_wynik <- inner_join(df_chor, df_hosp, by = "Kraj") %>%
  filter(!is.na(Chorobowosc), !is.na(Hospitalizacje))

mnoznik <- 1.4

ggplot(df_wynik, aes(x = reorder(Kraj, Chorobowosc))) +
  geom_col(aes(y = Chorobowosc), fill = "lightpink", alpha = 0.85) +
  geom_point(aes(y = Hospitalizacje * mnoznik), color = "darkred", size = 3.8) +
  coord_flip() +
  scale_y_continuous(
    name = "Chorobowość na 1 tys. ludności (różowe słupki)",
    sec.axis = sec_axis(~ . / mnoznik, name = "Hospitalizacje na 100 tys. mieszkańców (czerwone punkty)")
  ) +
  labs(
    title = "Polska na tle Europy: Chorobowość a hospitalizacje z powodu astmy",
    x = "Kraj"
  ) +
  theme_minimal()