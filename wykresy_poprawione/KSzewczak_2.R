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

ggplot(df_wynik, aes(x = Chorobowosc, y = reorder(Kraj, Chorobowosc))) +
  geom_col(aes(fill = Hospitalizacje, color = Kraj == "Polska"), linewidth = 1) +
  scale_fill_gradient(
    low = "#FCE4EC",
    high = "darkred",
    name = "Hospitalizacje\n(na 100 tys.)"
  ) +
  scale_color_manual(values = c("FALSE" = NA, "TRUE" = "black")) +
  guides(color = "none") +
  labs(
    x = "Chorobowość na 1 tys. ludności",
    y = "Kraj"
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 13))