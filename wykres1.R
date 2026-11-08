library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# 1. Wczytanie danych NFZ o chorobowości z Twojego pliku Excel
df_nfz_raw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.3", skip = 1)
df_nfz <- df_nfz_raw %>%
  select(wojewodztwo = 1, chorobowosc_1k = 2) %>%
  filter(!is.na(chorobowosc_1k), !grepl("Źródło|Powrót", wojewodztwo)) %>%
  mutate(chorobowosc_1k = as.numeric(chorobowosc_1k))

# 2. Dane GIOŚ o liczbie stacji z przekroczeniami norm (Tabela 3.13.3 z Raportu GIOŚ 2023)
df_gios <- tibble(
  wojewodztwo = c("Dolnośląskie", "Kujawsko-pomorskie", "Lubelskie", "Lubuskie",
                  "Łódzkie", "Małopolskie", "Mazowieckie", "Opolskie",
                  "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie",
                  "Świętokrzyskie", "Warmińsko-mazurskie", "Wielkopolskie", "Zachodniopomorskie"),
  stacje_przekroczenia = c(7, 6, 0, 0, 16, 18, 1, 1, 2, 0, 2, 13, 7, 1, 3, 0)
)

# 3. Połączenie danych
df_relacja <- inner_join(df_nfz, df_gios, by = "wojewodztwo")

# 4. Wykres rozrzutu z linią trendu
ggplot(df_relacja, aes(x = stacje_przekroczenia, y = chorobowosc_1k)) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", fill = "#BDC3C7", alpha = 0.3) +
  geom_point(color = "#F7122B", size = 4.5, alpha = 0.85) +
  geom_text_repel(aes(label = wojewodztwo), size = 4, max.overlaps = 20) +
  labs(
    title = "Jakość powietrza a chorobowość na astmę w województwach (2023)",
    subtitle = "Zestawienie liczby stacji pomiarowych GIOŚ z przekroczeniami norm vs wskaźnik NFZ",
    x = "Liczba stanowisk pomiarowych GIOŚ z przekroczeniami norm (PM10, BaP, NO2, O3)",
    y = "Chorobowość rejestrowana na 1 tys. ludności"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )