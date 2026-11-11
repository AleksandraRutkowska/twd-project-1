library(readxl)
library(dplyr)
library(ggplot2)

df_koszty <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.7", skip = 2) %>%
  select(rok = 1, koszty_szpital = 4) %>%
  filter(!is.na(rok), !grepl("Źródło|Wartość", as.character(rok))) %>%
  mutate(rok = as.numeric(rok), koszty_szpital = as.numeric(koszty_szpital)) %>%
  filter(rok >= 2013 & rok <= 2022)

df_hosp <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.5", skip = 2) %>%
  select(rok = 1, nagle = 2) %>%
  filter(!is.na(rok), !grepl("Źródło", as.character(rok))) %>%
  mutate(rok = as.numeric(rok), nagle = as.numeric(nagle)) %>%
  filter(rok >= 2013 & rok <= 2022)

df_wynik <- merge(df_koszty, df_hosp, by = "rok")

mnoznik <- 7 

ggplot(df_wynik, aes(x = rok)) +
  geom_col(aes(y = koszty_szpital), fill = "lightpink", alpha = 0.9) +
  geom_line(aes(y = nagle * mnoznik), color = "darkred", linewidth = 1.2) +
  geom_point(aes(y = nagle * mnoznik), color = "darkred", size = 3.5) +
  scale_x_continuous(breaks = 2013:2022) +
  scale_y_continuous(
    name = "Koszty leczenia szpitalnego w mln PLN (różowe słupki)",
    sec.axis = sec_axis(~ . / mnoznik, name = "Nagłe hospitalizacje w tys. (ciemnoczerwona linia)")
  ) +
  labs(
    title = "Cena zaostrzeń: Koszty szpitalne a liczba nagłych ataków astmy",
    x = "Rok"
  ) +
  theme_minimal()