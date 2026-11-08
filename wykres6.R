library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

df_leki_raw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.15", skip = 1)
df_leki <- df_leki_raw %>%
  select(rok = 1, refundacja = 2, doplata = 3) %>%
  filter(!is.na(rok), !grepl("Źródło|Powrót", as.character(rok))) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(cols = c(refundacja, doplata), names_to = "strona", values_to = "kwota_mln") %>%
  mutate(strona = recode(strona, "refundacja" = "Refundacja NFZ", "doplata" = "Dopłata pacjentów"))

ggplot(df_leki, aes(x = factor(rok), y = kwota_mln, fill = strona)) +
  geom_col(position = "stack", width = 0.65) +
  scale_fill_manual(values = c("Refundacja NFZ" = "#25A085", "Dopłata pacjentów" = "#A34E22")) +
  labs(
    title = "Roczne wydatki na leki przeciwastmatyczne w Polsce (2014–2023)",
    subtitle = "Podział łącznych nakładów na refundację NFZ i dopłaty bezpośrednie pacjentów (mln zł)",
    x = "Rok",
    y = "Wydatki [mln PLN]",
    fill = "Źródło finansowania"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )