library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

df_koszty_raw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.7", skip = 1)
df_koszty <- df_koszty_raw %>%
  select(rok = 1, aos = 3, szpital = 4, program_lekowy = 5, rehab = 6) %>%
  filter(!is.na(rok), !grepl("Źródło|Wartość|Powrót", as.character(rok))) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(cols = -rok, names_to = "kategoria", values_to = "koszt_mln") %>%
  mutate(kategoria = recode(kategoria,
                            "aos" = "Ambulatoryjna opieka specjalistyczna (AOS)",
                            "szpital" = "Leczenie szpitalne",
                            "program_lekowy" = "Programy lekowe",
                            "rehab" = "Rehabilitacja"
  ))

ggplot(df_koszty, aes(x = factor(rok), y = koszt_mln, fill = kategoria)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  labs(
    title = "Koszty bezpośrednie leczenia astmy oskrzelowej w Polsce (2013–2022)",
    subtitle = "Wydatki refundacyjne NFZ z podziałem na segmenty opieki zdrowotnej (mln zł)",
    x = "Rok",
    y = "Wartość refundacji [mln PLN]",
    fill = "Segment świadczen"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )