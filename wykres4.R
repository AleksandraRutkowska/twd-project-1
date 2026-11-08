library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

df_hosp_raw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.5", skip = 1)
df_hosp <- df_hosp_raw %>%
  select(rok = 1, nagle = 2, planowe = 3) %>%
  filter(!is.na(rok), !grepl("Źródło|Powrót", as.character(rok))) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(cols = c(nagle, planowe), names_to = "tryb", values_to = "liczba_tys") %>%
  mutate(tryb = recode(tryb, "nagle" = "Przyjęcia nagłe (ostre)", "planowe" = "Przyjęcia planowe"))

ggplot(df_hosp, aes(x = rok, y = liczba_tys, color = tryb)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Przyjęcia nagłe (ostre)" = "#10392B", "Przyjęcia planowe" = "#9980B9")) +
  scale_x_continuous(breaks = 2013:2023) +
  labs(
    title = "Hospitalizacje z powodu astmy u dorosłych: zaostrzenia ostre vs. planowe",
    subtitle = "Liczba hospitalizacji w latach 2013–2023 (w tys. pacjentów)",
    x = "Rok",
    y = "Liczba hospitalizacji [tys.]",
    color = "Tryb przyjęcia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )