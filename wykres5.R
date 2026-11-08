library(readxl)
library(dplyr)
library(ggplot2)

df_wiek_raw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.2", skip = 1)
df_wiek <- df_wiek_raw %>%
  select(grupa_wiekowa = 1, plec = 2, wskaźnik_1k = 4) %>%
  filter(!is.na(wskaźnik_1k), !grepl("Źródło|Powrót", as.character(grupa_wiekowa))) %>%
  mutate(
    wskaźnik_1k = as.numeric(wskaźnik_1k),
    plec = factor(plec, levels = c("KOBIETY", "MĘŻCZYŹNI")),
    grupa_wiekowa = factor(grupa_wiekowa, levels = c("0-5", "6-10", "11-17", "18-40", "41-55", "56-70", "71+"))
  )

ggplot(df_wiek, aes(x = grupa_wiekowa, y = wskaźnik_1k, fill = plec)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("KOBIETY" = "#9F80AD", "MĘŻCZYŹNI" = "#2980B9")) +
  labs(
    title = "Chorobowość na astmę według wieku i płci (2023)",
    subtitle = "Wskaźnik chorobowości rejestrowanej na 1 tys. ludności w danej grupie",
    x = "Grupa wiekowa",
    y = "Chorobowość na 1 tys. ludności",
    fill = "Płeć"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )