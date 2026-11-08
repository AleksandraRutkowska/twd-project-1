library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Wykres 9: Leki na astmę bez rozpoznania astmy (heatmapa rok x grupa wiekowa)
# Dane: Tabela 2.16 (NFZ, Astma - aktualizacja 2023)

grupy <- c("0-5", "6-10", "11-17", "18-40", "41-55", "56-70", "71+")

df_raw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.16", skip = 2)

df_diag <- df_raw %>%
  rename(rok = 1) %>%
  filter(grepl("^[0-9]{4}$", as.character(rok))) %>%
  select(rok, all_of(grupy)) %>%
  pivot_longer(cols = -rok, names_to = "grupa", values_to = "odsetek") %>%
  mutate(
    rok = factor(as.character(rok)),
    odsetek = as.numeric(odsetek),
    grupa = factor(grupa, levels = rev(grupy))   # najmłodsi na górze
  )

ggplot(df_diag, aes(x = rok, y = grupa, fill = odsetek)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = percent(odsetek, accuracy = 1), color = odsetek > 0.5),
            size = 4.2, fontface = "bold") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#5B1034"), guide = "none") +
  scale_fill_gradient(
    low = "#FDE7EF", high = "#B0155F",
    labels = percent_format(accuracy = 1),
    name = "Odsetek pacjentów\nz rozpoznaniem astmy"
  ) +
  labs(
    title = "Leki na astmę bez rozpoznania astmy w dokumentacji",
    subtitle = paste0(
      "Odsetek pacjentów realizujących recepty na leki przeciwastmatyczne, u których w ostatnich 3 latach sprawozdano rozpoznanie astmy.\n",
      "U dzieci 0–5 lat spadł z 30% do 25%, a u osób 71+ wzrósł z 46% do 55% (2014 vs 2023)."
    ),
    x = "Rok",
    y = "Grupa wiekowa",
    caption = paste0(
      "Źródło: NFZ, Astma – aktualizacja 2023 (Tabela 2.16). ",
      "Niski odsetek u najmłodszych może wynikać m.in. z ostrożności w stawianiu rozpoznania astmy u małych dzieci."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#6D3A4F"),
    plot.caption = element_text(color = "grey40", size = 9, hjust = 0),
    panel.grid = element_blank(),
    legend.position = "right"
  )



