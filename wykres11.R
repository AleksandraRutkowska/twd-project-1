library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Wykres 11: Polska na tle Europy - chorobowość vs hospitalizacje z powodu astmy
# Dane: Wykres 1.4 (chorobowość na 1 tys., 2019) i Wykres 2.6 (hospitalizacje 15+ na 100 tys., 2020, OECD)

df_chor <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 1.4", skip = 2) %>%
  select(kraj = 1, chorobowosc = 2) %>%
  mutate(chorobowosc = as.numeric(chorobowosc))

df_hosp <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.6", skip = 2) %>%
  select(kraj = 1, hospitalizacje = 2) %>%
  mutate(hospitalizacje = as.numeric(hospitalizacje))

df <- inner_join(df_chor, df_hosp, by = "kraj") %>%
  filter(!is.na(chorobowosc), !is.na(hospitalizacje)) %>%
  mutate(polska = kraj == "Polska")

med_x <- median(df$chorobowosc)
med_y <- median(df$hospitalizacje)
r <- cor(df$chorobowosc, df$hospitalizacje)

ggplot(df, aes(x = chorobowosc, y = hospitalizacje)) +
  geom_vline(xintercept = med_x, linetype = "dashed", color = "#E8A0B8") +
  geom_hline(yintercept = med_y, linetype = "dashed", color = "#E8A0B8") +
  geom_point(aes(fill = polska, size = polska), shape = 21, color = "white", stroke = 1.2) +
  geom_text_repel(aes(label = kraj, color = polska, fontface = ifelse(polska, "bold", "plain")),
                  size = 4, max.overlaps = 30) +
  annotate("text", x = 108, y = -5, hjust = 1, vjust = 0, size = 3.8,
           fontface = "italic", color = "#C06A8A",
           label = "dużo chorych, mało hospitalizacji") +
  annotate("text", x = 15, y = 78, hjust = 0, vjust = 1, size = 3.8,
           fontface = "italic", color = "#C06A8A",
           label = "mało chorych, dużo hospitalizacji") +
  scale_fill_manual(values = c("FALSE" = "#F4A6C0", "TRUE" = "#AD1457"), guide = "none") +
  scale_size_manual(values = c("FALSE" = 4.5, "TRUE" = 8), guide = "none") +
  scale_color_manual(values = c("FALSE" = "#6D3A4F", "TRUE" = "#880E4F"), guide = "none") +
  scale_x_continuous(limits = c(15, 108)) +
  scale_y_continuous(limits = c(-5, 78), breaks = seq(0, 70, 10)) +
  labs(
    title = "Polska: chorobowość powyżej mediany, a hospitalizacje w górnej piątce krajów",
    subtitle = paste0(
      "W ", nrow(df), " krajach obecnych w obu zestawieniach więcej chorych wcale nie oznacza więcej hospitalizacji (r = ",
      format(round(r, 2), decimal.mark = ","), ").\n",
      "Portugalia, Szwecja czy Holandia mają dużo chorych i niewiele hospitalizacji, Polska – umiarkowanie dużo chorych i wysoką hospitalizację."
    ),
    x = "Chorobowość na astmę na 1 tys. ludności (szacunek, 2019)",
    y = "Hospitalizacje z powodu astmy, 15+, na 100 tys. (2020)",
    caption = paste0(
      "Źródło: NFZ, Astma – aktualizacja 2023 (Wykresy 1.4 i 2.6; dane OECD). Linie przerywane to mediany. ",
      "Zestawienie łączy różne lata i metodyki, a rok 2020 to rok pandemii – wnioski traktować orientacyjnie."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#6D3A4F"),
    plot.caption = element_text(color = "grey40", size = 9, hjust = 0),
    panel.grid.minor = element_blank()
  )
