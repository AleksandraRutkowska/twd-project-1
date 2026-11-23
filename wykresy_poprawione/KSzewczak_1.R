library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

df_chor <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 1.4", skip = 2) %>%
  select(Kraj = 1, Chorobowosc = 2) %>%
  mutate(Chorobowosc = as.numeric(Chorobowosc))

df_hosp <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.6", skip = 2) %>%
  select(Kraj = 1, Hospitalizacje = 2) %>%
  mutate(Hospitalizacje = as.numeric(Hospitalizacje))

df_wynik <- inner_join(df_chor, df_hosp, by = "Kraj") %>%
  filter(!is.na(Chorobowosc), !is.na(Hospitalizacje))

med_x <- median(df_wynik$Hospitalizacje)
med_y <- median(df_wynik$Chorobowosc)

ggplot(df_wynik, aes(x = Hospitalizacje, y = Chorobowosc)) +
  geom_vline(xintercept = med_x, linetype = "dashed", color = "lightpink3") +
  geom_hline(yintercept = med_y, linetype = "dashed", color = "lightpink3") +
  geom_point(
    aes(fill = Kraj == "Polska", size = Kraj == "Polska"),
    shape = 21, color = "white", stroke = 1
  ) +
  geom_text_repel(
    aes(label = Kraj, color = Kraj == "Polska"),
    size = 3.8, max.overlaps = 30
  ) +
  scale_fill_manual(values = c("FALSE" = "lightpink", "TRUE" = "darkred")) +
  scale_color_manual(values = c("FALSE" = "grey30", "TRUE" = "darkred")) +
  scale_size_manual(values = c("FALSE" = 4.5, "TRUE" = 7)) +
  labs(
    title = "Polska na tle Europy: Hospitalizacje a chorobowość na astmę",
    x = "Hospitalizacje z powodu astmy na 100 tys. mieszkańców",
    y = "Chorobowość na astmę na 1 tys. ludności"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )
