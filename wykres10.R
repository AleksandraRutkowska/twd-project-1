library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Wykres 10: Chorobowość a intensywność opieki w województwach
# Dane: Wykres 2.3 (chorobowość standaryzowana, wg miejsca zamieszkania)
#       Wykres 2.8 (świadczenia na 100 tys., wg miejsca udzielenia świadczenia)

df_chor <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.3", skip = 2) %>%
  select(wojewodztwo = 1, chorobowosc_1k = 2) %>%
  mutate(chorobowosc_1k = as.numeric(chorobowosc_1k))

df_sw <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.8", skip = 2) %>%
  select(wojewodztwo = 1, swiadczenia_100k = 2) %>%
  mutate(swiadczenia_100k = as.numeric(swiadczenia_100k))

df <- inner_join(df_chor, df_sw, by = "wojewodztwo") %>%
  filter(!is.na(chorobowosc_1k), !is.na(swiadczenia_100k)) %>%
  # orientacyjnie: świadczeń przypadających na 1 chorego rocznie
  mutate(swiadczen_na_chorego = swiadczenia_100k / (chorobowosc_1k * 100))

r <- cor(df$chorobowosc_1k, df$swiadczenia_100k)
woj_min <- df$wojewodztwo[which.min(df$swiadczen_na_chorego)]
woj_max <- df$wojewodztwo[which.max(df$swiadczen_na_chorego)]
fmt <- function(x) format(round(x, 1), decimal.mark = ",")

ggplot(df, aes(x = chorobowosc_1k, y = swiadczenia_100k)) +
  geom_smooth(method = "lm", se = TRUE, color = "#880E4F", fill = "#F8BBD0", alpha = 0.35) +
  geom_point(aes(fill = swiadczen_na_chorego),
             shape = 21, size = 6, color = "white", stroke = 1.2) +
  geom_text_repel(aes(label = wojewodztwo), size = 4, color = "#5B1034", max.overlaps = 20) +
  scale_fill_gradient(low = "#F8BBD0", high = "#AD1457",
                      name = "Świadczeń na\n1 chorego (orient.)") +
  labs(
    title = "Gdzie chorych jest więcej, tam jest więcej świadczeń – ale nie wszędzie tak samo",
    subtitle = paste0(
      "Korelacja r = ", format(round(r, 2), decimal.mark = ","),
      ", jednak liczba świadczeń przypadających na jednego chorego waha się od ",
      fmt(min(df$swiadczen_na_chorego)), " (", woj_min, ") do ",
      fmt(max(df$swiadczen_na_chorego)), " (", woj_max, ")."
    ),
    x = "Chorobowość na astmę na 1 tys. ludności (standaryzowana, 2023)",
    y = "Świadczenia z rozpoznaniem astmy na 100 tys. ludności (2023)",
    caption = paste0(
      "Źródło: NFZ, Astma – aktualizacja 2023 (Wykresy 2.3 i 2.8). ",
      "Chorobowość liczona wg miejsca zamieszkania, świadczenia wg miejsca udzielenia – wskaźnik na 1 chorego jest orientacyjny."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#6D3A4F"),
    plot.caption = element_text(color = "grey40", size = 9, hjust = 0),
    panel.grid.minor = element_blank()
  )
