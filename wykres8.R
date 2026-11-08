library(dplyr)
library(ggplot2)

df_trend_strefy <- tibble(
  rok = 2010:2023,
  strefy_C_pm10 = c(42, 42, 38, 36, 42, 39, 35, 34, 39, 22, 16, 25, 14, 2),
  strefy_C_bap  = c(38, 42, 42, 42, 46, 44, 43, 43, 44, 36, 39, 39, 32, 21)
) %>%
  tidyr::pivot_longer(cols = -rok, names_to = "zanieczyszczenie", values_to = "liczba_stref_C") %>%
  mutate(zanieczyszczenie = recode(zanieczyszczenie,
                                   "strefy_C_pm10" = "Pył PM10 (norma 24-godz.)",
                                   "strefy_C_bap"  = "Benzo(a)piren B(a)P (norma roczna)"
  ))

ggplot(df_trend_strefy, aes(x = rok, y = liczba_stref_C, color = zanieczyszczenie, group = zanieczyszczenie)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("Pył PM10 (norma 24-godz.)" = "#27AE60", "Benzo(a)piren B(a)P (norma roczna)" = "#C0392B")) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(
    title = "Ewolucja czystości powietrza: Liczba stref z przekroczeniem norm (2010–2023)",
    subtitle = "Liczba stref w klasie C (na 46 stref w Polsce) dla pyłu PM10 i kancerogennego B(a)P",
    x = "Rok oceny",
    y = "Liczba stref w klasie C (przekroczenie)",
    color = "Substancja"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )