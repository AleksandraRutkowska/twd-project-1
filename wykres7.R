library(dplyr)
library(ggplot2)

df_sektory <- tibble(
  sektor = c("Ogrzewanie budynków (1A4)", "Rolnictwo", "Procesy przemysłowe", 
             "Transport drogowy (1A3)", "Emisja lotna z paliw", 
             "Przemysł wytwórczy i budownictwo", "Energetyka zawodowa", "Odpady"),
  udzial = c(67.61, 10.70, 9.04, 4.59, 2.64, 2.49, 1.53, 1.40)
) %>%
  mutate(sektor = reorder(sektor, udzial))

ggplot(df_sektory, aes(x = udzial, y = sektor)) +
  geom_col(fill = "#F32160", width = 0.65) +
  geom_text(aes(label = paste0(round(udzial, 1), "%")), hjust = -0.15, size = 4.2, fontface = "bold") +
  scale_x_continuous(limits = c(0, 75)) +
  labs(
    title = "Krajowy bilans emisji pyłu zawieszonego PM10 w Polsce",
    subtitle = "Udział poszczególnych sektorów gospodarki w emisji całkowitej (dane KOBiZE / GIOŚ)",
    x = "Udział w emisji krajowej [%]",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.major.y = element_blank()
  )