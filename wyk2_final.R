library(readxl)
library(dplyr)
library(ggplot2)

df_hosp <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.5", skip = 2) %>%
  select(rok = 1, nagle = 2) %>%
  mutate(rok = as.numeric(rok)) %>% 
  filter(rok >= 2013 & rok <= 2023) %>%
  mutate(nagle = as.numeric(nagle))

df_smog <- data.frame(
  rok = 2013:2023,
  strefy = c(36, 42, 39, 35, 34, 39, 22, 16, 25, 14, 2)
)

df_wynik <- merge(df_hosp, df_smog, by = "rok")

ggplot(df_wynik, aes(x = rok)) +
  geom_col(aes(y = strefy), fill = "lightpink") +
  geom_line(aes(y = nagle * 2.5), color = "darkred", linewidth = 1.2) +
  geom_point(aes(y = nagle * 2.5), color = "darkred", size = 3.5) +
  scale_x_continuous(breaks = 2013:2023) +
  
  scale_y_continuous(
    name = "Liczba stref smogowych (różowe słupki)",
    sec.axis = sec_axis(~ . / 2.5, name = "Nagłe hospitalizacje w tys. (ciemnoczerwona linia)")
  ) +
  labs(title = "Korelacja: Smog a nagłe ataki astmy (2013-2023)") +
  theme_minimal()