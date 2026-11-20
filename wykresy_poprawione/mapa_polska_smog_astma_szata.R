library(readxl)
library(dplyr)
library(ggplot2)
library(sf)

dane_astma <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.3", skip = 1) %>%
  select(nazwa = 1, astma = 2) %>%
  mutate(nazwa = tolower(nazwa), astma = as.numeric(astma)) %>%
  filter(!is.na(astma))

raw <- read_excel("2023_PM10_24g.xlsx", sheet = "2023_PM10_24g", col_names = FALSE)
woj_map <- c(
  Ds="dolnośląskie", Kp="kujawsko-pomorskie", Lb="lubelskie", Lu="lubuskie",
  Ld="łódzkie", Mp="małopolskie", Mz="mazowieckie", Op="opolskie",
  Pk="podkarpackie", Pd="podlaskie", Pm="pomorskie", Sl="śląskie",
  Sk="świętokrzyskie", Wm="warmińsko-mazurskie", Wp="wielkopolskie", Zp="zachodniopomorskie"
)

dane_smog <- tibble(
  nazwa = woj_map[substr(raw[2, -1], 1, 2)],
  dni = colSums(sapply(raw[-c(1:6), -1], as.numeric) > 50, na.rm = TRUE)
) %>% 
  filter(!is.na(nazwa)) %>% 
  group_by(nazwa) %>% 
  summarise(dni_smogowe = mean(dni, na.rm = TRUE))

mapa_sf <- st_read("https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-min.geojson", quiet = TRUE) %>%
  mutate(nazwa = tolower(nazwa)) %>%
  left_join(dane_smog, by = "nazwa") %>%
  left_join(dane_astma, by = "nazwa")

wykres <- ggplot(mapa_sf) +
  geom_sf(aes(fill = dni_smogowe), color = "white", linewidth = 0.6) +
  geom_sf(data = st_centroid(mapa_sf), aes(color = astma), size = 8) +
  scale_fill_gradient(
    low = "#EBEBEB", high = "#1A1A1A", 
    name = "Liczba dni z przekroczonym\nzanieczyszczeniem powietrza"
  ) +
  scale_color_gradient(
    low = "#F8BBD0", high = "#880E4F", 
    name = "Przypadki astmy\nna 1 tys. mieszkańców"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5, margin = margin(b = 15)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

ggsave("mapa_polska_smog_astma_szata2.png", plot = wykres, width = 10, height = 8, dpi = 300, bg = "white")
