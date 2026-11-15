library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

grupy <- c("0-5", "6-10", "11-17", "18-40", "41-55", "56-70", "71+")

df_wiek <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Wykres 2.2", skip = 1) %>%
  select(grupa = 1, plec = 2, chorobowosc = 4) %>%
  filter(!is.na(chorobowosc), grupa %in% grupy) %>%
  mutate(chorobowosc = as.numeric(chorobowosc)) %>%
  group_by(grupa) %>%
  summarise(chorobowosc_1k = mean(chorobowosc, na.rm = TRUE), .groups = "drop")

df_diag <- read_excel("Astma_aktualizacja_2023.xlsx", sheet = "Tabela 2.16", skip = 2) %>%
  rename(rok = 1) %>%
  filter(as.character(rok) == "2023") %>%
  select(all_of(grupy)) %>%
  pivot_longer(cols = everything(), names_to = "grupa", values_to = "odsetek_diag") %>%
  mutate(odsetek_proc = as.numeric(odsetek_diag) * 100)

df_wynik <- inner_join(df_wiek, df_diag, by = "grupa") %>%
  mutate(grupa = factor(grupa, levels = grupy))

mnoznik <- 1.2

ggplot(df_wynik, aes(x = grupa)) +
  geom_col(aes(y = chorobowosc_1k), fill = "lightpink", alpha = 0.9, width = 0.65) +
  geom_line(aes(y = odsetek_proc * mnoznik, group = 1), color = "darkred", linewidth = 1.2) +
  geom_point(aes(y = odsetek_proc * mnoznik), color = "darkred", size = 3.5) +
  scale_y_continuous(
    name = "Liczba chorych na astmę na 1000 osób w danym wieku (różowe słupki)",
    sec.axis = sec_axis(
      ~ . / mnoznik,
      name = "% osób biorących leki na astmę z wpisaną diagnozą (czerwona linia)"
    )
  ) +
  labs(
    title = "Liczba chorych a odsetek pacjentów z formalnym rozpoznaniem (2023)",
    x = "Grupa wiekowa [lata]"
  ) +
  theme_minimal()


