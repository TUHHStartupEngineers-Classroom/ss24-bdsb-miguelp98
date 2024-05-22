library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)

# Cargar datos
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Selección de países de interés
countries <- c("Europe", "Germany", "United Kingdom", "France", "Spain", "United States")

# Filtrar y preparar los datos
covid_data_filtered <- covid_data_tbl %>%
  filter(location %in% countries) %>%
  select(location, date, total_cases) %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
  group_by(location) %>%
  arrange(location, date) %>%
  mutate(cumulative_cases = total_cases)

# Para visualizar los datos, utilizaremos la última fecha para los labels
latest_data <- covid_data_filtered %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  filter(!is.na(cumulative_cases))

# Crear la visualización
ggplot(data = covid_data_filtered, aes(x = date, y = cumulative_cases, color = location)) +
  geom_line(size = 1.2) +
  geom_label_repel(data = latest_data, aes(label = scales::comma(cumulative_cases)), 
                   nudge_x = 10, nudge_y = 10, size = 3, color = "black", fill = "white") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of latest available data",
    x = "",
    y = "Cumulative Cases",
    color = "Continent / Country"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )