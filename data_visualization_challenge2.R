# Cargar librerías
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)
library(maps)

# Cargar datos de COVID-19
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Cargar datos de mapa mundial
world <- map_data("world")

# Filtrar y preparar los datos de COVID-19
covid_data_filtered <- covid_data_tbl %>%
  filter(!is.na(total_deaths), !is.na(population)) %>%
  select(location, date, total_deaths, population) %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  mutate(mortality_rate = total_deaths / population * 100) %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>%
  distinct(location, .keep_all = TRUE)

# Unir los datos del mapa con los datos de COVID-19
plot_data <- world %>%
  left_join(covid_data_filtered, by = c("region" = "location"))

# Crear la visualización
ggplot(plot_data) +
  geom_map(aes(map_id = region, fill = mortality_rate), map = world) +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_gradient(low = "red", high = "darkred", na.value = "grey50", 
                      name = "Mortality Rate", labels = scales::percent) +
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "Around 8.2 Million confirmed COVID-19 deaths worldwide",
    caption = "Date: latest available data"
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
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )
