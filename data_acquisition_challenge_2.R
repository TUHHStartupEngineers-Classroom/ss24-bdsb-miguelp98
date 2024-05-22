library(tidyverse)
library(rvest)
library(xopen)
library(readr)
library(scales)

# URL de la página principal de Rose Bikes con la correcta codificación para "Fahrräder"
url_home <- "https://www.rosebikes.de/fahrr%C3%A4der"
#xopen(url_home) # Abre la URL en el navegador para inspección
html_home <- read_html(url_home)

# Obtener las URLs de las categorías de bicicletas
bike_categories_chr <- html_home |>
  html_elements(css = ".catalog-navigation__link") |>
  html_attr('href') |>
  str_subset(pattern = "sale|outlet|gear|customer-service", negate = T) |>
  str_c("https://www.rosebikes.de", ... = _)

# Imprimir las URLs de las categorías para verificar
print(bike_categories_chr)


# Seleccionamos la primera categoría de bicicletas (Gravel)
bike_category_url <- bike_categories_chr[1]

# Obtener las URLs de los modelos de bicicletas para la categoría seleccionada
html_bike_category <- read_html(bike_category_url)

bike_url_chr <- html_bike_category |>
  html_elements(css = ".catalog-navigation__list-item+ .catalog-navigation__list-item .catalog-navigation__link") |>  # no vemos la de Alle
  html_attr("href") |>
  str_c("https://www.rosebikes.de", ... = _)

# Imprimir las URLs de los modelos de bicicletas para verificar
print(bike_url_chr)

# URL de la subcategoría Adventure
url_adventure <- bike_url_chr[1]  # Asumimos que "Adventure" es la primera subcategoría
#xopen(url_adventure) # Abre la URL en el navegador para inspección
html_adventure <- read_html(url_adventure)

#Obtener los nombres de los modelos y los precios
bike_names <- html_adventure |>
  html_elements(css = ".basic-headline--left .basic-headline__title") |>
  html_text() |>
  str_squish() # Limpiar texto

bike_prices <- html_adventure |>
  html_elements(css = ".catalog-category-bikes__price-title") |>
  html_text() |>
  str_squish() |>
  str_remove("^ab")

# Crear un data frame con los nombres y precios
bike_data <- tibble(
  Model = bike_names,
  Price = bike_prices
)

# Imprimir el data frame
print(bike_data)