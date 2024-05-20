library(tidyverse)
library(rvest)
library(xopen)

# URL de la página principal
url_home <- "https://www.rosebikes.de/fahrr%C3%A4der"
xopen(url_home) # Abre el enlace para inspeccionarlo
html_home <- read_html(url_home)

# Obtener las URLs de las categorías de bicicletas
bike_categories_chr <- html_home |>
  html_elements(css = ".main-navigation__item--bikes .main-navigation__link") |>
  html_attr('href') |>
  str_c("https://www.rosebikes.de", ... = _)

# Seleccionar la categoría "Gravel"
bike_category_url <- bike_categories_chr[1]

# Obtener las URLs de las subcategorías de "Gravel"
html_bike_category <- read_html(bike_category_url)

bike_subcategory_urls <- html_bike_category |>
  html_elements(css = ".catalog-navigation__link") |>
  html_attr("href") |>
  str_c("https://www.rosebikes.de", ... = _)

# Imprimir las URLs de las subcategorías para verificar
print(bike_subcategory_urls)

# Seleccionar la subcategoría "Adventure"
bike_subcategory_url <- bike_subcategory_urls[1]

# Obtener las URLs de los modelos de bicicletas para la subcategoría seleccionada
html_bike_subcategory <- read_html(bike_subcategory_url)

bike_model_urls <- html_bike_subcategory |>
  html_elements(css = ".catalog-category-bikes__link") |>
  html_attr("href") |>
  str_c("https://www.rosebikes.de", ... = _)

# Imprimir las URLs de los modelos de bicicletas para verificar
print(bike_model_urls)

# Función para extraer nombres y precios de los modelos de bicicletas
get_model_data <- function(url) {
  
  html_bike_model <- read_html(url)
  
  bike_price <- html_bike_model |>
    html_element(css = ".catalog-category-bikes__price") |>  # Ajustar el selector CSS según sea necesario
    html_text() |>
    parse_number() |>
    str_remove("\\.")
  
  bike_model <- html_bike_model |> 
    html_element(css = ".catalog-category-bikes__title") |>  # Ajustar el selector CSS según sea necesario
    html_text() |> 
    str_squish() # Clean
  
  bike_data <- tibble(url   = url,
                      model = bike_model,
                      price = bike_price)
  
  return(bike_data)
}

# Ejecutar la función para todos los modelos de la subcategoría
bike_model_data_tbl <- bike_model_urls |> map_dfr(get_model_data)

# Imprimir los datos de los modelos
print(bike_model_data_tbl)
