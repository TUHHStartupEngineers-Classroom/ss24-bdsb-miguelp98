
library(tidyverse)
library(rvest)
library(xopen)

html_home <- read_html("https://www.canyon.com/en-de")
html_home |> xml2::write_html("home.html")

url_home        <- "https://www.canyon.com/en-de"
xopen(url_home) # Open links directly from RStudio to inspect them
html_home       <- read_html(url_home)


bike_categories_chr <- html_home |>
  
  # Get the nodes for the categories. Take a look at the source code for the selector.
  # (Unfortunately not working with the Selector Gadget with the original page)
  html_elements(css = ".header__navBarPreloadItem--level2") |>
  
  # Extract the href attribute (the URLs)
  html_attr('href') |>
  
  # Remove the product families Sale, Outlet, Gear and Customer Service
  str_subset(pattern = "sale|outlet|gear|customer-service", negate = T) |>
  
  # Add the domain, because we will get only the subdirectories
  # Watch out for the new pipe placeholder `_` here.
  # It requires a named argument (`...` in this case)
  str_c("https://www.canyon.com", ... = _)

# 2.0 COLLECT BIKE DATA ----
# Select first bike category url
bike_category_url <- bike_categories_chr[1]


# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)

bike_url_chr        <- html_bike_category |>
  
  # Get the 'a' nodes that containt the title and the link
  html_elements(".productTileDefault__productName") |>
  
  # html_elements(css = ".productTileDefault--bike > div > a") |>
  html_attr("href") |>
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*")



get_bike_urls <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_chr  <- html_bike_category |> 
    html_elements(css = ".productTileDefault__productName") |>
    html_attr("href") |>
    str_remove(pattern = "\\?.*")
  
  return(bike_url_chr)
  
}

# Run the function with the first url to check if it is working
get_bike_urls(url = bike_categories_chr[1])

# Map the function against all urls
bike_urls_chr <- map(bike_categories_chr, get_bike_urls) |>
  flatten_chr() |>
  unique()

# Split data to enable filtering
bike_urls_tbl <- bike_urls_chr |>
  
  tibble::as_tibble_col(column_name = "url") |>
  tidyr::separate_wider_regex(cols = url, patterns = c(".*en-de/", family   = "[^/]*", "/",
                                                       category = "[^/]*", "/",
                                                       model    = "[^/]*", "/",
                                                       material = "[^/]*", "/",
                                                       ".*"), cols_remove = F,
                              too_few ="align_start"
                              )

# Filter
bike_urls_endurace_tbl <- bike_urls_tbl |>
  filter(model == "endurace")

# Print
bike_urls_endurace_tbl |> slice(1:5)

#3.2 Get name and prices for the endurace category bikes ----

html_bike_model <- read_html(bike_urls_endurace_tbl$url[1])

bike_price <- html_bike_model |>
  html_element(css = ".productDescription__priceSale") |>
  html_text() |>
  parse_number() |>
  str_remove("\\.")

bike_model <- html_bike_model |> 
  html_elements(css = ".xlt-pdpName") |> 
  html_text() |> 
  str_squish() # Clean

# Function to do it for multiple urls
get_model_data <- function(url) {
  
  html_bike_model <- read_html(url)
  
  bike_price <- html_bike_model |>
    html_element(css = ".productDescription__priceSale") |>
    html_text() |>
    parse_number() |>
    str_remove("\\.")
  
  bike_model <- html_bike_model |> 
    html_elements(css = ".xlt-pdpName") |> 
    html_text() |> 
    str_squish() # Clean
  
  bike_data <- tibble(url   = url,
                      model = bike_model,
                      price = bike_price)
  
  return(bike_data)
  
}

### RUN FUNCTION ----

# For one model
bike_model_data_tbl <- get_model_data(url = bike_urls_endurace_tbl$url[1])

# For all models of the category
bike_model_data_tbl <- bike_urls_endurace_tbl$url |> map_dfr(get_model_data)

# Join data
bike_model_data_joined_tbl <- bike_urls_endurace_tbl |> 
  left_join(bike_model_data_tbl, by = join_by("url"))

# Print
bike_model_data_joined_tbl