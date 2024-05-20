# Your API Key for OpenWeatherMap
api_key <- "a256ad6d852c483510ecff0ed3518e2f"

# The city for which you want to get the weather data
city <- "Madrid"

# Construct the request URL
url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=", city, "&appid=", api_key, "&units=metric")

# Make the GET request
response <- GET(url)

# Check the status of the response
if (status_code(response) == 200) {
  # Parse the content of the response
  weather_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  # Check the structure of the weather_data$weather object
  str(weather_data$weather)
  
  # Extract relevant data
  main_data <- weather_data$main
  
  # weather_data$weather is a list, access the first entry
  weather_description <- weather_data$weather$description
  
  # Create a data frame with the data
  weather_df <- data.frame(
    City = city,
    Temperature = main_data$temp,
    Pressure = main_data$pressure,
    Humidity = main_data$humidity,
    Description = weather_description
  )
  
  # Print the data in a readable format
  print(weather_df)
  
  # Plot the temperature
  ggplot(weather_df, aes(x = City, y = Temperature, fill = Description)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Temperature in", city),
         y = "Temperature (°C)")
  
} else {
  print("Error in the API request")
}
