install.packages("ggplot2")
install.packages("ggfortify")
library(ggplot2)
library("ggfortify")

#Baza Danych:
install.packages("httr")
install.packages("dplyr")
install.packages("jsonlite")
install.packages("lubridate")

library(httr)
library(dplyr)
library(jsonlite)
library(lubridate)

url <- "https://archive-api.open-meteo.com/v1/archive"

get_weather_data <- function(latitude, longitude, start_date, end_date, hourly_data) {
  params <- list(
    latitude = latitude,
    longitude = longitude,
    start_date = start_date,
    end_date = end_date,
    hourly = hourly_data
  )
  
  response <- GET(url, query = params)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    
    df <- as.data.frame(data$hourly)
    
    df$date <- ymd_hm(df$time)
    
    if (any(is.na(df$date))) {
      print("Błąd konwersji daty. Sprawdzam strukturę danych...")
      print(head(df$time))
    }
    
    df <- df %>% select(date, temperature_2m, relative_humidity_2m, precipitation, soil_temperature_0_to_7cm)
    df <- df %>% select(date, everything())
    
    return(df)
    
  } else {
    stop(paste("Błąd podczas pobierania danych:", status_code(response)))
  }
}

latitude <- 52.4064  # Szerokość geograficzna Poznania
longitude <- 16.9252 # Długość geograficzna Poznania
start_date <- "2014-01-01"
end_date <- "2024-12-31"
hourly_data <- "temperature_2m,relative_humidity_2m,precipitation,soil_temperature_0_to_7cm"

weather_data <- get_weather_data(latitude, longitude, start_date, end_date, hourly_data)

print(head(weather_data))


#NeuralProphet:
install.packages("reticulate")
reticulate::install_python()

library(reticulate)
np <- import("neuralprophet")

df_prophet <- weather_data %>% select(date, temperature_2m)
colnames(df_prophet) <- c("ds", "y")

model <- np$NeuralProphet(yearly_seasonality = TRUE, weekly_seasonality = TRUE)
model$fit(df_prophet)
# prognozy 
future <- model$make_future_dataframe(df_prophet, periods = 168)
forecast <- model$predict(future)
#plot(forecast)

#ARIMA

install.packages("forecast")
install.packages("urca")

library(forecast)
library(urca)

weather_data$date <- as.Date(weather_data$date)
temperature_data <- weather_data$temperature_2m

adf_test <- ur.df(temperature_data, type = "none", lags = 12)
print(adf_test)

if (adf_test@teststat[1] > adf_test@cval[1,1]) {
  temperature_data <- diff(temperature_data)
}

arima_model <- auto.arima(temperature_data)
forecasted_values <- forecast(arima_model, h = 168) #7dnix24h

print(forecasted_values)

#autoplot(forecasted_values) +
#  ggtitle("Prognoza temperatury z modelem ARIMA") +
#  xlab("Czas") +
#  ylab("Temperatura")

#Model ETS

install.packages("forecast")
library(forecast)

temperature_data <- weather_data$temperature_2m
ets_model <- ets(temperature_data)
forecast_ets <- forecast(ets_model, h = 168)
print(forecast_ets)

#plot


# Klasyczny Prophet
install.packages("prophet")
library(prophet)
library(dplyr)

df_prophet <- weather_data %>% select(date, temperature_2m)
colnames(df_prophet) <- c("ds", "y")  # Prophet wymaga kolumny 'ds' (data) i 'y' (wartość)
model_prophet <- prophet(df_prophet)

#Prognoza 
future <- make_future_dataframe(model_prophet, periods = 168, freq = "hour")  # 7 dni po 24 godzinach
forecast <- predict(model_prophet, future)

print(forecas)
#plot(model_prophet, forecast)



