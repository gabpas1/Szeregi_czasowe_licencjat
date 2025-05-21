# ------------------------------------------------------------------------------
# Praca Licencjacka - Analiza Danych Pogodowych 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 1.Instalacja i ładowanie niezbędnych pakietów
# ------------------------------------------------------------------------------

# Instalacja pakietów
# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("httr")
# install.packages("dplyr")
# install.packages("jsonlite")
# install.packages("lubridate")
# install.packages("forecast")
# install.packages("urca")
# install.packages("prophet")
# install.packages("reticulate")
# reticulate::install_python()
# reticulate::py_install("neuralprophet", upgrade = TRUE)

# Ustawienie Pythona dla reticulate
library(reticulate)
use_python("/Library/Frameworks/Python.framework/Versions/3.13/bin/python3")
py_config()

# Ładowanie bibliotek
library(ggplot2)
library(ggfortify)
library(httr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(forecast)
library(urca)
library(prophet)
#library(neuralprophet)

# ------------------------------------------------------------------------------
# 2. Pobieranie danych pogodowych z API Open-Meteo
# ------------------------------------------------------------------------------

# Definiowanie parametrów zapytania do API 
url <- "https://archive-api.open-meteo.com/v1/archive"
latitude <- 52.4064  # Szerokość geograficzna Poznania
longitude <- 16.9252 # Długość geograficzna Poznania
start_date <- "2014-01-01"
end_date <- "2024-12-31"
hourly_data <- "temperature_2m,relative_humidity_2m,precipitation,soil_temperature_0_to_7cm"

# Funkcja do pobierania danych pogodowych
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

# Pobranie danych pogodowych
weather_data <- get_weather_data(latitude, longitude, start_date, end_date, hourly_data)

# Wyświetlenie pierwszych wierszy pobranych danych
print("Pierwsze wiersze pobranych danych:")
print(head(weather_data))

# ------------------------------------------------------------------------------
# 3. Analiza opisowa danych
# ------------------------------------------------------------------------------

print("Podsumowanie statystyczne danych godzinowych:")
summary(weather_data)


# ------------------------------------------------------------------------------
# 4. Agregacja danych
# ------------------------------------------------------------------------------

# Agregacja miesięczna 
monthly_data <- weather_data %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(
    avg_temp = mean(temperature_2m, na.rm = TRUE),
    min_temp = min(temperature_2m, na.rm = TRUE),
    max_temp = max(temperature_2m, na.rm = TRUE),
    avg_humidity = mean(relative_humidity_2m, na.rm = TRUE),
    total_precipitation = sum(precipitation, na.rm = TRUE),
    avg_soil_temp = mean(soil_temperature_0_to_7cm, na.rm = TRUE)
  )

print("Miesięczne statystyki:")
print(head(monthly_data))

# Agregacja roczna

yearly_data <- weather_data %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(
    avg_temp = mean(temperature_2m, na.rm = TRUE),
    min_temp = min(temperature_2m, na.rm = TRUE),
    max_temp = max(temperature_2m, na.rm = TRUE),
    avg_humidity = mean(relative_humidity_2m, na.rm = TRUE),
    total_precipitation = sum(precipitation, na.rm = TRUE),
    avg_soil_temp = mean(soil_temperature_0_to_7cm, na.rm = TRUE)
  )

print("Roczne statystyki:")
print(head(yearly_data))

# ------------------------------------------------------------------------------
# 5. Wizualizacja danych
# ------------------------------------------------------------------------------

# 5.1. Wykresy przebiegu czasowego (godzinowo)
print("Wykresy godzinowe:")

plot_temp_hourly <- ggplot(weather_data, aes(x = date, y = temperature_2m)) +
  geom_line() +
  labs(title = "Godzinowa temperatura w Poznaniu (2014-2024)", x = "Data", y = "Temperatura (°C)") +
  theme_minimal()
print(plot_temp_hourly)

plot_humidity_hourly <- ggplot(weather_data, aes(x = date, y = relative_humidity_2m)) +
  geom_line() +
  labs(title = "Godzinowa wilgotność względna w Poznaniu (2014-2024)", x = "Data", y = "Wilgotność (%)") +
  theme_minimal()
print(plot_humidity_hourly)

plot_precipitation_hourly <- ggplot(weather_data, aes(x = date, y = precipitation)) +
  geom_line() +
  labs(title = "Godzinowe opady w Poznaniu (2014-2024)", x = "Data", y = "Opady (mm)") +
  theme_minimal()
print(plot_precipitation_hourly)

plot_soil_temp_hourly <- ggplot(weather_data, aes(x = date, y = soil_temperature_0_to_7cm)) +
  geom_line() +
  labs(title = "Godzinowa temperatura gleby (0-7 cm) w Poznaniu (2014-2024)", x = "Data", y = "Temperatura gleby (°C)") +
  theme_minimal()
print(plot_soil_temp_hourly)

# 5.2. Wykresy przebiegu czasowego (miesięcznie)
print("Wykresy miesięczne:")

plot_temp_monthly <- ggplot(monthly_data, aes(x = year_month, y = avg_temp)) +
  geom_line(group = 1) +
  geom_point() +
  labs(title = "Średnia miesięczna temperatura w Poznaniu (2014-2024)", x = "Miesiąc", y = "Średnia temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot_temp_monthly)

plot_precipitation_monthly <- ggplot(monthly_data, aes(x = year_month, y = total_precipitation)) +
  geom_bar(stat = "identity") +
  labs(title = "Suma miesięcznych opadów w Poznaniu (2014-2024)", x = "Miesiąc", y = "Suma opadów (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot_precipitation_monthly)

# 5.3. Dane i wykresy roczne
print("Dane i wykresy roczne:")

# Najcieplejszy roku:
hottest_year <- yearly_data %>% arrange(desc(avg_temp)) %>% head(1)
print(paste("Najcieplejszy rok:", hottest_year$year, "ze średnią temperaturą:", round(hottest_year$avg_temp, 2), "°C"))

# Rok z nawiększymi opadami:
wettest_year <- yearly_data %>% arrange(desc(total_precipitation)) %>% head(1)
print(paste("Rok z największą sumą opadów:", wettest_year$year, "z sumą opadów:", round(wettest_year$total_precipitation, 2), "mm"))

# Średnia roczna temperatura 
yearly_temp_plot <- ggplot(yearly_data, aes(x = year, y = avg_temp)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Średnia roczna temperatura w Poznaniu (2014-2024)", x = "Rok", y = "Średnia temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(yearly_temp_plot)

# Roczna suma opadów 
yearly_precip_plot <- ggplot(yearly_data, aes(x = year, y = total_precipitation)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Roczna suma opadów w Poznaniu (2014-2024)", x = "Rok", y = "Suma opadów (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(yearly_precip_plot)

# Minimalna roczna temperatura 

yearly_min_temp_plot <- ggplot(yearly_data, aes(x = year, y = min_temp)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Minimalna roczna temperatura w Poznaniu (2014-2024)", x = "Rok", y = "Minimalna temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(yearly_min_temp_plot)

# Maksymalna roczna temperatura
yearly_max_temp_plot <- ggplot(yearly_data, aes(x = year, y = max_temp)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Maksymalna roczna temperatura w Poznaniu (2014-2024)", x = "Rok", y = "Maksymalna temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(yearly_max_temp_plot)

# Średnia roczna wilgotność względna
yearly_humidity_plot <- ggplot(yearly_data, aes(x = year, y = avg_humidity)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Średnia roczna wilgotność względna w Poznaniu (2014-2024)", x = "Rok", y = "Średnia wilgotność (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(yearly_humidity_plot)

# Średnia roczna temperatura gleby
yearly_soil_temp_plot <- ggplot(yearly_data, aes(x = year, y = avg_soil_temp)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Średnia roczna temperatura gleby (0-7 cm) w Poznaniu (2014-2024)", x = "Rok", y = "Średnia temperatura gleby (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(yearly_soil_temp_plot)

# ------------------------------------------------------------------------------
# 6. Modelowanie szeregów czasowych - NeuralProphet Nie skończony :(
# ------------------------------------------------------------------------------

np <- import("neuralprophet")

df_prophet <- weather_data %>% select(date, temperature_2m, relative_humidity_2m, precipitation)
colnames(df_prophet) <- c("ds", "y", "relative_humidity_2m", "precipitation")

model_np_regressors <- np$NeuralProphet(
  yearly_seasonality = TRUE,
  weekly_seasonality = TRUE,
  regressors = c("relative_humidity_2m", "precipitation")
)
metrics_regressors <- model_np_regressors$fit(df_prophet, freq = "H")

# ------------------------------------------------------------------------------
# 7. Modelowanie szeregów czasowych - Klasyczny Prophet
# ------------------------------------------------------------------------------

df_prophet <- weather_data %>% select(date, temperature_2m)
colnames(df_prophet) <- c("ds", "y")  # Prophet wymaga kolumny 'ds' (data) i 'y' (wartość)
model_prophet <- prophet(df_prophet)

#Prognoza
future <- make_future_dataframe(model_prophet, periods = 168, freq = "hour")
forecast <- predict(model_prophet, future)


print("Prognoza temperatury na najbliższe 7 dni (godzinowo) przy użyciu modelu Prophet:")
print(head(forecast))

# Wykres prognozy z danymi historycznymi
plot_forecast_prophet <- plot(model_prophet, forecast)
print(plot_forecast_prophet)

# Wykres prognozy bez danych historycznych
last_historical_date <- max(df_prophet$ds)
start_forecast_date <- last_historical_date + hours(1)


forecast_future <- forecast %>%
  filter(ds >= start_forecast_date)


plot_future_forecast <- ggplot(forecast_future, aes(x = ds, y = yhat)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "lightblue", alpha = 0.3) +
  labs(
    title = "Prognoza temperatury na najbliższe 7 dni (bez danych historycznych)",
    x = "Data",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()

print(plot_future_forecast)

# Trend i sezonowość (roczna, tygodniowa, dobowa)
plot_components_prophet <- prophet_plot_components(model_prophet, forecast)
print(plot_components_prophet)

# ------------------------------------------------------------------------------
# 8. Modelowanie szeregów czasowych - ARIMA
# ------------------------------------------------------------------------------

weather_data$date <- as.Date(weather_data$date)
temperature_data <- weather_data$temperature_2m

# Test ADF z różnymi typami i liczbą opóźnień
adf_test_none <- ur.df(temperature_data, type = "none", lags = 12)
print("Test ADF (brak stałej i trendu):")
print(summary(adf_test_none))

adf_test_drift <- ur.df(temperature_data, type = "drift", lags = 12)
print("Test ADF (ze stałą):")
print(summary(adf_test_drift))

adf_test_trend <- ur.df(temperature_data, type = "trend", lags = 12)
print("Test ADF (ze stałą i trendem):")
print(summary(adf_test_trend))

# Funkcja sprawdzająca stacjonarność
is_stationary <- function(adf_result, alpha = 0.05) {
  cval <- adf_result@cval

  if (is.null(rownames(cval))) {
    warning("Brak nazw poziomów istotności w adf_result@cval.")
    return(FALSE)
  }
  
  available_alphas <- as.numeric(gsub("%", "", rownames(cval))) / 100
  closest_index <- which.min(abs(available_alphas - alpha))
  
  # Sprawdzenie poprawności indeksu i wartości
  if (length(closest_index) == 0 || is.na(adf_result@teststat[1])) {
    warning("Brak odpowiedniego poziomu istotności lub wartości testu ADF.")
    return(FALSE)
  }
  
  critical_value <- cval[closest_index, 1]
  test_stat <- adf_result@teststat[1]
  
  return(test_stat < critical_value)
}

# Przygotowanie do różnicowania
temp_data_stationary <- temperature_data
diff_count <- 0
max_diff <- 2  # Maksymalna liczba różnicowań

# Automatyczne różnicowanie aż do stacjonarności lub do limitu
while (!is_stationary(ur.df(na.omit(temp_data_stationary), type = "drift", lags = 12)) &&
       diff_count < max_diff) {
  
  print(paste("ADF test, różnicowanie =", diff_count))
  
  temp_data_stationary <- diff(temp_data_stationary)
  temp_data_stationary <- na.omit(temp_data_stationary)
  diff_count <- diff_count + 1
  
  print(paste("Dane zostały zróżnicowane po raz", diff_count))
}

# Komunikat końcowy
if (diff_count == 0) {
  print("Dane są prawdopodobnie stacjonarne.")
} else {
  print(paste("Ostateczny poziom różnicowania:", diff_count))
}

# Automatyczne dopasowanie modelu ARIMA
arima_model <- auto.arima(temp_data_stationary)
print(summary(arima_model))

# Diagnostyka reszt
par(mar = c(4, 4, 2, 1))  # dolny, lewy, górny, prawy
tsdiag(arima_model)


# Prognozowanie
forecasted_values <- forecast::forecast(arima_model, h = 168)
print(forecasted_values)

# Wizualizacja
plot(forecasted_values,
     main = paste("Prognoza Temperatury (różnicowanie =", diff_count, ")"),
     xlab = "Czas",
     ylab = "Temperatura (°C)",
     col = "blue",
     fcol = "red",
     flwd = 2,
     shadecols = "lightgray",
     cex.main = 1.2,
     cex.lab = 1.1,
     cex.axis = 1.0,
     lwd = 1.5,
     ylim = range(c(forecasted_values$lower, forecasted_values$upper, temperature_data)) # Dostosowanie zakresu Y
)


# ------------------------------------------------------------------------------
# 9. Modelowanie szeregów czasowych - Model ETS
# ------------------------------------------------------------------------------

temperature_data_ts <- ts(weather_data$temperature_2m, frequency = 24)

ets_model <- ets(temperature_data_ts)
forecast_ets <- forecast(ets_model, h = 168)

# Ramka danych dla prognozy ETS
forecast_ets_df <- data.frame(
  date = seq(from = tail(weather_data$date, 1) + hours(1), by = "hour", length.out = 168),
  forecast = as.numeric(forecast_ets$mean),
  lower_80 = as.numeric(forecast_ets$lower[, "80%"]),
  upper_80 = as.numeric(forecast_ets$upper[, "80%"]),
  lower_95 = as.numeric(forecast_ets$lower[, "95%"]),
  upper_95 = as.numeric(forecast_ets$upper[, "95%"])
)

# Zakresu dat dla wykresu - skupienie na ostatnich danych i prognozie
last_history_date <- tail(weather_data$date, 1)
start_plot_date <- last_history_date - days(30) #ostatnie 30 dni historii
end_plot_date <- tail(forecast_ets_df$date, 1)

# Kolumny w weather_data muszą być klasy POSIXct
if (!inherits(weather_data$date, "POSIXct")) {
  weather_data$date <- as.POSIXct(weather_data$date)
}

if (!inherits(forecast_ets_df$date, "POSIXct")) {
  forecast_ets_df$date <- as.POSIXct(forecast_ets_df$date)
}

if (!inherits(start_plot_date, "POSIXct")) {
  start_plot_date <- as.POSIXct(start_plot_date)
}
if (!inherits(end_plot_date, "POSIXct")) {
  end_plot_date <- as.POSIXct(end_plot_date)
}

plot_ets_zoomed <- ggplot() +
  geom_line(data = subset(weather_data, date >= start_plot_date & date <= last_history_date),
            aes(x = date, y = temperature_2m), color = "blue", linewidth = 0.5) +
  geom_line(data = forecast_ets_df, aes(x = date, y = forecast), color = "red", linewidth = 1) + # Wyróżniona linia prognozy
  geom_ribbon(data = forecast_ets_df, aes(x = date, ymin = lower_80, ymax = upper_80), fill = "lightgreen", alpha = 0.4) +
  geom_ribbon(data = forecast_ets_df, aes(x = date, ymin = lower_95, ymax = upper_95), fill = "green", alpha = 0.3) +
  labs(
    title = "Prognoza Temperatury (ETS) - Przybliżenie",
    x = "Data",
    y = "Temperatura (°C)"
  ) +
  scale_x_datetime(limits = c(start_plot_date, end_plot_date), date_breaks = "3 days", date_labels = "%Y-%m-%d %H:%M") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(plot_ets_zoomed)