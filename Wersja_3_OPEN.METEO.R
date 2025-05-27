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
#reticulate::py_install("neuralprophet", upgrade = TRUE)

# Ustawienie Pythona dla reticulate
library(reticulate)
use_python(Sys.which("python3"))
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
library(neuralprophet)

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
    
    # Zmiana: Sprawdzenie, czy 'data' i 'data$hourly' istnieją
    if (is.null(data$hourly)) {
      stop("Odpowiedź z API nie zawiera oczekiwanych danych godzinowych ('data$hourly').")
    }
    
    df <- as.data.frame(data$hourly)
    
    # Zmiana: Walidacja formatu daty
    tryCatch({
      df$date <- ymd_hm(df$time)
    }, error = function(e) {
      stop("Nieprawidłowy format daty w danych z API. Sprawdź strukturę 'df$time'.")
    })
    
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

# Sprawdzenie brakujących danych na wczesnym etapie
print("Liczba brakujących wartości w każdej kolumnie po pobraniu danych:")
print(sapply(weather_data, function(x) sum(is.na(x))))

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
# 6. Modelowanie szeregów czasowych - NeuralProphet Nieskończony :(
# ------------------------------------------------------------------------------

np <- reticulate::import("neuralprophet")
pd <- reticulate::import("pandas", convert = FALSE)

df_prophet_np <- weather_data %>%
  select(date, temperature_2m, relative_humidity_2m, precipitation) %>%
  rename(ds = date, y = temperature_2m, relative_humidity = relative_humidity_2m, precipitation = precipitation)

# Przekonwertuj DataFrame R na DataFrame Pandas
df_prophet_np_pd <- reticulate::r_to_py(df_prophet_np)

# Jawna konwersja kolumny 'ds' na datetime64[ns]
df_prophet_np_pd$ds <- pd$to_datetime(df_prophet_np_pd$ds)

# Inicjalizacja i dopasowanie modelu NeuralProphet z regresorami
model_np <- np$NeuralProphet(
  growth = "linear",
  yearly_seasonality = TRUE,
  weekly_seasonality = TRUE,
  daily_seasonality = TRUE
)
# Dodanie regresorów
model_np$add_regressor("relative_humidity", standardize = "auto")
model_np$add_regressor("precipitation", standardize = "auto")

metrics_np <- model_np$fit(df_prophet_np_pd, freq = "H")
print(metrics_np)
# Tworzenie przyszłych danych
future_np_pd <- model_np$make_future_dataframe(df_prophet_np_pd, periods = 168)

# Prognozowanie
forecast_np_pd <- model_np$predict(future_np_pd)

# Konwersja wyników Pandas DataFrame z powrotem do R DataFrame
forecast_np <- reticulate::py_to_r(forecast_np_pd)

print("Prognoza temperatury na najbliższe 7 dni (godzinowo) przy użyciu modelu NeuralProphet z regresorami:")
print(head(forecast_np))

# Wizualizacja
plot_forecast_np <- np$plot(forecast_np_pd)
print(plot_forecast_np)

plot_components_np <- np$plot_components(forecast_np_pd)
print(plot_components_np)

# ------------------------------------------------------------------------------
# 7. Modelowanie szeregów czasowych - Klasyczny Prophet
# ------------------------------------------------------------------------------

df_prophet_p <- weather_data %>% select(date, temperature_2m)
colnames(df_prophet_p) <- c("ds", "y")  # Prophet wymaga kolumny 'ds' (data) i 'y' (wartość)
model_prophet <- prophet(df_prophet_p)

#Prognoza
future_p <- make_future_dataframe(model_prophet, periods = 168, freq = "hour")
forecast_p <- predict(model_prophet, future_p)


print("Prognoza temperatury na najbliższe 7 dni (godzinowo) przy użyciu modelu Prophet:")
print(head(forecast_p))

# Wykres prognozy z danymi historycznymi
plot_forecast_prophet <- plot(model_prophet, forecast_p)
print(plot_forecast_prophet)

# Wykres prognozy bez danych historycznych
last_historical_date_p <- max(df_prophet_p$ds)
start_forecast_date_p <- last_historical_date_p + hours(1)


forecast_future_p <- forecast_p %>%
  filter(ds >= start_forecast_date_p)

# Trend i sezonowość (roczna, tygodniowa, dobowa)
plot_components_prophet <- prophet_plot_components(model_prophet, forecast_p)
print(plot_components_prophet)

# ------------------------------------------------------------------------------
# 8. Modelowanie szeregów czasowych - ARIMA
# ------------------------------------------------------------------------------

# Zmiana:Nie konwertuję daty na Date, zachowujemy POSIXct
temperature_data_arima <- weather_data$temperature_2m

# Funkcja sprawdzająca stacjonarność (z automatycznym wyborem opóźnień)
is_stationary_adf <- function(ts, alpha = 0.05) {
  if (length(na.omit(ts)) < 2) {
    warning("Zbyt mało danych do przeprowadzenia testu ADF.")
    return(FALSE) # Uznaję za niestacjonarne, aby uniknąć błędów
  }
  adf_result <- tryCatch({
    ur.df(ts, type = "drift", selectlags = "AIC")
  }, error = function(e) {
    warning(paste("Błąd podczas wykonywania ur.df:", e$message))
    return(NULL) # W przypadku błędu zwracam NULL
  })
  
  if (is.null(adf_result) || any(is.na(adf_result@teststat)) || is.null(adf_result@cval)) {
    warning("Nie można było przeprowadzić testu ADF lub brak wyników.")
    return(FALSE) # Uznaję za niestacjonarne
  }
  
  critical_value_row <- rownames(adf_result@cval) == paste0(alpha * 100, "%")
  if (!any(critical_value_row)) {
    warning(paste("Nie znaleziono wartości krytycznej dla alpha =", alpha))
    return(FALSE) # Uznaję za niestacjonarne
  }
  critical_value <- adf_result@cval[critical_value_row, 1]
  test_stat <- adf_result@teststat[1]
  
  return(test_stat < critical_value)
}

# Przygotowanie do różnicowania
temp_data_stationary <- temperature_data_arima
diff_count <- 0
max_diff <- 2  # Maksymalna liczba różnicowań

# Automatyczne różnicowanie aż do stacjonarności lub do limitu
while (!is_stationary_adf(na.omit(temp_data_stationary)) && diff_count < max_diff) {
  print(paste("Test ADF (AIC), różnicowanie =", diff_count))
  temp_data_stationary <- diff(temp_data_stationary)
  temp_data_stationary <- na.omit(temp_data_stationary)
  diff_count <- diff_count + 1
  print(paste("Dane zostały zróżnicowane po raz", diff_count))
}

# Zmiana: Ostrzeżenie, jeśli nie osiągnięto stacjonarności
if (!is_stationary_adf(na.omit(temp_data_stationary))) {
  warning("Nie osiągnięto stacjonarności szeregu czasowego temperatury po maksymalnie ", max_diff, " różnicowaniach (test ADF z AIC).")
} else if (diff_count == 0) {
  print("Dane są prawdopodobnie stacjonarne (test ADF z AIC).")
} else {
  print(paste("Ostateczny poziom różnicowania:", diff_count, "(test ADF z AIC)."))
}

# Automatyczne dopasowanie modelu ARIMA
# Zmiana (Sugestia 5b): Nie ustawiamy stałej liczby opóźnień w ur.df, auto.arima sam dobierze p, d, q
arima_model <- auto.arima(temp_data_stationary)
print(summary(arima_model))

# Diagnostyka reszt
par(mar = c(4, 4, 2, 1))  # dolny, lewy, górny, prawy
tsdiag(arima_model)

# Dodatkowa analiza reszt
# Test Ljung-Box na autokorelację reszt (do 24 opóźnień dla danych godzinowych)
box_test_result <- Box.test(arima_model$residuals, lag = 24, type = "Ljung-Box")
print("Test Ljung-Box na autokorelację reszt:")
print(box_test_result)

# Wizualizacja ACF reszt
acf(arima_model$residuals, main = "Autokorelacja Reszt ARIMA")

# Wizualizacja PACF reszt
pacf(arima_model$residuals, main = "Częściowa Autokorelacja Reszt ARIMA")

# Histogram reszt
hist(arima_model$residuals, main = "Histogram Reszt ARIMA", xlab = "Reszty")


# Prognozowanie
forecasted_values <- forecast::forecast(arima_model, h = 168)
print(forecasted_values)

# Wizualizacja
plot(forecasted_values,
     main = paste("Prognoza Temperatury (ARIMA, różnicowanie =", diff_count, ")"),
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
     ylim = range(c(forecasted_values$lower, forecasted_values$upper, temperature_data_arima)) # Dostosowanie zakresu Y
)

# ------------------------------------------------------------------------------
# 9. Modelowanie szeregów czasowych - Model ETS
# ------------------------------------------------------------------------------

# Zmiana: Sprawdzenie równomierności danych przed ETS
time_diffs <- diff(weather_data$date)
unique_diffs <- unique(time_diffs)
print("Unikalne interwały czasowe między obserwacjami (dla ETS):")
print(unique_diffs)

expected_interval <- as.difftime(1, units = "hours")
tolerance <- as.difftime(1, units = "mins") # Poprawna jednostka: "mins"

if (length(unique_diffs) > 1 || any(abs(unique_diffs - expected_interval) > tolerance)) {
  warning("Dane dla modelu ETS wydają się nierównomiernie rozłożone w czasie lub mają nietypowe interwały. Rozważ ich przetworzenie.")
}

# Sprawdzenie i ewentualna imputacja brakujących danych
if(any(is.na(weather_data$temperature_2m))) {
  warning("W danych temperatury wykryto brakujące wartości. Zastosowano prostą imputację (ostatnia obserwacja przeniesiona).")
  weather_data$temperature_2m <- zoo::na.locf(weather_data$temperature_2m)
}

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

# Zakres dat dla wykresu - skupienie na ostatnich danych i prognozie
last_history_date <- tail(weather_data$date, 1)
start_plot_date <- last_history_date - days(30) #ostatnie 30 dni historii
end_plot_date <- tail(forecast_ets_df$date, 1)

# Upewnienie się, że kolumny dat są w odpowiednim formacie
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