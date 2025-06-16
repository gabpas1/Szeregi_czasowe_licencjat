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
#install.packages("padr")
#py_install(c("neuralprophet", "torch", "pytorch_lightning"), pip = TRUE)
#reticulate::py_install("neuralprophet", upgrade = TRUE)
#reticulate::py_install("ipython", pip = TRUE)
#install.packages("plotly")

# Ustawienie Pythona dla reticulate
library(reticulate)
use_python(Sys.which("python3"))
py_config()
if ("r-reticulate" %in% reticulate::virtualenv_list()) {
  reticulate::use_virtualenv("r-reticulate", required = TRUE)
  cat("Użyto istniejącego środowiska wirtualnego 'r-reticulate'.\n")
} else {
  warning("Środowisko wirtualne 'r-reticulate' nie zostało znalezione. \nUpewnij się, że zostało ono utworzone za pomocą: \nreticulate::virtualenv_create('r-reticulate')")
}

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
library(plotly)
library(padr)

# ------------------------------------------------------------------------------
# 2. Pobieranie danych pogodowych z API Open-Meteo
# ------------------------------------------------------------------------------

url <- "https://archive-api.open-meteo.com/v1/archive"
latitude <- 52.4064  # Szerokość geograficzna Poznania
longitude <- 16.9252 # Długość geograficzna Poznania
start_date <- "2014-01-01"
end_date <- "2024-12-31"
hourly_data <- "temperature_2m,relative_humidity_2m,precipitation,soil_temperature_0_to_7cm"

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
    
    if (is.null(data$hourly)) {
      stop("Odpowiedź z API nie zawiera oczekiwanych danych godzinowych ('data$hourly').")
    }
    
    df <- as.data.frame(data$hourly)
    
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

weather_data <- get_weather_data(latitude, longitude, start_date, end_date, hourly_data)

print("Pierwsze wiersze pobranych danych:")
print(head(weather_data))

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
  mutate(year_month = floor_date(date, "month")) %>%
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

kolory <- list(
  wilgotnosc = "mediumseagreen",
  opady = "deepskyblue3",
  temperatura = "firebrick2",
   gleba = "sienna3",
  t_plus = "tomato3",
  t_minus = "dodgerblue4"
)

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
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot_temp_monthly)

plot_precipitation_monthly <- ggplot(monthly_data, aes(x = year_month, y = total_precipitation)) +
  geom_bar(stat = "identity") +
  labs(title = "Suma miesięcznych opadów w Poznaniu (2014-2024)", x = "Miesiąc", y = "Suma opadów (mm)") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
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
  geom_bar(stat = "identity", fill = kolory$temperatura) + 
  labs(title = "Średnia roczna temperatura w Poznaniu (2014-2024)", x = "Rok", y = "Średnia temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
yearly_temp_plot

# Roczna suma opadów 
yearly_precip_plot <- ggplot(yearly_data, aes(x = year, y = total_precipitation)) +
  geom_bar(stat = "identity", fill = kolory$opady ) +
  labs(title = "Roczna suma opadów w Poznaniu (2014-2024)", x = "Rok", y = "Suma opadów (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
yearly_precip_plot

# Minimalna roczna temperatura 

yearly_min_temp_plot <- ggplot(yearly_data, aes(x = year, y = min_temp)) +
  geom_bar(stat = "identity", fill = kolory$t_minus ) +
  labs(title = "Minimalna roczna temperatura w Poznaniu (2014-2024)", x = "Rok", y = "Minimalna temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
yearly_min_temp_plot

# Maksymalna roczna temperatura
yearly_max_temp_plot <- ggplot(yearly_data, aes(x = year, y = max_temp)) +
  geom_bar(stat = "identity", fill = kolory$t_plus) +
  labs(title = "Maksymalna roczna temperatura w Poznaniu (2014-2024)", x = "Rok", y = "Maksymalna temperatura (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
yearly_max_temp_plot

# Średnia roczna wilgotność względna
yearly_humidity_plot <- ggplot(yearly_data, aes(x = year, y = avg_humidity)) +
  geom_bar(stat = "identity", fill = kolory$wilgotnosc ) +
  labs(title = "Średnia roczna wilgotność względna w Poznaniu (2014-2024)", x = "Rok", y = "Średnia wilgotność (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
yearly_humidity_plot

# Średnia roczna temperatura gleby
yearly_soil_temp_plot <- ggplot(yearly_data, aes(x = year, y = avg_soil_temp)) +
  geom_bar(stat = "identity", fill = kolory$gleba) +
  labs(title = "Średnia roczna temperatura gleby (0-7 cm) w Poznaniu (2014-2024)", x = "Rok", y = "Średnia temperatura gleby (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
yearly_soil_temp_plot

# ------------------------------------------------------------------------------
# 6. Modelowanie szeregów czasowych - NeuralProphet
# ------------------------------------------------------------------------------

np <- reticulate::import("neuralprophet")
pd <- reticulate::import("pandas", convert = FALSE)

# Przygotowanie danych
df_prophet_np <- weather_data %>%
  select(date, temperature_2m, relative_humidity_2m, precipitation) %>%
  rename(ds = date, y = temperature_2m, relative_humidity = relative_humidity_2m, precipitation = precipitation)

# Konwersja na pandas DataFrame
df_prophet_np_pd <- reticulate::r_to_py(df_prophet_np)
df_prophet_np_pd$ds <- pd$to_datetime(df_prophet_np_pd$ds)

# Funkcja tworząca i zwracająca nowy model z regresorami
create_model <- function() {
  model <- np$NeuralProphet(
    growth = "linear",
    yearly_seasonality = TRUE,
    weekly_seasonality = TRUE,
    daily_seasonality = TRUE,
    quantiles = c(0.025, 0.975)
  )
  model$add_future_regressor("relative_humidity")
  model$add_future_regressor("precipitation")
  return(model)
}

# Utworzenie modelu i "trening"
model_np <- create_model()
metrics_np <- model_np$fit(df_prophet_np_pd, freq = "H")
print(metrics_np)

# Tworzenie przyszłych dat (168 godzin do przodu)
last_date <- max(df_prophet_np$ds)
future_dates <- seq.POSIXt(from = as.POSIXct(last_date) + 3600, by = "hour", length.out = 168)

# Przyszłye daty z regresorami (tu średnie wartości)
future_regressors_df <- data.frame(
  ds = future_dates,
  relative_humidity = mean(df_prophet_np$relative_humidity, na.rm = TRUE),
  precipitation = mean(df_prophet_np$precipitation, na.rm = TRUE)
)

# Połączenie danych historycznych i przyszłych
df_all <- bind_rows(df_prophet_np, future_regressors_df)
df_all_pd <- reticulate::r_to_py(df_all)
df_all_pd$ds <- pd$to_datetime(df_all_pd$ds)

# Predykcja
forecast_np_pd <- model_np$predict(df_all_pd)
forecast_np <- reticulate::py_to_r(forecast_np_pd)

print("Prognoza temperatury na najbliższe 7 dni (godzinowo) przy użyciu modelu NeuralProphet z regresorami:")
print(head(forecast_np))
# Oddzielenie danych prognozowanych
forecast_only <- forecast_np %>%
  filter(ds >= min(future_dates))

# Wizualizacja - Prognoza temperatury
forecast_plot <- ggplot(forecast_only, aes(x = ds, y = yhat1)) +
  geom_line(color = "red") +
  labs(title = "Prognoza temperatury na kolejne 7 dni (godzinowo)", x = "Data", y = "Prognozowana temperatura (°C)") +
  theme_minimal()
print(forecast_plot)

# Prognoza z niepewnością (poprawione nazwy kolumn)
if ("yhat1 2.5%" %in% names(forecast_only) && "yhat1 97.5%" %in% names(forecast_only)) {
  forecast_uncertainty_plot <- ggplot(forecast_only, aes(x = ds)) +
    geom_line(aes(y = yhat1), color = "blue") +
    geom_ribbon(aes(ymin = `yhat1 2.5%`, ymax = `yhat1 97.5%`), alpha = 0.2, fill = "lightblue") +
    labs(title = "Prognoza temperatury z przedziałem niepewności", x = "Data", y = "Temperatura (°C)") +
    theme_minimal()
  print(forecast_uncertainty_plot)
}

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

temperature_data_arima <- weather_data$temperature_2m

print("Automatyczne dopasowywanie modelu ARIMA...")
arima_model <- auto.arima(
  temperature_data_arima,
  seasonal = TRUE, # Pozwalamy na modelowanie sezonowości, co jest kluczowe dla danych godzinowych
  stepwise = TRUE, # Użyj przybliżenia stepwise dla szybkości
  trace = TRUE     # Pokaż kroki dopasowywania modelu
)

print("Wynikowy model ARIMA:")
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

# Wizualizacja - tytuł wykresu można uprościć
plot(forecasted_values,
     main = "Prognoza Temperatury (model auto.arima)",
     xlab = "Czas",
     ylab = "Temperatura (°C)"
)

# ------------------------------------------------------------------------------
# 9. Modelowanie szeregów czasowych - Model ETS
# ------------------------------------------------------------------------------

# Sprawdzenie równomierności danych przed ETS
print("Sprawdzanie i uzupełnianie regularności szeregu czasowego...")
weather_data_regular <- weather_data %>%
  pad(interval = "hour")

if (nrow(weather_data_regular) > nrow(weather_data)) {
  print(paste("Dodano", nrow(weather_data_regular) - nrow(weather_data), "brakujących obserwacji."))
  weather_data_regular <- weather_data_regular %>%
    fill_by_prevalent(temperature_2m) 
}

if(any(is.na(weather_data_regular$temperature_2m))) {
  warning("W danych temperatury wciąż istnieją braki. Użyto zoo::na.locf.")
  weather_data_regular$temperature_2m <- zoo::na.locf(weather_data_regular$temperature_2m, na.rm = FALSE)
}

temperature_data_ts <- ts(weather_data_regular$temperature_2m, frequency = 168)

print("Dopasowywanie modelu ETS...")
ets_model <- ets(temperature_data_ts)
print(summary(ets_model))
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