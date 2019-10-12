# Load Packages
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)

# Read in the data
fires_clean <- read_rds(here::here("data", "fires_clean.rds"))
fires_all <- read_rds(here::here("data", "fires_clean_all.rds"))

fires_all %>%
  gg_season(y = fires)

fires_all %>%
  gg_subseries(y = fires)

fires_all %>%
  gg_lag()

fires_all %>%
  ACF() %>%
  autoplot()

fires_all %>%
  ACF() %>%
  ggplot(aes(x = lag, y = acf)) +
  geom_line()

fires_all %>% feasts::STL(fires ~ season(window = Inf)) %>% autoplot()

aus_production %>% STL(Beer ~ season(window = Inf)) %>% autoplot()

?STL

fires_all %>%
  classical_decomposition() %>%
  autoplot()

fire_detrend <- fires_all %>%
  feasts::STL() %>%
  select(date, remainder) %>%
  rename(fires = remainder)


fire_detrend %>%
  filter(is.na(fires) == FALSE) %>%
  filter(is.nan(fires) == FALSE) %>%
  model(
    # ets = ETS(box_cox(fires, 0.3)),
    arima = ARIMA((fires)),
    snaive = SNAIVE(fires),
    linear = fable::TSLM(fires)
    ) %>%
  forecast(h = "2 years") %>%
  autoplot(filter(fire_detrend, lubridate::year(date) > 2012), level = NULL)


fires_all %>%
  select(date, fires) %>%
  filter(is.na(fires) == FALSE) %>%
  filter(is.nan(fires) == FALSE) %>%
  model(
    # ets = ETS(box_cox(fires, 0.3)),
    arima = ARIMA((fires)),
    snaive = SNAIVE(fires),
    linear = fable::TSLM(fires)
  ) %>%
  forecast(h = "2 years") %>%
  autoplot(filter(fires_all, lubridate::year(date) > 2012), level = NULL)



fire_detrend %>%
  ACF() %>%
  ggplot(aes(x = as.numeric(lag), y = acf)) +
  geom_point()

fire_detrend$fires %>%
  acf(lag.max = 400)

a <- fires_all %>%
  filter(is.na(fires) == FALSE) %>%
  filter(is.nan(fires) == FALSE) %>%
  model(
    ets = ETS(box_cox(fires, 0.3)),
    arima = ARIMA(fires),
    snaive = SNAIVE(fires)
  )

autoplot(
  fabletools::forecast(a, h = " years"),
  filter(fires_all, lubridate::year(date) > 2015),
  level = NULL
)

a$arima %>% View()
?fable::SNAIVE()
