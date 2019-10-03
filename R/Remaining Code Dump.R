#Other code that I did that hasn't been included here yet -James
fires_duplicates <- fires_clean %>%
  mutate(new_month = yearmonth(date_month)) %>%
  distinct(new_month, state, number, .keep_all = TRUE) %>%
  duplicates(index = new_month, key = state)

fires_duplicates
fires_clean %>%
  mutate(new_month = yearmonth(date_month)) %>%
  filter(year == 2001, month_clean == "January", state == "Mato Grosso")


fires_clean %>%
  mutate(new_month = yearmonth(date)) %>%
  arrange(date_month, state) %>% View()
duplicates(index = new_month, key = state) %>%

  fires_clean

fires_clean %>%
  count(date_month, wt = number) %>%
  ggplot(aes(x = date_month, y = n)) +
  geom_point() +
  geom_line()


library(tsibble)
library(feasts)

# After checking that there is uniform distribution of data accross month and
# year, it is deemed valid to put an arbitrary time index for each row (i.e. the
# observations are steadily incrementing through time)

fires_clean %>%
  count(date_month, wt = number) %>%
  rowid_to_column("index") %>%
  as_tsibble(index = index) %>%
  # tsibble::fill_gaps() %>%
  # visdat::vis_miss()
  feasts::ACF(n) %>%
  ggplot(aes(x = lag, y = acf)) +
  geom_point()


fires_clean %>%
  count(date_month, wt = number) %>%
  rowid_to_column("index") %>%
  as_tsibble(index = index) %>%
  # tsibble::fill_gaps() %>%
  # visdat::vis_miss()
  feasts::ACF(n) %>%
  autoplot()  +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20))

a <- fires_clean %>%
  count(date_month, wt = number)

ts(a$n) %>%
  acf(lag.max = 230)

fires_clean %>%
  count(date_month, wt = number) %>%
  rowid_to_column("index") %>%
  as_tsibble(index = index) %>%
  # tsibble::fill_gaps() %>%
  # visdat::vis_miss()
  feasts::ACF(n) %>%
  mutate(lag = as.numeric(lag)) %>%
  ggplot(aes(x = lag, y = acf)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf))


fires_clean %>%
  count(date_month, wt = number) %>%
  count(lubridate::day(date_month))


library(tsibbledata)
library(lubridate)
aus_production %>% ACF(Beer) %>% autoplot()




library(fable)
library(tsibble)
library(tsibbledata)
library(lubridate)
library(dplyr)
library(feasts)

a <- fires_clean %>%
  count(date_month, wt = number) %>%
  rowid_to_column("index") %>%
  as_tsibble(index = index)


a %>%
  feasts::classical_decomposition(n, )

a %>%
  model(
    # ets = ETS(box_cox(n, 0.3)),
    arima = ARIMA(n),
  ) %>%
  forecast(h = 200) %>%
  autoplot(a, level = NULL)


ts(a$n) %>% forecast::auto.arima() %>% plot
