library(tidyverse)
library(tsibble)
library(lubridate)

# Read the unclean data
fires <- read_csv(here::here("data-raw", "amazon.csv"))

# Fixing the Problem Month
problem_term <- fires %>%
  filter(stringr::str_detect(month, "[^a-zA-Z]")) %>%
  slice(1) %>%
  pull(month)

fires_date <- fires %>%
  mutate(month_clean = case_when(
    month == "Abril"        ~ "April",
    month == "Agosto"       ~ "August",
    month == "Dezembro"     ~ "December",
    month == "Fevereiro"    ~ "February",
    month == "Janeiro"      ~ "January",
    month == "Julho"        ~ "July",
    month == "Junho"        ~ "June",
    month == "Maio"         ~ "May",
    month == problem_term     ~  "March",
    month == "Novembro"     ~ "November",
    month == "Outubro"      ~ "October",
    month == "Setembro"     ~ "September",
    TRUE                    ~ NA_character_
  ),
  day = 1) %>%
  mutate(date_month = lubridate::ymd(glue::glue("{year}-{month_clean}-{day}")))

# Creating the clean fire object
fires_clean <-
  fires_date %>%
  filter(!(state %in% c("Mato Grosso", "Rio", "Paraiba"))) %>%
  filter(duplicated(.) == FALSE) %>%
  as_tsibble(index = date_month, key = state) %>%
  select(-c(month, date, day)) %>%
  rename(date = date_month, month = month_clean, fires = number)

# Save the clean data
write_rds(fires_clean, here::here("data", "fires_clean.rds"))

