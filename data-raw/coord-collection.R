library(tidyverse)
library(ggmap)

fires_clean <- read_rds(here::here("data", "fires_clean.rds"))

state_list <- fires_clean %>%
  pull(state) %>%
  unique() %>%
  stringi::stri_enc_toutf8()

problem_state <- state_list[[12]]

state_list[[12]] <- "Para" # This page has both terms on it so Para seems like a logical guess https://ccsearch.creativecommons.org/photos/2a1131c1-9ccf-4b65-b550-6d1e5e974795




state_list_clean <- state_list %>%
  map_chr(paste, " Brazil")

state_coords <- map_df(state_list_clean, ggmap::geocode)

write_rds(state_coords, here::here("data", "state_coords.rds"))
