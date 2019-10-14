fires_clean_rds <- read_rds(here::here("data", "fires_clean.rds"))


fires_clean_rds %>%
  as_tibble() %>%
  count(date, wt = fires) %>%
  ggplot(aes(x = date, y = n))+
  geom_point()

fires_clean_rds %>%
  as_tibble() %>%
  group_by(state) %>%
  summarise(fires = sum(fires)) %>%
  mutate(state=forcats::fct_reorder(state,fires)) %>%
  ggplot(aes(x = state, y = fires))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggthemes :: theme_fivethirtyeight() +
  ggtitle("Fires in Brazilian States", subtitle = "1998 - 2017")
