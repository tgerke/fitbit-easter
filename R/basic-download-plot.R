# pak::pkg_install("teramonagi/fitbitr")
library(fitbitr)
library(tidyverse)
library(lubridate)

token <- fitbitr::oauth_token(
  key = Sys.getenv("FITBIT_KEY"),
  secret = Sys.getenv("FITBIT_SECRET")
)

dates_to_query <- 
  seq(as_date("2021-01-01"), as_date("2021-04-04"), by = "days")

dat <- map_dfr(
  dates_to_query, 
  ~ get_activity_intraday_time_series(
    token, 
    "steps", 
    .x, 
    detail_level="15min"
  )
)

dat %>%
  ggplot(
    aes(x = dataset_time, y = dataset_value, group = dateTime)
  ) + 
  geom_line()
