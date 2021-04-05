# setup -------------------------------------------------------------------

# pak::pkg_install("teramonagi/fitbitr")
library(fitbitr)
library(tidyverse)
library(lubridate)
library(ggtext)

# load --------------------------------------------------------------------

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


# plot --------------------------------------------------------------------

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_family = "Fira Sans Condensed",
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_markdown(face = "plain"),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown(),
          plot.title.position = "plot")
)

time_labels <- tribble(
  ~minute, ~label,
  0, "12am",
  3*60, "3am",
  6*60, "6am",
  9*60, "9am",
  12*60, "12pm",
  15*60, "3pm",
  18*60, "6pm",
  21*60, "9pm"
)

plot_dat <- dat %>% 
  mutate(time = hms(dataset_time) %>% as.numeric()/60)

# requires pak::pkg_install("dill/emoGG")
plot <- ggplot(
  plot_dat %>% filter(dateTime != "2021-04-04")
) + 
  aes(x = time, y = dataset_value, group = dateTime) + 
  geom_line(color = "#a9a9a9") +
  geom_line(
    color = "#b573dc",
    size = 1.5,
    data = plot_dat %>% filter(dateTime == "2021-04-04")) +
  emoGG::geom_emoji(
    aes(x = 5.75*60, y = 750, group = NULL),
    data = NULL,
    emoji = "1f430"
  ) +
  scale_x_continuous(
    breaks = time_labels$minute,
    labels = time_labels$label
  ) + 
  labs(
    title = glue::glue(
      "Steps recorded in 2021: ",
      "<strong style = 'color: #a9a9a9'>All days</strong>",
      " versus ",
      "<strong style = 'color: #b573dc'>Easter day</strong>"
    ),
    subtitle = "Aggregated from FitBit in 15-minute intervals",
    caption = glue::glue(
      "Code: github.com/tgerke/fitbit-easter<br>",
      "Twitter: @travisgerke"
    )
  ) + 
  xlab(element_blank()) + 
  ylab(element_blank()) 

ggsave(
  here::here("figures", glue::glue("easter-steps.png")),
  plot,
  device = "png",
  width = 8,
  height = 9,
  units = "in",
  bg = "white"
)
