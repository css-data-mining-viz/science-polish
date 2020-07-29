library(tidyverse)
library(lubridate)
library(ggridges)
library(gghalves)

theme_set(theme_minimal())

# import/clean data
weather_atl_raw <- read_csv("data/atl-weather-2019.csv")

weather_atl <- weather_atl_raw %>% 
  mutate(Month = month(time, label = TRUE, abbr = FALSE),
         Day = wday(time, label = TRUE, abbr = FALSE))
