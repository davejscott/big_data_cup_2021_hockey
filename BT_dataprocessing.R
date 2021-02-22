library(here)
library(tidyverse)
library(lubridate)
library(janitor)

# data_ohl <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
# data_woly <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv")

data_nwhl <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv") %>% 
  clean_names() %>% 
  filter(event %in% c("Shot", "Play", "Goal")) %>% 
  mutate(game_seconds = case_when(
    period == 1 ~ 1200 - as.numeric(ms(str_sub(clock, 1, 5))),
    period == 2 ~ 2400 - as.numeric(ms(str_sub(clock, 1, 5))),
    period == 3 ~ 3600 - as.numeric(ms(str_sub(clock, 1, 5))),
    period == 4 ~ 3900 - as.numeric(ms(str_sub(clock, 1, 5)))
  )) %>%
  relocate(game_seconds, .after = clock) %>% 
  # mutate(time_btw_events = game_seconds - lag(game_seconds, n = 1L)) %>% 
  # relocate(time_btw_events, .after =  game_seconds) %>% 
  mutate(shot_from_pass = if_else(player == lag(player_2, n = 1L) & event %in% c("Shot", "Goal") & lag(event, n = 1L) == "Play", TRUE, FALSE)) %>% 
  mutate(pass_to_shot = if_else(player_2 == lead(player, n = 1L) & event == "Play" & lead(event, n = 1L) %in% c("Shot", "Goal"), TRUE, FALSE)) %>% 
  filter(pass_to_shot | shot_from_pass == TRUE) %>% 
  mutate(time_btw_events = ifelse(shot_from_pass == TRUE, game_seconds - lag(game_seconds, n = 1L), NA)) %>%
  relocate(time_btw_events, .after =  game_seconds) 

write_csv(data_nwhl, "data.csv")

# Next:
#   New data frame that combines passes and shots/goals to the same row
#   Viz for pass to shot
#   Start figuring out time filter threshold