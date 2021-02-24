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
  mutate(
    shot_from_pass = if_else(player == lag(player_2, n = 1L) & event %in% c("Shot", "Goal") & lag(event, n = 1L) == "Play", TRUE, FALSE),
    pass_to_shot = if_else(player_2 == lead(player, n = 1L) & event == "Play" & lead(event, n = 1L) %in% c("Shot", "Goal"), TRUE, FALSE)
  ) %>%
  filter(pass_to_shot | shot_from_pass == TRUE) %>%
  mutate(time_btw_events = ifelse(shot_from_pass == TRUE, game_seconds - lag(game_seconds, n = 1L), NA)) %>%
  relocate(time_btw_events, .after = game_seconds)

data <- data_nwhl %>%
  mutate(
    game_state = case_when(
      team == home_team ~ str_c(home_team_skaters, "v", away_team_skaters),
      team == away_team ~ str_c(away_team_skaters, "v", home_team_skaters)
    ),
    passer = if_else(lag(event, n = 1L) == "Play", lag(player, n = 1L), NA_character_),
    pass_type = if_else(lag(detail_1, n = 1L) %in% c("Direct", "Indirect"), lag(detail_1, n = 1L), NA_character_),
    pass_x = if_else(lag(event, n = 1L) == "Play", lag(x_coordinate, n = 1L), NA_real_),
    pass_y = if_else(lag(event, n = 1L) == "Play", lag(85 - y_coordinate, n = 1L), NA_real_),
    rec_x = if_else(lag(event, n = 1L) == "Play", lag(x_coordinate_2, n = 1L), NA_real_),
    rec_y = if_else(lag(event, n = 1L) == "Play", lag(85 - y_coordinate_2, n = 1L), NA_real_),
    shooter = if_else(event %in% c("Shot", "Goal"), player, NA_character_),
    shot_x = if_else(event %in% c("Shot", "Goal"), x_coordinate, NA_real_),
    shot_y = if_else(event %in% c("Shot", "Goal"), 85 - y_coordinate, NA_real_),
    shot_type = if_else(event %in% c("Shot", "Goal"), detail_1, NA_character_),
    shot_outcome = if_else(event %in% c("Shot", "Goal"), detail_2, NA_character_),
    shot_outcome = if_else(event == "Goal", "Goal", shot_outcome),
    shot_traffic = if_else(event %in% c("Shot", "Goal"), detail_3, NA),
    shot_onetimer = if_else(event %in% c("Shot", "Goal"), detail_4, NA),
    pass_dist = sqrt((rec_x - pass_x)^2 + (rec_y - pass_y)^2),
    pass_angle = atan2(43 - pass_y, 189 - pass_x) * 180 / pi,
    shooter_dist = sqrt((shot_x - rec_x)^2 + (shot_y - rec_y)^2),
    shot_dist = sqrt((189 - shot_x)^2 + (43 - shot_y)^2),
    shot_angle = atan2(43 - shot_y, 189 - shot_x) * 180 / pi
  ) %>%
  filter(shot_from_pass == TRUE,
         pass_type == "Direct") %>%
  select(-c(
    clock, home_team_skaters, away_team_skaters, event, player,
    pass_type, detail_1, detail_2, detail_3, detail_4, player_2,
    x_coordinate, y_coordinate, x_coordinate_2, y_coordinate_2,
    shot_from_pass, pass_to_shot
  )) %>%
  rename(time_since_pass = time_btw_events) %>% 
  relocate(shooter, .after = passer) %>%

# filter(
#   time_since_pass > 8,
#   pass_type == "Direct")

# filter(pass_type == "Direct") %>%
# filter(shot_outcome == "On Net") %>%
# filter(team == "Boston Pride") %>%
# filter(game_date == "2021-01-23")

filter(
  # passer == "Shiann Darkangelo",
  # shooter == "Taylor Woods",
  shot_type == "Slapshot",
  # game_state == "5v3"
  time_since_pass <= 1,
  # shot_outcome %in% c("On Net", "Missed"),
)

# ggplot(data = filter(data_nwhl, !is.na(time_btw_events)),
#        mapping = aes(x = time_btw_events, fill = event)) +
#   geom_histogram(binwidth = 1) +
#   scale_x_continuous(breaks = seq(0,25,1)) +
#   theme(panel.grid.minor = element_blank())

# ggplot(data = data) +
#   geom_segment(aes(x = pass_x, y = pass_y, xend = rec_x, yend = rec_y),
#     arrow = arrow(length = unit(0.3, "cm"))
#   ) +
#   geom_segment(aes(x = rec_x, y = rec_y, xend = shot_x, yend = shot_y),
#     # arrow = arrow(length = unit(0.3, "cm")),
#     linetype = "dashed"
#   ) +
#   geom_segment(aes(x = shot_x, y = shot_y, xend = 189, yend = 43, colour = shot_outcome),
#     # arrow = arrow(length = unit(0.3, "cm"))
#   ) +
#   labs(
#     # title = paste0("Direct passes from ", data$passer),
#     # title = paste0("Shots by ", data$shooter, " originating from direct passes"),
#     title = paste0("All shots classified as a ", tolower(data$shot_type)),
#     # title = paste0("All shots taken at ", data$game_state),
#     subtitle = paste0("Arrows show pass distance and direction,",
#                       "\ndashed lines show displacement of skater before shot,",
#                       "\nand coloured lines show shot outcome"),
#     x = "",
#     y = ""
#   ) +
#   lims(
#     x = c(0, 200),
#     y = c(0, 85)
#   ) +
#   coord_fixed() +
#   theme_classic()

ggplot(data = data,
       mapping = aes(x = shot_x, y = shot_y)) +
  geom_segment(aes(x = pass_x, y = pass_y, xend = rec_x, yend = rec_y),
               arrow = arrow(length = unit(0.3, "cm"))
  ) +
  geom_segment(aes(x = rec_x, y = rec_y, xend = shot_x, yend = shot_y),
               # arrow = arrow(length = unit(0.3, "cm")),
               linetype = "dashed"
  ) +
  geom_point(aes(colour = shot_outcome)) +
  # geom_density_2d() +
  labs(
    # title = paste0("Direct passes from ", data$passer),
    # title = paste0("Shots by ", data$shooter, " originating from direct passes"),
    title = paste0("All shots classified as a ", tolower(data$shot_type), ", taken within 1 sec of receiving a pass"),
    # title = paste0("All shots taken at ", data$game_state),
    subtitle = paste0("Arrows show pass distance and direction,",
                      "\ndashed lines show displacement of skater before shot,",
                      "\nand coloured lines show shot outcome"),
    x = "",
    y = ""
  ) +
  lims(
    x = c(0, 200),
    y = c(0, 85)
  ) +
  coord_fixed() +
  theme_classic() +
  facet_wrap(vars(shot_outcome))

# ggplot(data = data,
#        mapping = aes(x = pass_angle, y = shot_angle)) +
#   geom_point() +
#   coord_fixed()

write_csv(data, "data_2020-02-24.csv")

# Next:
#   Start figuring out time filter threshold
#   Add rink