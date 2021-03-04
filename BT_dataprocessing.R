library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(pdftools)

source("bdc_rink_plot.R")

# data_ohl <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
# data_woly <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv")

# data2 <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv") %>% 
#   clean_names() %>% 
#   filter(event %in% c("Shot", "Goal"))

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
  relocate(shooter, .after = passer)# %>%


# ggplot(data = filter(histo_data, team == "Boston Pride"),
#        mapping = aes(x = shot_ongoal, fill = shot_ongoal)) +
#   geom_histogram(stat = "count")

  

# filter(
#   time_since_pass > 8,
#   pass_type == "Direct")

# filter(pass_type == "Direct") %>%
# filter(shot_outcome == "On Net") %>%
# filter(team == "Boston Pride") %>%
# filter(game_date == "2021-01-23")

filter(
  # passer == "Shiann Darkangelo",
  # shooter == "Jillian Dempsey",
  # shot_type == "Fan",
  # team == "Toronto Six",
  # game_state == "5v3"
  # time_since_pass <= 3,
  # shot_outcome %in% c("On Net", "Missed"),
  shot_onetimer == FALSE
)

# ggplot(data = data,
#        mapping = aes(x = pass_angle, y = shot_angle)) +
#   geom_point() +
#   coord_fixed()

## Old plot that has "shot vectors"
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

bdc_rink_plot() +
  # geom_segment(data = data,
  #              aes(x = pass_x, y = pass_y, xend = rec_x, yend = rec_y),
  #              arrow = arrow(length = unit(0.3, "cm"))
  # ) +
  # geom_segment(data = data,
  #              aes(x = rec_x, y = rec_y, xend = shot_x, yend = shot_y),
  #              # arrow = arrow(length = unit(0.3, "cm")),
  #              linetype = "dashed"
  # ) +
  geom_point(data = data,
             aes(x = shot_x,
                 y = shot_y,
                 colour = period,
                 shape = shot_type),
             size = 4) +
  # geom_density_2d() +
  labs(
    # title = paste0("Direct passes from ", data$passer),
    title = paste0("Shots by ", data$shooter, " (", data$team, ") originating from direct passes"),
    # title = paste0("Direct ", data$team, " passes leading to ", tolower(data$shot_type), "s, taken within 1 sec of receiving a pass"),
    # title = paste0("All shots taken at ", data$game_state),
    subtitle = paste0("Arrows show pass distance and direction,",
                      "\ndashed lines show displacement of skater before shot,",
                      "\nand coloured dots show shot location and outcome\n"),
    x = "",
    y = ""
  ) +
  lims(
    x = c(0, 200),
    y = c(0, 85)
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0, 5.5, 0, 16.5, "pt"),
    # text = element_text(family = "Rubik"),
    # plot.title = element_markdown(),
    # plot.subtitle = element_markdown(),
    legend.position = "top",
    legend.title = element_blank())

bdc_rink_plot() +
  geom_point(data = data,
             mapping = aes(x = shot_x,
                           y = shot_y,
                           colour = time_since_pass,
                           shape = shot_outcome),
             size = 4) +
  labs(
    title = "All NHWL shot attempts and the outcomes based on the time since receiving a direct pass",
    x = "",
    y = ""
  ) +
  lims(
    x = c(0, 200),
    y = c(0, 85)
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0, 5.5, 0, 16.5, "pt"),
    # text = element_text(family = "Rubik"),
    # plot.title = element_markdown(),
    # plot.subtitle = element_markdown(),
    legend.position = "top",
    legend.title = element_blank())


viz_data <- data %>% 
  select(-game_date) %>% 
  mutate(shot_ongoal = ifelse(shot_outcome %in% c("On Net", "Goal"), TRUE, FALSE))

ggplot(data = viz_data,
       mapping = aes(x = pass_angle, fill = shot_ongoal)) +
  geom_histogram(position = "identity", binwidth = 30, alpha = 0.5) +
  scale_fill_manual(values = c("#008080", "#ca562c"),
                    breaks = c("TRUE", "FALSE")) +
  theme_minimal() +
  theme(legend.position = "none")

filter(
  # passer == "Shiann Darkangelo",
  # shooter == "Jillian Dempsey",
  # shot_type == "Fan",
  # team == "Toronto Six",
  # game_state == "5v3"
  # time_since_pass <= 3,
  # shot_outcome %in% c("On Net", "Missed"),
  shot_onetimer == FALSE
)

# pass_origins
bdc_rink_plot() +
  geom_point(data = viz_data,
             aes(x = pass_x,
                 y = pass_y,
                 colour = shot_ongoal),
             alpha = 0.2,
             size = 4,
             shape = 16) +
  scale_colour_manual(values = c("#008080", "#ca562c"),
                      breaks = c("TRUE", "FALSE")) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  lims(
    x = c(0, 200),
    y = c(0, 85)
  ) +
  theme_void() +
  theme(
    # plot.margin = margin(0, 5.5, 0, 16.5, "pt"),
    # text = element_text(family = "Rubik"),
    # plot.title = element_markdown(),
    # plot.subtitle = element_markdown(),
    legend.position = "none",
    legend.title = element_blank()) +
  ggsave("pass_origins.pdf", width = 8, height = 4, units = "in", dpi = 320) 

pdf_convert(pdf = paste0(here(), "/pass_origins.pdf"),
            filenames = "pass_origins.png",
            format = "png", dpi = 320)

# Shot locations
bdc_rink_plot() +
  geom_point(data = viz_data,
             aes(x = shot_x,
                 y = shot_y,
                 colour = shot_ongoal),
             alpha = 0.2,
             size = 4,
             shape = 17) +
  scale_colour_manual(values = c("#008080", "#ca562c"),
                      breaks = c("TRUE", "FALSE")) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  lims(
    x = c(0, 200),
    y = c(0, 85)
  ) +
  theme_void() +
  theme(
    # plot.margin = margin(0, 5.5, 0, 16.5, "pt"),
    # text = element_text(family = "Rubik"),
    # plot.title = element_markdown(),
    # plot.subtitle = element_markdown(),
    legend.position = "none",
    legend.title = element_blank()) +
  ggsave("shot_origins.pdf", width = 8, height = 4, units = "in", dpi = 320) 

pdf_convert(pdf = paste0(here(), "/shot_origins.pdf"),
            filenames = "shot_origins.png",
            format = "png", dpi = 320)

# write_csv(data, "data_2020-02-24.csv")

# Next:
#   Start figuring out time filter threshold
#   Add rink