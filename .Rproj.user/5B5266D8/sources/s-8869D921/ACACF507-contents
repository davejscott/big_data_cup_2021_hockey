library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(pdftools)

data <- read_csv("df_feature_importances.csv") %>% 
  arrange(desc(importance)) %>% 
  slice(18:43)

ggplot(data = data,
       mapping = aes(x = importance, y = reorder(feature, importance))
) +
  geom_bar(stat = "identity",
           fill = "#008080") +
  labs(title = "Top 25 shot success model features by feature importance",
       subtitle = "Overall feature groups and individual features values are highlighted",
       x = "",
       y = "") +
  # scale_colour_manual()
  theme_light() +
  ggsave("feature_importances_individual.pdf", width = 8, height = 5, units = "in", dpi = 320)
  

