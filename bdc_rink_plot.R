library(tidyverse)
library(ggforce)

# Setting up colour values
NHL_red <- "#FFCCD8" # Use #C8102E for original red in the rules, #FFE6EB for lighter hue
NHL_blue <- "#CCE1FF" # Use #0033A0 for original blue in the rules, #E6EFFF for lighter hue
NHL_light_blue <- "#CCF5FF" # Use #41B6E6 for original crease blue in the rules, #E6F9FF for lighter hue

bdc_rink_plot <- function () {
  
  # Plotting an NHL rink completely following the NHL rule book:
  # https://cms.nhl.bamgrid.com/images/assets/binary/308893668/binary-file/file.pdf
  # Line widths, lengths, colours, all followed as closely as possible
  
ggplot() +
  
  # Faceoff circles
  geom_circle(aes(x0 = 0 + 100, y0 = 0 + 42.5, r = 15), colour = NHL_blue, size = 2 / 12) + # Centre
  geom_circle(aes(x0 = 69 + 100, y0 = 22 + 42.5, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Right
  geom_circle(aes(x0 = 69 + 100, y0 = -22 + 42.5, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Right
  geom_circle(aes(x0 = -69 + 100, y0 = 22 + 42.5, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Left
  geom_circle(aes(x0 = -69 + 100, y0 = -22 + 42.5, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Left
  
  # Hash marks in T-R/B-R/T-L/B-R order, groups of four
  # geom_tile(aes(x = 66.125 + 100, y = 37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 66.125 + 100, y = 6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 71.875 + 100, y = 37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 71.875 + 100, y = 6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 66.125 + 100, y = -37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 66.125 + 100, y = -6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 71.875 + 100, y = -37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = 71.875 + 100, y = -6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -66.125 + 100, y = 37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -66.125 + 100, y = 6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -71.875 + 100, y = 37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -71.875 + 100, y = 6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -66.125 + 100, y = -37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -66.125 + 100, y = -6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -71.875 + 100, y = -37.77 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  # geom_tile(aes(x = -71.875 + 100, y = -6.23 + 42.5, width = 2 / 12, height = 2), fill = NHL_red) +
  
  # Centre line
  geom_tile(aes(x = 0 + 100, y = 0 + 42.5, width = 1, height = 85), fill = NHL_red) + # Centre line

  # Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
  geom_circle(aes(x0 = 0 + 100, y0 = 0 + 42.5, r = 6 / 12), colour = "#FF99B4", fill = "#FF99B4", size = 0) + # Centre dot with unique red
  geom_circle(aes(x0 = 69 + 100, y0 = 22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Right
  geom_circle(aes(x0 = 69 + 100, y0 = -22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Right
  geom_circle(aes(x0 = -69 + 100, y0 = 22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Left
  geom_circle(aes(x0 = -69 + 100, y0 = -22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Left
  
  geom_circle(aes(x0 = 20.5 + 100, y0 = 22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Right
  geom_circle(aes(x0 = 20.5 + 100, y0 = -22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Right
  geom_circle(aes(x0 = -20.5 + 100, y0 = 22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Left
  geom_circle(aes(x0 = -20.5 + 100, y0 = -22 + 42.5, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Left
  
  # Ells surrounding faceoff dots
  # geom_tile(aes(x = 65 + 100, y = 22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Right
  # geom_tile(aes(x = 73 + 100, y = 22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = 65 + 100, y = 21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = 73 + 100, y = 21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = 66.92 + 100, y = 24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = 71.08 + 100, y = 24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = 66.92 + 100, y = 19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = 71.08 + 100, y = 19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # 
  # geom_tile(aes(x = 65 + 100, y = -22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Right
  # geom_tile(aes(x = 73 + 100, y = -22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = 65 + 100, y = -21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = 73 + 100, y = -21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = 66.92 + 100, y = -24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = 71.08 + 100, y = -24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = 66.92 + 100, y = -19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = 71.08 + 100, y = -19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # 
  # geom_tile(aes(x = -65 + 100, y = 22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Left
  # geom_tile(aes(x = -73 + 100, y = 22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = -65 + 100, y = 21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = -73 + 100, y = 21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = -66.92 + 100, y = 24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = -71.08 + 100, y = 24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = -66.92 + 100, y = 19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = -71.08 + 100, y = 19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # 
  # geom_tile(aes(x = -65 + 100, y = -22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Left
  # geom_tile(aes(x = -73 + 100, y = -22.83 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = -65 + 100, y = -21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = -73 + 100, y = -21.17 + 42.5, width = 4, height = 2 / 12), fill = NHL_red) +
  # geom_tile(aes(x = -66.92 + 100, y = -24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = -71.08 + 100, y = -24.25 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = -66.92 + 100, y = -19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  # geom_tile(aes(x = -71.08 + 100, y = -19.75 + 42.5, width = 2 / 12, height = 3), fill = NHL_red) +
  
  # Referee crease
  # geom_arc(aes(x0 = 0 + 100, y0 = -42.5 + 42.5, start = -pi / 2, end = pi / 2, r = 10), colour = NHL_red) +
  
  # Left goalie crease
  geom_tile(aes(x = -86.75 + 100, y = 0 + 42.5, width = 4.5, height = 8), fill = NHL_light_blue) +
  geom_arc_bar(aes(x0 = -89 + 100, y0 = 0 + 42.5, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
  geom_tile(aes(x = -86.75 + 100, y = -4 + 42.5, width = 4.5, height = 2 / 12), fill = NHL_red) +
  geom_tile(aes(x = -86.75 + 100, y = 4 + 42.5, width = 4.5, height = 2 / 12), fill = NHL_red) +
  geom_arc(aes(x0 = -89 + 100, y0 = 0 + 42.5, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
  # geom_tile(aes(x = -85 + 100, y = 3.75 + 42.5, width = 2 / 12, height = 0.42), fill = NHL_red) +
  # geom_tile(aes(x = -85 + 100, y = -3.75 + 42.5, width = 2 / 12, height = 0.42), fill = NHL_red) +
  
  # Right goalie crease
  geom_tile(aes(x = 86.75 + 100, y = 0 + 42.5, width = 4.5, height = 8), fill = NHL_light_blue) +
  geom_arc_bar(aes(x0 = 89 + 100, y0 = 0 + 42.5, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
  geom_tile(aes(x = 86.75 + 100, y = -4 + 42.5, width = 4.5, height = 2 / 12), fill = NHL_red) +
  geom_tile(aes(x = 86.75 + 100, y = 4 + 42.5, width = 4.5, height = 2 / 12), fill = NHL_red) +
  geom_arc(aes(x0 = 89 + 100, y0 = 0 + 42.5, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
  # geom_tile(aes(x = 85 + 100, y = 3.75 + 42.5, width = 2 / 12, height = 0.42), fill = NHL_red) +
  # geom_tile(aes(x = 85 + 100, y = -3.75 + 42.5, width = 2 / 12, height = 0.42), fill = NHL_red) +
  
  # Goalie nets placed as rectangles
  geom_tile(aes(x = -90.67 + 100, y = 0 + 42.5, width = 3.33, height = 6), fill = "#E5E5E3") + # Left # with grey fills
  geom_tile(aes(x = 90.67 + 100, y = 0 + 42.5, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
  
  # Trapezoids
  # geom_polygon(aes(x = c(-100 + 100, -100 + 100, -89 + 100, -89 + 100), y = c(10.92 + 42.5, 11.08 + 42.5, 7.08 + 42.5, 6.92 + 42.5)), fill = NHL_red) + # Left
  # geom_polygon(aes(x = c(-100 + 100, -100 + 100, -89 + 100, -89 + 100), y = c(-10.92 + 42.5, -11.08 + 42.5, -7.08 + 42.5, -6.92 + 42.5)), fill = NHL_red) + # Left 
  # geom_polygon(aes(x = c(100 + 100, 100 + 100, 89 + 100, 89 + 100), y = c(10.92 + 42.5, 11.08 + 42.5, 7.08 + 42.5, 6.92 + 42.5)), fill = NHL_red) + # Right
  # geom_polygon(aes(x = c(100 + 100, 100 + 100, 89 + 100, 89 + 100), y = c(-10.92 + 42.5, -11.08 + 42.5, -7.08 + 42.5, -6.92 + 42.5)), fill = NHL_red) + # Right
  
  # Lines
  geom_tile(aes(x = -25.5 + 100, y = 0 + 42.5, width = 1, height = 85), fill = NHL_blue) + # Left Blue line
  geom_tile(aes(x = 25.5 + 100, y = 0 + 42.5, width = 1, height = 85),  fill = NHL_blue) + # Right Blue line
  geom_tile(aes(x = -89 + 100, y = 0 + 42.5, width = 2 / 12, height = 73.50), fill = NHL_red) + # Left goal line (73.5 value is rounded from finding intersect of goal line and board radius)
  geom_tile(aes(x = 89 + 100, y = 0 + 42.5, width = 2 / 12, height = 73.50), fill = NHL_red) + # Right goal line
  
  # Borders as line segments - plotted last to cover up line ends, etc.
  geom_line(aes(x = c(-72 + 100, 72 + 100), y = c(42.5 + 42.5, 42.5 + 42.5))) + # Top
  geom_line(aes(x = c(-72 + 100, 72 + 100), y = c(-42.5 + 42.5, -42.5 + 42.5))) + # Bottom
  geom_line(aes(x = c(-100 + 100, -100 + 100), y = c(-14.5 + 42.5, 14.5 + 42.5))) + # Left
  geom_line(aes(x = c(100 + 100, 100 + 100), y = c(-14.5 + 42.5, 14.5 + 42.5))) + # Right
  geom_arc(aes(x0 = 72 + 100, y0 = 14.5 + 42.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
  geom_arc(aes(x0 = 72 + 100, y0 = -14.5 + 42.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
  geom_arc(aes(x0 = -72 + 100, y0 = 14.5 + 42.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
  geom_arc(aes(x0 = -72 + 100, y0 = -14.5 + 42.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
  
  # Fixed scale for the coordinate system  
  coord_fixed()
}

# theme_void() # include this for a blank plot, but likely easier to include in ggplot2 function in main code to control layering