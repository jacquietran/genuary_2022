# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(showtext)
library(ggplot2)
# library(ggforce)

# Create data ------------------------------------------------------------------

ground_floor <- tibble(
  xmin = 0, xmax = 23, ymin = 0, ymax = 7)

first_floor <- tibble(
  xmin = 6, xmax = 29, ymin = 7, ymax = 14)

# Set custom colours
archway_colour <- "#411D01"
trim_colour <- "#833A02"

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img/20220104_camcorder/"),
    "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 16, # width of saved image
  height = 9, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("Homemade Apple", "homemade")
showtext_auto()

p <- ggplot()
# Ground floor
p <- p + ggpattern::geom_rect_pattern(
  data = ground_floor,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern_angle = 90),
  pattern = "weave", pattern_type = "satin", colour = "#B48841",
  pattern_colour = "#B48841", pattern_density = 0.98, pattern_fill = "#e1aa52")
# First floor
p <- p + ggpattern::geom_rect_pattern(
  data = first_floor,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern_angle = 90),
  pattern = "weave", pattern_type = "satin", colour = "#AF4E03",
  pattern_colour = "#AF4E03", pattern_density = 0.98, pattern_fill = "#db6204")
# Arch G.1 - 1st from left
p <- p + geom_segment( # Left straight line
  aes(x = 2, xend = 2, y = 0, yend = 3), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 2, xend = 3.5, y = 2.8, yend = 5.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 5, xend = 5, y = 0, yend = 3), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 5, xend = 3.5, y = 2.8, yend = 5.5),
  curvature = 0.2, colour = archway_colour)
# Arch G.2 - 2nd from left
p <- p + geom_segment( # Left straight line
  aes(x = 6, xend = 6, y = 0, yend = 3), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 6, xend = 7.5, y = 2.8, yend = 5.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 9, xend = 9, y = 0, yend = 3), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 9, xend = 7.5, y = 2.8, yend = 5.5),
  curvature = 0.2, colour = archway_colour)
# Arch G.3 - 3rd from left
p <- p + geom_segment( # Left straight line
  aes(x = 10, xend = 10, y = 0, yend = 3), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 10, xend = 11.5, y = 2.8, yend = 5.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 13, xend = 13, y = 0, yend = 3), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 13, xend = 11.5, y = 2.8, yend = 5.5),
  curvature = 0.2, colour = archway_colour)
# Arch G.4 - 4th from left
p <- p + geom_segment( # Left straight line
  aes(x = 14, xend = 14, y = 0, yend = 3), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 14, xend = 15.5, y = 2.8, yend = 5.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 17, xend = 17, y = 0, yend = 3), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 17, xend = 15.5, y = 2.8, yend = 5.5),
  curvature = 0.2, colour = archway_colour)
# Arch G.5 - 5th from left
p <- p + geom_segment( # Left straight line
  aes(x = 18, xend = 18, y = 0, yend = 3), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 18, xend = 19.5, y = 2.8, yend = 5.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 21, xend = 21, y = 0, yend = 3), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 21, xend = 19.5, y = 2.8, yend = 5.5),
  curvature = 0.2, colour = archway_colour)
# Arch 1.1 - 1st from left
p <- p + geom_segment( # Left straight line
  aes(x = 8, xend = 8, y = 7, yend = 10), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 8, xend = 9.5, y = 9.8, yend = 12.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 11, xend = 11, y = 7, yend = 10), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 11, xend = 9.5, y = 9.8, yend = 12.5),
  curvature = 0.2, colour = archway_colour)
# Arch 1.2 - 2nd from left
p <- p + geom_segment( # Left straight line
  aes(x = 12, xend = 12, y = 7, yend = 10), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 12, xend = 13.5, y = 9.8, yend = 12.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 15, xend = 15, y = 7, yend = 10), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 15, xend = 13.5, y = 9.8, yend = 12.5),
  curvature = 0.2, colour = archway_colour)
# Arch 1.3 - 3rd from left
p <- p + geom_segment( # Left straight line
  aes(x = 16, xend = 16, y = 7, yend = 10), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 16, xend = 17.5, y = 9.8, yend = 12.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 19, xend = 19, y = 7, yend = 10), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 19, xend = 17.5, y = 9.8, yend = 12.5),
  curvature = 0.2, colour = archway_colour)
# Arch 1.4 - 4th from left
p <- p + geom_segment( # Left straight line
  aes(x = 20, xend = 20, y = 7, yend = 10), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 20, xend = 21.5, y = 9.8, yend = 12.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 23, xend = 23, y = 7, yend = 10), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 23, xend = 21.5, y = 9.8, yend = 12.5),
  curvature = 0.2, colour = archway_colour)
# Arch 1.5 - 5th from left
p <- p + geom_segment( # Left straight line
  aes(x = 24, xend = 24, y = 7, yend = 10), size = 2, colour = archway_colour)
p <- p + geom_curve( # Left curved line
  aes(x = 24, xend = 25.5, y = 9.8, yend = 12.5),
  curvature = -0.2, size = 2, colour = archway_colour)
p <- p + geom_segment( # Right straight line
  aes(x = 27, xend = 27, y = 7, yend = 10), colour = archway_colour)
p <- p + geom_curve( # Right curved line
  aes(x = 27, xend = 25.5, y = 9.8, yend = 12.5),
  curvature = 0.2, colour = archway_colour)
# Ground floor trim
p <- p + geom_curve(
  aes(x = 0, xend = 1, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 1, xend = 2, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 2, xend = 3, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 3, xend = 4, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 4, xend = 5, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 5, xend = 6, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 6, xend = 7, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 7, xend = 8, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 8, xend = 9, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 9, xend = 10, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 10, xend = 11, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 11, xend = 12, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 12, xend = 13, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 13, xend = 14, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 14, xend = 15, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 15, xend = 16, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 16, xend = 17, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 17, xend = 18, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 18, xend = 19, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 19, xend = 20, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 20, xend = 21, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 21, xend = 22, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 22, xend = 23, y = 6.25, yend = 6.25),
  curvature = -0.8, colour = trim_colour)
# First floor trim
p <- p + geom_curve(
  aes(x = 6, xend = 7, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 7, xend = 8, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 8, xend = 9, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 9, xend = 10, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 10, xend = 11, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 11, xend = 12, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 12, xend = 13, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 13, xend = 14, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 14, xend = 15, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 15, xend = 16, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 16, xend = 17, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 17, xend = 18, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 18, xend = 19, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 19, xend = 20, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 20, xend = 21, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 21, xend = 22, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 22, xend = 23, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 23, xend = 24, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 24, xend = 25, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 25, xend = 26, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 26, xend = 27, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 27, xend = 28, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
p <- p + geom_curve(
  aes(x = 28, xend = 29, y = 13.25, yend = 13.25),
  curvature = -0.8, colour = trim_colour)
# Plot titles and theming
p <- p + labs(
  title = "Fidenza Cantilevered",
  subtitle = "#genuary2022 - Day 4",
  caption = "@jacquietran")
p <- p + theme_void()
p <- p + theme(
  text = element_text(family = "homemade"),
  plot.title = element_text(
    size = rel(16), margin = margin(0,0,5,0, unit = "pt")),
  plot.subtitle = element_text(
    size = rel(9), margin = margin(0,0,10,0, unit = "pt")),
  plot.caption = element_text(
    size = rel(9), margin = margin(10,0,0,0, unit = "pt")),
  plot.background = ggfx::with_halftone_dither(
    element_rect(fill = "#f7d3b5", colour = "#f7d3b5"),
    map_size = 4),
  plot.margin = margin(10,10,10,10, unit = "pt"))

# Export to file ---------------------------------------------------------------

# Export plot to PNG
ggsave(
  here::here("img/20220104.png"),
  last_plot(), width = 18, height = 18, units = "cm", dpi = 600)

# Generate GIF of plot building process
gg_playback(
  name = file.path(
    here::here("img/20220104_camcorder/"), "recording", "day04_gif.gif"),
  first_image_duration = 0.15,
  last_image_duration = 15,
  frame_duration = 0.15)

showtext_auto(FALSE)