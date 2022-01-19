# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(ambient)
library(showtext)
library(ggplot2)
library(geomtextpath)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220119_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 12, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Create data ------------------------------------------------------------------

# Background noise
set.seed(123456)
noisy_bg <- long_grid(
  x = seq(-2.5, 5.5, length.out = 2000),
  y = seq(0, 6,  length.out = 2000)) %>%
  mutate(
    noise = gen_value(x = x, y = y, frequency = 0.1, seed = seed_num))

# Lyrics
df1 <- tibble(
  x = seq(-2, 5, length.out = 200),
  y = (4 + (1.2*x) + (-0.25*(x^2))),
  z = "BUT YOU KNOW IT GETS HARD SOMETIMES")
df2 <- tibble(
  x = seq(-2, 5, length.out = 200),
  y = (3 + (1.1*x) + (-0.15*(x^2))),
  z = "TO LEAVE IT ALL BEHIND")

# Band name
band_name_sask <- tibble(x = 3.75, y = 0.465, z = "SASK")
band_name_watch <- tibble(x = 4.5, y = 0.2, z = "WATCH")

# Noise overlay
num_of_lines <- 400
dist_between_lines <- 6 / num_of_lines
num_of_points <- 500
noisy_lines <- tibble(
  x = rep(
    seq(-2.5, 5.5, length.out = num_of_points), times = num_of_lines),
  y = rep(
    seq(0 + dist_between_lines, 6, by = dist_between_lines),
    each = num_of_points),
  alpha_value = sample(
    c(0.01, 0.02, 0.04), (num_of_points * num_of_lines), replace = TRUE))

# Build plot -------------------------------------------------------------------

p <- ggplot()
# Background noise
p <- p + geom_raster(data = noisy_bg, aes(x, y, fill = noise))
p <- p + scale_fill_gradient(low = "#131313", high = "#3F3F3F")
# Lyrics line 1
p <- p + geom_textpath(
  data = df1, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.4,
  colour = "#fc608f")
# Lyrics line 2
p <- p + geom_textpath(
  data = df2, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.75,
  colour = "#fdd827")
# Band name SASK-
p <- p + geom_text(
  data = band_name_sask, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 1, lineheight = 0.8, colour = "#F9F9F9")
# Band name -WATCH
p <- p + geom_text(
  data = band_name_watch, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 0, lineheight = 0.8, colour = "#F9F9F9", angle = 90)
# Noisy lines overlay
p <- p + geom_point(
  data = noisy_lines, aes(x = x, y = y, alpha = alpha_value),
  colour = "#2A2A2A", shape = 16, stroke = 0, size = 0.2)
# Lyrics line 1 - slightly transparent overlay
p <- p + geom_textpath(
  data = df1, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.4,
  colour = "#fc608f", alpha = 0.6)
# Lyrics line 2 - slightly transparent overlay
p <- p + geom_textpath(
  data = df2, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.75,
  colour = "#fdd827", alpha = 0.6)
# Band name SASK- - slightly transparent overlay
p <- p + geom_text(
  data = band_name_sask, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 1, lineheight = 0.8, colour = "#F9F9F9", alpha = 0.6)
# Band name -WATCH - slightly transparent overlay
p <- p + geom_text(
  data = band_name_watch, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 0, lineheight = 0.8, colour = "#F9F9F9", angle = 90, alpha = 0.6)
p <- p + theme_void()
p <- p + theme(
  legend.position = "none",
  plot.background = element_blank())

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220119_camcorder", "day19_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220119.png"),
  last_plot(), width = 8, height = 12, units = "cm", dpi = 600)
