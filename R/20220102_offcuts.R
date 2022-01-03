# Offcut 01 --------------------------------------------------------------------

# Load libraries
library(magick)

# Oddish
# Image sourced from here: https://pokemondb.net/pokedex/oddish

# Read image into R
oddish <- image_read(here::here("misc/oddish.jpg"))

oddish_quantized <- image_quantize(oddish, 6)

# Write dithered image out to file
image_write(
  oddish_quantized,
  path = here::here("img/offcuts/20220102_offcut01.jpg"),
  format = "jpg")

# Offcut 02 --------------------------------------------------------------------

# Load libraries
library(dplyr)
# library(ggfx)
library(ggplot2)

# Set colours
# Palette from here: https://colorpalettes.net/color-palette-4224/
colour_vector <- c(
  "#09280a",
  "#446b04",
  "#e1c1b7",
  "#b05449",
  "#dbb238",
  "#FFFFFF")

# Create the data
set.seed(2022)
data_points_layer1 <- tibble(
  x = runif(30, min=0, max=5),
  y = runif(30, min=0, max=5),
  colour = sample(colour_vector, 30, replace = TRUE),
  size = runif(30, min=80, max=190))

set.seed(2022)
data_points_layer2 <- tibble(
  x = runif(60, min=0, max=5),
  y = runif(60, min=0, max=5),
  colour = sample(colour_vector, 60, replace = TRUE),
  size = runif(60, min=30, max=200))

# Build the plot
p <- ggplot(
  data_points_layer2, aes(x = x, y = y, colour = colour))
# Layer 1
p <- p + geom_point(
  shape = 15, fill = data_points_layer2$colour,
  stroke = 0, size = data_points_layer2$size)
# Layer 2
p <- p + geom_point(
  data = data_points_layer1, inherit.aes = TRUE,
  shape = 16, stroke = 0, size = data_points_layer1$size, alpha = 0.2)
p <- p + scale_colour_identity()
p <- p + theme_void()

# Export to PNG
ggsave(
  here::here("img/offcuts/20220102_offcut02.png"),
  last_plot(), width = 9, height = 6, units = "in", dpi = 600)

# Offcut 02 --------------------------------------------------------------------

# Load libraries
# library(dplyr)
# library(ggfx)
# library(ggplot2)

# Set colours
# Palette from here: https://colorpalettes.net/color-palette-4224/
colour_vector <- c(
  "#09280a",
  "#446b04",
  "#e1c1b7",
  "#b05449",
  "#dbb238")

# Create the data
set.seed(202)
data_points <- tibble(
  x = runif(70, min=0, max=5),
  y = runif(70, min=0, max=5),
  colour = sample(colour_vector, 70, replace = TRUE),
  size = runif(70, min=30, max=125))

# Build the plot
p <- ggplot(
  data_points, aes(x = x, y = y, colour = colour))
p <- p + geom_point(
  shape = 15, fill = data_points$colour,
  stroke = 0, size = data_points$size)
p <- p + scale_colour_identity()
p <- p + theme_void()

# Export to PNG
ggsave(
  here::here("img/offcuts/20220102_offcut03.png"),
  last_plot(), width = 9, height = 6, units = "in", dpi = 600)