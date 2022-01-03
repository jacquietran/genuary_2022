# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggfx)
library(showtext)
library(ggplot2)

# Create data ------------------------------------------------------------------

# Set colours
# Palette from here: https://colorpalettes.net/color-palette-4224/
colour_vector <- c(
  "#09280a",
  "#446b04",
  "#e1c1b7",
  "#b05449",
  "#dbb238")

# Create the data
# Layer 1
set.seed(2005787)
layer_1 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1))

# Layer 2
set.seed(47698431)
layer_2 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1))

# Layer 3
set.seed(546861)
layer_3 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1))

# Layer 4
set.seed(20)
layer_4 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1))

# Layer 5
set.seed(84751)
layer_5 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1))

# Import fonts from Google
font_add_google("Righteous", "righteous")

showtext_auto()

# Build the plot ---------------------------------------------------------------

p <- ggplot(
  layer_1, aes(x = x, y = y, colour = colour))
# Layer 1
p <- p + with_halftone_dither(
  geom_point(size = 60, shape = 16, stroke = 0),
  map_size = 6)
# Layer 2
p <- p + with_halftone_dither(
  geom_point(data = layer_2, inherit.aes = TRUE,
             shape = 16, stroke = 0, size = 105),
  map_size = 4)
# Layer 3
p <- p + with_halftone_dither(
  geom_point(data = layer_3, inherit.aes = TRUE,
             shape = 16, stroke = 0, size = 45),
  map_size = 4)
# Layer 4
p <- p + with_halftone_dither(
  geom_point(data = layer_4, inherit.aes = TRUE,
             shape = 16, stroke = 0, size = 210),
  map_size = 6)
# Layer 5
p <- p + with_halftone_dither(
  geom_point(data = layer_5, inherit.aes = TRUE,
             shape = 16, stroke = 0, size = 144),
  map_size = 8)
p <- p + scale_colour_identity()
p <- p + labs(
  title = "The Halftones",
  subtitle = "#genuary2022 - Day 2",
  caption = "@jacquietran")
p <- p + theme_void()
p <- p + theme(
  text = element_text(family = "righteous"),
  plot.title = element_text(
    size = rel(16), margin = margin(0,0,5,0, unit = "pt")),
  plot.subtitle = element_text(
    size = rel(9), margin = margin(0,0,10,0, unit = "pt")),
  plot.caption = element_text(
    size = rel(7), margin = margin(10,0,0,0, unit = "pt")),
  plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
  panel.background = with_halftone_dither(
      element_rect(fill = "#F8DAA8", colour = "#F8DAA8"),
      map_size = 4),
  plot.margin = margin(10,20,10,20, unit = "pt"))

# Export to PNG ----------------------------------------------------------------

ggsave(
  here::here("img/20220102.png"),
  last_plot(), width = 7, height = 3.9375, units = "in", dpi = 600)

showtext_auto(FALSE)