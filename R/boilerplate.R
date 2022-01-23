# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "202201XX_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Create data ------------------------------------------------------------------

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#", colour = "#"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "202201XX_camcorder", "dayXX_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("", "")
showtext_auto()

p <- ggplot() +
  # Plot titles and theming
  labs(
    title = "",
    subtitle = "#genuary2022 - Day XX",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "", colour = "#"),
    plot.title = element_text(
      size = rel(14), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#", colour = "#"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/202201XX.png"),
  last_plot(), width = 12, height = 8, units = "cm", dpi = 600)

showtext_auto(FALSE)