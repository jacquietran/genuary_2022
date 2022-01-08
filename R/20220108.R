# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(showtext)
library(ggplot2)
library(ggforce)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220108_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Create data ------------------------------------------------------------------

base <- tibble(
  x = c(
    # White curve on charcoal bg
    0, 4, 2, # x coords for left-most part of curve
    2, 0, 4, # x coords for right-most part of curve
    # Light gradient curve on charcoal bg
    0, 4, 2, # x coords for left-most part of curve
    2, 0, 4, # x coords for right-most part of curve
    # Colourful gradient curve on charcoal bg
    0, 4, 2, # x coords for left-most part of curve
    2, 0, 4  # x coords for right-most part of curve
    ),
  y = c(
    # White curve on charcoal bg
    -0.5, 2, 0, # y coords for left-most part of curve
    0, -2, 0.5, # y coords for right-most part of curve
    # Light gradient curve on charcoal bg
    -0.5, 2, 0, # y coords for left-most part of curve
    0, -2, 0.5, # y coords for right-most part of curve
    # Colourful gradient curve on charcoal bg
    -0.5, 2, 0, # y coords for left-most part of curve
    0, -2, 0.5  # y coords for right-most part of curve
    ),
  facet_id = c(
    rep(1, 6),
    rep(2, 6),
    rep(3, 6)),
  curve_id = c(
    rep(1, 3),
    rep(2, 3),
    rep(3, 3),
    rep(4, 3),
    rep(5, 3),
    rep(6, 3)),
  colour = c(
    # White curve on charcoal bg
    rep("#FFFFFF", 6),
    # Light gradient curve on charcoal bg
    "#dee2e6",
    "#ced4da",
    "#adb5bd", # same colours here for smooth transition
    "#adb5bd", # same colours here for smooth transition
    "#6c757d",
    "#495057",
    # Colourful gradient curve on charcoal bg
    "#f9c80e",
    "#f86624",
    "#ea3546", # same colours here for smooth transition
    "#ea3546", # same colours here for smooth transition
    "#662e9b",
    "#43bccd")
  )

# Build plot -------------------------------------------------------------------

p <- ggplot()
p <- p + geom_bezier2(
  data = base, aes(x = x, y = y, group = curve_id, colour = colour),
  size = 2)
p <- p + facet_wrap(~facet_id, nrow = 1)
p <- p + theme_void()
p <- p + scale_colour_identity()
p <- p + theme(
  legend.position = "none",
  strip.text = element_blank(),
  plot.background = element_rect(fill = "#2D2D2D", colour = "#2D2D2D"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220108_camcorder", "day08_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Wallpoet", "wallpoet")
showtext_auto()

# Build plot
p <- ggplot()
p <- p + geom_bezier2(
  data = base, aes(x = x, y = y, group = curve_id, colour = colour),
  size = 2)
p <- p + facet_wrap(~facet_id, nrow = 1)
p <- p + scale_colour_identity()
# Plot titles and theming
p <- p + labs(
  title = "A Singular Illusion",
  subtitle = "#genuary2022 - Day 8",
  caption = "@jacquietran")
p <- p + theme_void()
p <- p + theme(
  text = element_text(family = "wallpoet", colour = "#dee2e6"),
  plot.title = element_text(
    size = rel(8), margin = margin(0,0,5,0, unit = "pt")),
  plot.subtitle = element_text(
    size = rel(5), margin = margin(0,0,10,0, unit = "pt")),
  plot.caption = element_text(
    size = rel(5), margin = margin(10,0,0,0, unit = "pt")),
  strip.text = element_blank(),
  legend.position = "none",
  plot.background = element_rect(fill = "#2D2D2D", colour = "#2D2D2D"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220108.png"),
  last_plot(), width = 16, height = 9, units = "cm", dpi = 600)

showtext_auto(FALSE)