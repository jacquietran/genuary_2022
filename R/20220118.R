# Load libraries ---------------------------------------------------------------

library(camcorder)
library(dplyr)
library(ambient)
library(showtext)
library(ggplot2)
library(ggfx)
library(magick)
library(grid)
library(jpeg)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220118_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Create data ------------------------------------------------------------------

# Create static base
# Inspo: https://en.wikipedia.org/wiki/File:SMPTE_Color_Bars.svg
static <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~fill_colour,
  
  # Row 1
  0, 1, 1.67, 4.67, "#c0c0c0",
  1, 2, 1.67, 4.67, "#c0c000",
  2, 3, 1.67, 4.67, "#00c0c0",
  3, 4, 1.67, 4.67, "#00c000",
  4, 5, 1.67, 4.67, "#c000c0",
  5, 6, 1.67, 4.67, "#c00000",
  6, 7, 1.67, 4.67, "#0000c0",
  # Row 2
  0, 1, 1.25, 1.67, "#0000c0",
  1, 2, 1.25, 1.67, "#131313",
  2, 3, 1.25, 1.67, "#c000c0",
  3, 4, 1.25, 1.67, "#131313",
  4, 5, 1.25, 1.67, "#00c0c0",
  5, 6, 1.25, 1.67, "#131313",
  6, 7, 1.25, 1.67, "#c0c0c0",
  # Row 3
  0, 1.25, 0, 1.25, "#00214c",
  1.25, 2.5, 0, 1.25, "#FFFFFF",
  2.5, 3.75, 0, 1.25, "#32006a",
  3.75, 5, 0, 1.25, "#131313",
  5, 5.33, 0, 1.25, "#090909",
  5.33, 5.66, 0, 1.25, "#131313",
  5.66, 6, 0, 1.25, "#1d1d1d",
  6, 7, 0, 1.25, "#131313")

# Generate noisy lines
#num_of_lines <- 100
#dist_between_lines <- 4.67 / num_of_lines
#num_of_points <- 500
#noisy_lines <- tibble(
#  x = rep(
#    seq(0, 7, length.out = num_of_points), times = num_of_lines),
#  y = rep(
#    seq(0 + dist_between_lines, 4.67, by = dist_between_lines),
#    each = num_of_points),
#  alpha_value = sample(
#    c(0.01, 0.02, 0.04), (num_of_points * num_of_lines), replace = TRUE))
# Commented out because the above chunk creates a neat effect when plotted
# with geom_point(), but I didn't end up using it in this piece

# Make noise
seed_num <- 1235710
set.seed(seed_num)
white_noize <- long_grid(
  x = seq(0, 7, length.out = 2000),
  y = seq(0, 4.67,  length.out = 2000)) %>%
  mutate(
    noise = gen_white(x = x, y = y, frequency = 1, seed = seed_num))

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  as_reference(
    geom_rect(
      data = static,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          fill = fill_colour, colour = fill_colour)),
    id = "base") +
  scale_fill_identity() +
  scale_colour_identity() +
  ggnewscale::new_scale_fill() +
  with_blend(
    geom_raster(
      data = white_noize, aes(x, y, fill = noise)),
    bg_layer = "base",
    blend_type = "soft_light") +
  scale_fill_gradient(low = "#A6A6A6", high = "#EEEEEE") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(-0.4, -0.8, -0.4, -0.8, unit = "cm"))

# Export static base image to file
ggsave(
  here::here("img/ingredients/20220118_static_base.png"),
  last_plot(), width = 12, height = 8, units = "cm", dpi = 600)

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220118_camcorder", "day18_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Use {magick} for more image degradation --------------------------------------

static_base <- image_read(
  here::here("img/ingredients/20220118_static_base.png"))

static_degraded <- static_base %>%
  image_noise() %>%
  image_oilpaint() %>%
  image_noise() %>%
  image_oilpaint() %>%
  image_noise() %>%
  image_oilpaint() %>%
  image_noise() %>%
  image_oilpaint() %>%
  image_noise() %>%
  image_oilpaint()

image_write(
  static_degraded,
  path = here::here("img/ingredients/20220118_static_degraded.jpg"),
  quality = 89, format = "jpg")

# Switch back to {ggplot2} workflow --------------------------------------------

# Import static_degraded image
static_degraded_import <- readJPEG(
  here::here("img/ingredients/20220118_static_degraded.jpg"))
static_degraded_raster <- rasterGrob(
  static_degraded_import, width = unit(1,"npc"), height = unit(1,"npc"))

# Import fonts from Google
font_add_google("Rammetto One", "rammetto")
showtext_auto()

# Build final plot
p <- ggplot() +
  annotation_custom(static_degraded_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "VIDEO EZY",
    subtitle = "#genuary2022 - Day 18",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "rammetto", colour = "#F1F1F1"),
    plot.title = element_text(
      size = rel(10.5), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(4.5), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(4.5), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#000000", colour = "#000000"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220118.png"),
  last_plot(), width = 12, height = 10, units = "cm", dpi = 400)

showtext_auto(FALSE)