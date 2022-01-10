# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(camcorder)
library(ambient)
library(ggbump)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220110_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Create data ------------------------------------------------------------------

# Define seed
seed_num <- 4831

# Generate noise for plot background
set.seed(seed_num)
grid <- long_grid(
  x = seq(0, 12, length.out = 1000),
  y = seq(0, 8, length.out = 1000)) %>%
  mutate(
    noise = gen_value(x = x, y = y, frequency = 0.1, seed = seed_num))

# Generate data to be used in ggbump::geom_bump()
# Set parameters for generating bumpy data 
num_of_curves = 15
x_start = -1 # intentionally out of frame
range_y_start = seq(1.2, 6.8, by = 0.2)
range_x_nudge = seq(0.5, 5, by = 0.25)
range_y_nudge = seq(-2, 2, by = 0.5)
colour_vec = c("#168aad", "#34a0a4", "#52b69a")

set.seed(seed_num)
bumps_x <- tibble(
  curve_id = seq(1, num_of_curves, by = 1),
  coordinate = "x",
  point_01 = x_start) %>%
  mutate(
    nudge_01 = sample(range_x_nudge, n(), replace = TRUE),
    point_02 = point_01 + nudge_01,
    nudge_02 = sample(range_x_nudge, n(), replace = TRUE),
    point_03 = point_02 + nudge_02,
    nudge_03 = sample(range_x_nudge, n(), replace = TRUE),
    point_04 = point_03 + nudge_03) %>%
  select(-contains("nudge"))

set.seed(seed_num)
bumps_y <- tibble(
  curve_id = seq(1, num_of_curves, by = 1),
  coordinate = "y",
  point_01 = sample(range_y_start, num_of_curves, replace = TRUE)) %>%
  mutate(
    nudge_01 = sample(range_y_nudge, n(), replace = TRUE),
    point_02 = point_01 + nudge_01,
    nudge_02 = sample(range_y_nudge, n(), replace = TRUE),
    point_03 = point_02 + nudge_02,
    nudge_03 = sample(range_y_nudge, n(), replace = TRUE),
    point_04 = point_03 + nudge_03) %>%
  select(-contains("nudge"))

set.seed(seed_num)
bumps <- bind_rows(bumps_x, bumps_y) %>%
  pivot_longer(
    cols = contains("point"),
    names_to = "point_num",
    values_to = "value") %>%
  pivot_wider(
    id_cols = c("curve_id", "point_num"),
    names_from = "coordinate",
    values_from = "value") %>%
  # Identify any curves with any xy vals beyond the desired plot area
  # (x = 0:12, y = 0:8)
  group_by(curve_id) %>%
  mutate(
    exceeds_plot_area = case_when(
      max(y) >= 7.8   ~ "yes",
      min(y) <= 0.2   ~ "yes",
      max(x) >= 11.6  ~ "yes",
      TRUE            ~ "no")) %>%
  filter(exceeds_plot_area == "no") %>%
  ungroup() %>%
  select(-exceeds_plot_area) %>%
  mutate(
    # Add variable for assigning colour to each curve
    colour = case_when(
      point_num == "point_01" ~ 
        sample(colour_vec, n(), replace = TRUE)),
    # Add variable to specify line size
    size = case_when(
      point_num == "point_01" ~ 
        sample(c(1.5, 1.5, 2, 2, 3.5), n(), replace = TRUE))) %>%
  fill(colour) %>%
  fill(size)

# Use bumps data to specify curve end points
end_dots <- bumps %>%
  filter(point_num == "point_04") %>%
  mutate(
    # Boost point size
    point_size = size + 2.5,
    blur_size = point_size + 2)

# Build plot -------------------------------------------------------------------

# Set custom colours for noisy background
low_colour = "#001a2c"
high_colour = "#00406c"

p <- ggplot() +
  # Noisy background
  geom_raster(data = grid, aes(x, y, fill = noise)) +
  scale_fill_gradient(low = low_colour, high = high_colour) +
  # Glowy bloops
  ggfx::with_blur(
    geom_point(
      data = end_dots,
      aes(x, y, group = curve_id, size = blur_size),
      colour = "#ecf6c8", alpha = 0.5),
    sigma = 40) +
  # Bump charts
  geom_bump(
    data = bumps,
    aes(x, y, group = curve_id, colour = colour, size = size), #smooth = 10,
    lineend = "round") +
  # Curve end points
  geom_point(
    data = end_dots,
    aes(x, y, group = curve_id, colour = colour, size = point_size),
    shape = 16, stroke = 0) +
  scale_colour_identity() +
  scale_size_identity() +
  theme_void() +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 8), expand = FALSE) +
  theme(
    # plot.background = element_rect(fill = "#", colour = "#"),
    legend.position = "none")

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220110_camcorder", "day10_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Unica One", "unica")
showtext_auto()

p <- ggplot() +
  # Noisy background
  geom_raster(data = grid, aes(x, y, fill = noise)) +
  scale_fill_gradient(low = low_colour, high = high_colour) +
  # Glowy bloops
  ggfx::with_blur(
    geom_point(
      data = end_dots,
      aes(x, y, group = curve_id, size = blur_size),
      colour = "#ecf6c8", alpha = 0.5),
    sigma = 40) +
  # Bump charts
  geom_bump(
    data = bumps,
    aes(x, y, group = curve_id, colour = colour, size = size), #smooth = 10,
    lineend = "round") +
  # Curve end points
  geom_point(
    data = end_dots,
    aes(x, y, group = curve_id, colour = colour, size = point_size),
    shape = 16, stroke = 0) +
  scale_colour_identity() +
  scale_size_identity() +
  # Plot titles and theming
  labs(
    title = "Wrong answers learning machine only",
    subtitle = "#genuary2022 - Day 10",
    caption = "@jacquietran") +
  theme_void() +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 8), expand = FALSE) +
  theme(
    legend.position = "none",
    text = element_text(family = "unica", colour = "#ade8f4"),
    plot.title = element_text(
      size = rel(8), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(5.5), margin = margin(0,0,5,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(5.5), margin = margin(5,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#001523", colour = "#001523"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220110.png"),
  last_plot(), width = 12, height = 8, units = "cm", dpi = 600)

showtext_auto(FALSE)