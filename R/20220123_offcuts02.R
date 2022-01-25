# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ambient)
library(ggfx)
library(showtext)
library(ggplot2)

# Create data ------------------------------------------------------------------

seed_num <- 1068

## Generate noisy background #####
set.seed(seed_num)
perlin_noise <- long_grid(
  x = seq(0, 10, length.out = 1000),
  y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    noise = fracture(
      gen_perlin, fbm, octaves = 8, frequency = 60, x = x, y = y))

## Generate leaves #####
leaf_colours <- c(
  "#77613a", "#3c352d", "#b0a442", "#e1c0a1", "#dd977d", "#afd371", "#94a770",
  "#d2bd6a", "#5d4430", "#9d8e63")

set.seed(seed_num)
leaves <- tibble(
  x = rep(seq(-3, 11, by = 0.1), each = 30),
  intercept = rep(seq(7, 21, by = 0.1), times = 30),
  y = (intercept + (-1.2*x))) %>%
  mutate(
    y_limit = sample(seq(2, 7, by = 0.2), n(), replace = TRUE),
    y_limit_check = case_when(
      y <= y_limit ~ "below",
      y > y_limit  ~ "above")) %>%
  filter(y_limit_check == "above") %>%
  mutate(
    point_size = sample(seq(0.5, 4, by = 0.5), n(), replace = TRUE),
    point_colour = sample(leaf_colours, n(), replace = TRUE),
    point_shape = sample(c(15, 17, 18), n(), replace = TRUE),
    layer_num = sample(seq(1, 4, by = 1), n(), replace = TRUE),
    point_alpha = case_when(
      layer_num == 1 ~ 0.3,
      layer_num == 2 ~ 0.4,
      layer_num == 3 ~ 0.5,
      layer_num == 4 ~ 0.6))

## Trunk and branches #####
trunk_main <- tibble(
  x = 3, y = -2,
  xend = 7, yend = 14)

set.seed(seed_num)
trunk_branch_left <- tibble(
  x = seq(trunk_main$x, trunk_main$xend, length.out = 100),
  y = seq(trunk_main$y, trunk_main$yend, length.out = 100)) %>%
  filter(y >= 6 & y <= 8) %>%
  mutate(
    selection_var = sample(1:n(), n(), replace = FALSE)) %>%
  filter(selection_var == 10) %>%
  mutate(
    xend = x - 2,
    yend = y + 5) %>%
  select(x, y, xend, yend)

set.seed(seed_num)
trunk_branch_right <- tibble(
  x = seq(trunk_main$x, trunk_main$xend, length.out = 100),
  y = seq(trunk_main$y, trunk_main$yend, length.out = 100)) %>%
  filter(y >= 3 & y <= 6) %>%
  mutate(
    selection_var = sample(1:n(), n(), replace = FALSE)) %>%
  filter(selection_var == 20) %>%
  mutate(
    xend = x + 8,
    yend = y + 7) %>%
  select(x, y, xend, yend)

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("", "")
showtext_auto()

# Set custom colour gradient for noisy background
selected_colours <- c("#a2a9af", "#99abc3", "#d6e4f7")
noise_gradient <- (grDevices::colorRampPalette(selected_colours))(50)

set.seed(seed_num)
p <- ggplot() +
  # Noisy background base
  geom_raster(data = perlin_noise, aes(x, y, fill = noise)) +
  scale_fill_gradientn(colours = noise_gradient) +
  # Leaves
  as_reference(
    geom_jitter(
      data = leaves,
      aes(x = x, y = y, size = point_size, colour = point_colour,
          alpha = point_alpha, shape = point_shape), width = 2, height = 2),
    id = "leaves") +
  # Noise overlay
  with_blend(
    geom_raster(data = perlin_noise, aes(x, y, fill = noise)),
    bg_layer = "leaves",
    blend_type = "pin_light") +
  # Tree
  geom_curve(
    data = trunk_main, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.08, size = 20, colour = "#1C1111") +
  geom_curve(
    data = trunk_branch_left, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.1, size = 5, colour = "#1C1111") +
  geom_curve(
    data = trunk_branch_right, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.3, size = 5, colour = "#1C1111") +
  # Leaves overlay
  geom_jitter(
    data = leaves,
    aes(x = x, y = y, size = point_size, colour = point_colour,
        alpha = point_alpha, shape = point_shape), width = 2, height = 2) +
  scale_colour_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_shape_identity() +
  theme_void() +
  coord_cartesian(xlim = c(0,10), ylim = c(0,10), expand = FALSE) +
  theme(
    legend.position = "none",
    plot.background = element_blank())


  # Plot titles and theming
  labs(
    title = "",
    subtitle = "#genuary2022 - Day 23",
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
  here::here("img/offcuts/20220123_02.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)

showtext_auto(FALSE)