# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ambient)
library(showtext)
library(ggplot2)
library(ggfx)

# Define custom function -------------------------------------------------------

generate_circle_coords <- function(radius, n_points){
  
  # Requires {dplyr}
  
  data <- tibble(
    degrees = seq(0, 360, length.out = n_points),
    radians = degrees * pi / 180,
    radius = radius,
    x = radius * cos(radians),
    y = radius * sin(radians))
  
  return(data)
  
}

# Create circle data -----------------------------------------------------------

circle <- generate_circle_coords(radius = 5, n_points = 17)

# Tidy up a little
circle_wrangled <- circle %>%
  mutate(
    counting_var = 1,
    point_num = cumsum(counting_var)) %>%
  filter(point_num != 17) %>%
  mutate(
    pair_id = case_when(
      point_num %in% c(5,6)   ~ 1,
      point_num %in% c(4,7)   ~ 2,
      point_num %in% c(3,8)   ~ 3,
      point_num %in% c(2,9)   ~ 4,
      point_num %in% c(1,10)  ~ 5,
      point_num %in% c(16,11) ~ 6,
      point_num %in% c(15,12) ~ 7,
      point_num %in% c(14,13) ~ 8)) %>%
  arrange(pair_id) %>%
  mutate(point_label = rep(c("start", "end"), 8))

circle_pivotx <- circle_wrangled %>%
  pivot_wider(
    id_cols = pair_id,
    names_from = point_label,
    values_from = x,
    names_glue = "x_{point_label}")

circle_pivoty <- circle_wrangled %>%
  pivot_wider(
    id_cols = pair_id,
    names_from = point_label,
    values_from = y,
    names_glue = "y_{point_label}")

circle_merged <- left_join(
  circle_pivotx, circle_pivoty, by = "pair_id") %>%
  mutate(
    curve_colour = case_when(
      pair_id %in% c(1,8) ~ "#723C70",
      pair_id %in% c(2,7) ~ "#892B64",
      pair_id %in% c(3,6) ~ "#A01A58",
      pair_id %in% c(4,5) ~ "#B7094C"))

circle_top <- circle_merged %>%
  filter(pair_id <= 5)

circle_bot <- circle_merged %>%
  filter(pair_id > 5)

# Create noise -----------------------------------------------------------------

set.seed(754)
noise_bg <- long_grid(
  x = seq(-10, 10, length.out = 1000),
  y = seq(-10, 10, length.out = 1000)) %>%
  mutate(
    noise = fracture(
      gen_value, fbm, octaves = 8, frequency = 100, x = x, y = y))

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("Work Sans", "work")
showtext_auto()

# Set absolute curvature value
curvature_value <- 0.12

# Create a colour gradient
colour_vec <- c(
  "#455E89", "#2E6F95", "#1780A1", "#0091AD", "#179BB4", "#38C8E5")

custom_gradient <- (grDevices::colorRampPalette(colour_vec))(256)

p <- ggplot() +
  # Noisy background
  geom_raster(data = noise_bg, aes(y, x, fill = noise)) +
  scale_fill_gradientn(colours = custom_gradient) +
  # Top circle part - shadow
  geom_curve(
    data = circle_top,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = pair_id),
    curvature = -curvature_value, size = 3, lineend = "round",
    colour = "#374A6D") +
  # Top circle part - contour lines
  geom_curve(
    data = circle_top,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = pair_id,
        colour = curve_colour),
    curvature = -curvature_value, size = 2.5, lineend = "round") +
  # Bottom circle part - shadow
  geom_curve(
    data = circle_bot,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = pair_id),
    curvature = curvature_value, size = 3, lineend = "round",
    colour = "#374A6D") +
  # Bottom circle part - contour lines
  geom_curve(
    data = circle_bot,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = pair_id,
        colour = curve_colour),
    curvature = curvature_value, size = 2.5, lineend = "round") +
  scale_colour_identity() +
  coord_equal(xlim = c(-10,10), ylim = c(-10,10), expand = FALSE) +
  # Plot titles and theming
  labs(
    title = "Suggestion",
    subtitle = "#genuary2022 - Day 31",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "work", colour = "#2E6F95"),
    plot.title = element_text(
      size = rel(14), face = "bold", margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(6.75), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(6.75), margin = margin(10,0,0,0, unit = "pt")),
    legend.position = "none",
    plot.background = element_rect(fill = "#C1DCEC", colour = "#C1DCEC"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220131.png"),
  last_plot(), width = 12, height = 14, units = "cm", dpi = 600)

showtext_auto(FALSE)