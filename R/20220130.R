# Load libraries ---------------------------------------------------------------

library(dplyr)
library(showtext)
library(ambient)
library(ggplot2)
library(ggpattern)
library(magick)
library(grid)

# Define custom function -------------------------------------------------------

################################
# Paint a singular brushstroke #
################################
brushstroke <- function(
  seed_num, granularity, x_lower, x_upper, y_lower, y_upper){
  
  # Requires {dplyr} and {purrr}
  
  operator_vec <- c("add", "sub")
  
  set.seed(seed_num)
  data <- tibble(
    side = c(
      rep("top", times = granularity),
      rep("right", times = granularity),
      rep("bottom", times = granularity),
      rep("left", times = granularity)),
    x = c(
      seq(x_lower, x_upper, length.out = granularity),
      rep(x_upper, times = granularity),
      seq(x_upper, x_lower, length.out = granularity),
      rep(x_lower, times = granularity)),
    y = c(
      rep(y_upper, times = granularity),
      seq(y_upper, y_lower, length.out = granularity),
      rep(y_lower, times = granularity),
      seq(y_lower, y_upper, length.out = granularity))) %>%
    mutate(
      operator = case_when(
        side %in% c(
          "top", "bottom") ~ "neu",
        TRUE               ~ sample(operator_vec, n(), replace = TRUE)),
      x_nudge_amount1 = sample(seq(0.1, 0.5, by = 0.1), n(), replace = TRUE),
      x_nudge_amount2 = sample(seq(-0.25, 0.25, by = 0.05), n(), replace = TRUE),
      x_nudged = case_when(
        operator == "add" ~ x + x_nudge_amount1,
        operator == "sub" ~ x - x_nudge_amount1,
        operator == "neu" ~ x),
      x_nudged_again = x_nudged + x_nudge_amount2)
  
  return(data)
  
}

# Paint brushstroke: Top line --------------------------------------------------

# Key parameters
granularity_num <- 2000
x_low <- 8
x_up <- 18
y_low <- 18
y_up <- 24

# Main stroke
#brushed1 <- brushstroke(
#  seed_num = 1247, granularity = granularity_num,
#  x_lower = x_low, x_upper = x_up, y_lower = y_low, y_upper = y_up)

#top_stroke <- purrr::map_dfr(seq_len(5), ~brushed1, .id = "id")

# Understrokes
brushed1_nudged1 <- brushstroke(
  seed_num = 12471, granularity = granularity_num,
  x_lower = x_low - 0.2, x_upper = x_up + 0.2, y_lower = y_low, y_upper = y_up)

brushed1_nudged2 <- brushstroke(
  seed_num = 12472, granularity = granularity_num,
  x_lower = x_low - 0.4, x_upper = x_up + 0.4, y_lower = y_low, y_upper = y_up)

brushed1_nudged3 <- brushstroke(
  seed_num = 12473, granularity = granularity_num,
  x_lower = x_low - 0.6, x_upper = x_up + 0.6, y_lower = y_low, y_upper = y_up)

brushed1_nudged4 <- brushstroke(
  seed_num = 12474, granularity = granularity_num,
  x_lower = x_low - 0.8, x_upper = x_up + 0.8, y_lower = y_low, y_upper = y_up)

brushed1_nudged5 <- brushstroke(
  seed_num = 12475, granularity = granularity_num,
  x_lower = x_low - 1.0, x_upper = x_up + 1.0, y_lower = y_low, y_upper = y_up)

top_stroke_nudged <- bind_rows(
  brushed1_nudged1, brushed1_nudged2, brushed1_nudged3, brushed1_nudged4,
  brushed1_nudged5, .id = "id")

# Paint brushstroke: Middle line -----------------------------------------------

# Key parameters
granularity_num <- 2000
x_low <- 9.8
x_up <- 19.5
y_low <- 10.5
y_up <- 16.5

# Main stroke
#brushed2 <- brushstroke(
#  seed_num = 2471, granularity = granularity_num,
#  x_lower = x_low, x_upper = x_up, y_lower = y_low, y_upper = y_up)

#mid_stroke <- purrr::map_dfr(seq_len(5), ~brushed2, .id = "id")

# Understrokes
brushed2_nudged1 <- brushstroke(
  seed_num = 2471, granularity = granularity_num,
  x_lower = x_low - 0.2, x_upper = x_up + 0.2, y_lower = y_low, y_upper = y_up)

brushed2_nudged2 <- brushstroke(
  seed_num = 2472, granularity = granularity_num,
  x_lower = x_low - 0.4, x_upper = x_up + 0.4, y_lower = y_low, y_upper = y_up)

brushed2_nudged3 <- brushstroke(
  seed_num = 2473, granularity = granularity_num,
  x_lower = x_low - 0.6, x_upper = x_up + 0.6, y_lower = y_low, y_upper = y_up)

brushed2_nudged4 <- brushstroke(
  seed_num = 2474, granularity = granularity_num,
  x_lower = x_low - 0.8, x_upper = x_up + 0.8, y_lower = y_low, y_upper = y_up)

brushed2_nudged5 <- brushstroke(
  seed_num = 2475, granularity = granularity_num,
  x_lower = x_low - 1.0, x_upper = x_up + 1.0, y_lower = y_low, y_upper = y_up)

mid_stroke_nudged <- bind_rows(
  brushed2_nudged1, brushed2_nudged2, brushed2_nudged3, brushed2_nudged4,
  brushed2_nudged5, .id = "id")

# Paint brushstroke: Bottom line -----------------------------------------------

# Key parameters
granularity_num <- 2000
x_low <- 6.6
x_up <- 18.5
y_low <- 2
y_up <- 8

# Main stroke
#brushed3 <- brushstroke(
#  seed_num = 4712, granularity = granularity_num,
#  x_lower = x_low, x_upper = x_up, y_lower = y_low, y_upper = y_up)

# bot_stroke <- purrr::map_dfr(seq_len(5), ~brushed3, .id = "id")

# Understrokes
brushed3_nudged1 <- brushstroke(
  seed_num = 47121, granularity = granularity_num,
  x_lower = x_low - 0.2, x_upper = x_up + 0.2, y_lower = y_low, y_upper = y_up)

brushed3_nudged2 <- brushstroke(
  seed_num = 47122, granularity = granularity_num,
  x_lower = x_low - 0.4, x_upper = x_up + 0.4, y_lower = y_low, y_upper = y_up)

brushed3_nudged3 <- brushstroke(
  seed_num = 47123, granularity = granularity_num,
  x_lower = x_low - 0.6, x_upper = x_up + 0.6, y_lower = y_low, y_upper = y_up)

brushed3_nudged4 <- brushstroke(
  seed_num = 47124, granularity = granularity_num,
  x_lower = x_low - 0.8, x_upper = x_up + 0.8, y_lower = y_low, y_upper = y_up)

brushed3_nudged5 <- brushstroke(
  seed_num = 47125, granularity = granularity_num,
  x_lower = x_low - 1.0, x_upper = x_up + 1.0, y_lower = y_low, y_upper = y_up)

bot_stroke_nudged <- bind_rows(
  brushed3_nudged1, brushed3_nudged2, brushed3_nudged3, brushed3_nudged4,
  brushed3_nudged5, .id = "id")

# Create noise -----------------------------------------------------------------

set.seed(222)
white_noise_lines <- long_grid(
  x = seq(-6, 32, length.out = 1000),
  y = seq(-6, 32, length.out = 1000)) %>%
  mutate(
    noise = fracture(
      gen_white, fbm, octaves = 8, frequency = 1, x = x, y = y))

# Build plot -------------------------------------------------------------------

# Burger with the lot
p <- ggplot() +
  # Noisy background
  geom_raster(
    data = white_noise_lines, aes(y, x, fill = noise)) +
  scale_fill_gradient(low = "#e6e2cd", high = "#EAD0C3") +
  # Top line - understrokes
  geom_polygon_pattern(
    data = top_stroke_nudged, aes(x_nudged_again, y, group = id), 
    fill = "#167479", colour = "#167479", size = 0.005, alpha = 0.4,
    pattern_fill = "#187C82", pattern_colour = "#167479", pattern_alpha = 0.4,
    pattern_angle = 0, pattern_spacing = 0.004, pattern_density = 1) +
  # Middle line - understrokes
  geom_polygon_pattern(
    data = mid_stroke_nudged, aes(x_nudged_again, y, group = id), 
    fill = "#B1A095", colour = "#B1A095", size = 0.005, alpha = 0.4,
    pattern_fill = "#B6A69B", pattern_colour = "#B1A095", pattern_alpha = 0.4,
    pattern_angle = 0, pattern_spacing = 0.004, pattern_density = 1) +
  # Bottom line - understrokes
  geom_polygon_pattern(
    data = bot_stroke_nudged, aes(x_nudged_again, y, group = id), 
    fill = "#75896C", colour = "#75896C", size = 0.005, alpha = 0.4,
    pattern_fill = "#7A8E71", pattern_colour = "#75896C", pattern_alpha = 0.4,
    pattern_angle = 0, pattern_spacing = 0.004, pattern_density = 1) +
  coord_equal(xlim = c(-6,32), ylim = c(-6,32), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e6e2cd", colour = "#e6e2cd"))

ggsave(
  here::here("img/ingredients/20220130_01.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Bun only
p <- ggplot() +
  # Noisy background
  geom_raster(
    data = white_noise_lines, aes(y, x, fill = noise)) +
  scale_fill_gradient(low = "#e6e2cd", high = "#EAD0C3") +
  coord_equal(xlim = c(-6,32), ylim = c(-6,32), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e6e2cd", colour = "#e6e2cd"))

ggsave(
  here::here("img/ingredients/20220130_02.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

# Read in image
img <- image_read(here::here("img/ingredients/20220130_01.png"))
noise_bg <- image_read(here::here("img/ingredients/20220130_02.png"))

# Apply filering
img_filtered <- img %>%
  image_morphology(method = "Dilate", kernel = "Octagon", iter = 8) %>%
  image_noise(noisetype = "Laplacian") %>%
  image_noise(noisetype = "Laplacian") %>%
  image_noise(noisetype = "Laplacian")

img_blended <- image_flatten(
  c(img_filtered, img_filtered, noise_bg, noise_bg), "Darken")

# Export
image_write(
  img_blended,
  path = here::here("img/ingredients/20220130_blended_img.png"),
  format = "png")

# Shift to {ggplot2} workflow --------------------------------------------------

# Import fonts from Google
font_add_google("Signika", "signika")
showtext_auto()

# Read in blended image
tape <- image_read(here::here("img/ingredients/20220130_blended_img.png"))

# Convert to raster
tape_raster <- rasterGrob(tape, width = unit(1,"npc"), height = unit(1,"npc"))

# Build plot
p <- ggplot() +
  annotation_custom(tape_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "A Fix For Everything",
    subtitle = "#genuary2022 - Day 30",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "signika", colour = "#c3a698"),
    plot.title = element_text(
      size = rel(14), margin = margin(5,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#f9f3e1", colour = "#f9f3e1"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220130.png"),
  last_plot(), width = 12, height = 14, units = "cm", dpi = 600)

showtext_auto(FALSE)