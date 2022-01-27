# Load libraries ---------------------------------------------------------------

library(dplyr)
library(showtext)
library(ggplot2)
library(ggpattern)
library(magick)
library(grid)

# Define custom functions ------------------------------------------------------

###############
# Grow leaves #
###############
grow_leaves <- function(initial_seed, n_leaves){
  
  # Requires {dplyr} and {purrr}
  
  set.seed(initial_seed)
  seed_vec <- sample(1:10000, n_leaves, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    # Set colour vector
    colour_vec <- c(
      "#DBA515", "#C6C013", "#034732", "#EF8A17", "#02271B", "#000000")
    
    set.seed(i)
    leaves <- tibble(
      x = sample(seq(3, 9, by = 0.25), 1),
      y = sample(seq(3, 9, by = 0.26), 1),
      seed_num = i,
      curve_linetype = sample(c("solid", "dotted"), 1),
      curve_size = sample(seq(0.3, 2.3, by = 0.25), 1),
      curve_colour = sample(colour_vec, 1)) %>%
      mutate(
        xend = x - sample(seq(-4, 4, by = 0.25), 1),
        yend = x - sample(seq(-4, 4, by = 0.24), 1),)
    
  }) -> leaves
  
  return(leaves)
  
}

#################
# Hatched blobs #
#################
drop_blobs <- function(initial_seed, n_blobs){
  
  # Requires {dplyr} and {purrr}
  
  set.seed(initial_seed)
  seed_vec <- sample(1:10000, n_blobs, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    operator_vec <- c("add", "sub", rep("neu", 8))
    
    set.seed(i)
    circle <- tibble(
      seed_num = i,
      degrees = seq(0, 355, by = 5),
      radians = degrees * pi / 180,
      radius = sample(seq(0.5, 2, by = 0.2), 1),
      x = (radius + sample(seq(3, 8, by = 0.5), 1)) + (radius * cos(radians)),
      y = (radius + sample(seq(3, 8, by = 0.5), 1)) + (radius * sin(radians))) %>%
    mutate(
      operator = sample(operator_vec, n(), replace = TRUE),
      x_nudge_amount = sample(seq(0.04, 0.08, by = 0.02), n(), replace = TRUE),
      y_nudge_amount = sample(seq(0.045, 0.08, by = 0.015), n(), replace = TRUE),
      x_nudged = case_when(
        operator == "add" ~ x + x_nudge_amount,
        operator == "sub" ~ x - x_nudge_amount,
        operator == "neu" ~ x),
      y_nudged = case_when(
        operator == "add" ~ y + y_nudge_amount,
        operator == "sub" ~ y - y_nudge_amount,
        operator == "neu" ~ y))
    
  }) -> circle
  
  return(circle)

}

# Pattern step 1 ---------------------------------------------------------------

first_seed <- 842

# Grow leaves
leaf_data <- grow_leaves(initial_seed = first_seed, n_leaves = 25)

# Drop blobs
blob_data <- drop_blobs(initial_seed = first_seed, n_blobs = 12)

# Build plot
p <- ggplot() +
  geom_polygon_pattern(
    data = blob_data,
    aes(x_nudged, y_nudged, group = seed_num), 
    fill = "#32913B", pattern_density = 0.28, pattern_spacing = 0.01,
    pattern_fill = "#008148", pattern_colour  = "#008148") +
  geom_curve(
    data = leaf_data,
    aes(x = x, y = y, xend = xend, yend = yend, group = seed_num,
        size = curve_size, linetype = curve_linetype, colour = curve_colour),
    curvature = +0.4, lineend = "round") +
  geom_curve(
    data = leaf_data,
    aes(x = x, y = y, xend = xend, yend = yend, group = seed_num,
        size = curve_size, linetype = curve_linetype, colour = curve_colour),
    curvature = -0.4, lineend = "round") +
  scale_size_identity() +
  scale_linetype_identity() +
  scale_alpha_identity() +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0,12), ylim = c(0,12), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#32913B", colour = "#32913B"))

# Export plot to file
ggsave(
  here::here("img/ingredients/20220126_pattern_step1.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Pattern step 2 ---------------------------------------------------------------

pattern_step1 <- image_read(
  here::here("img/ingredients/20220126_pattern_step1.png"))

# Slice long ways
tile_right <- image_crop(pattern_step1, "1417x2834")
tile_left <- image_crop(pattern_step1, "1417x2834+1417")

# Flip and recombine tiles
recombined_tiles_step1 <- image_append(c(tile_left, tile_right))

# Slice horizontally
tile_top <- image_crop(recombined_tiles_step1, "2834x1417+0+1417")
tile_bottom <- image_crop(recombined_tiles_step1, "2834x1417")

# Flip and recombine tiles once more
recombined_tiles_step2 <- image_append(c(tile_top, tile_bottom), stack = TRUE)

# Export recombined tile to file
image_write(
  recombined_tiles_step2,
  path = here::here("img/ingredients/20220126_pattern_step2.png"),
  format = "png")

# Pattern step 3 ---------------------------------------------------------------

# Read in image of recombined tile
pattern_step2 <- image_read(
  here::here("img/ingredients/20220126_pattern_step2.png"))

# Convert to raster
pattern_raster <- rasterGrob(
  pattern_step2, width = unit(1,"npc"), height = unit(1,"npc"))

second_seed <- 106

# Grow leaves
leaf_data2 <- grow_leaves(initial_seed = second_seed, n_leaves = 20)

# Drop blobs
blob_data2 <- drop_blobs(initial_seed = second_seed, n_blobs = 16)

# Build plot
p <- ggplot() +
  # Recombined tile
  annotation_custom(pattern_raster, -Inf, Inf, -Inf, Inf) +
  # New pattern overlay
  geom_polygon_pattern(
    data = blob_data2,
    aes(x_nudged, y_nudged, group = seed_num), 
    fill = "#32913B", pattern_density = 0.28, pattern_spacing = 0.01,
    pattern_fill = "#008148", pattern_colour  = "#008148") +
  geom_curve(
    data = leaf_data2,
    aes(x = x, y = y, xend = xend, yend = yend, group = seed_num,
        size = curve_size, linetype = curve_linetype, colour = curve_colour),
    curvature = +0.4, lineend = "round") +
  geom_curve(
    data = leaf_data2,
    aes(x = x, y = y, xend = xend, yend = yend, group = seed_num,
        size = curve_size, linetype = curve_linetype, colour = curve_colour),
    curvature = -0.4, lineend = "round") +
  scale_size_identity() +
  scale_linetype_identity() +
  scale_alpha_identity() +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0,12), ylim = c(0,12), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#32913B", colour = "#32913B"))

# Export plot to file
ggsave(
  here::here("img/ingredients/20220126_pattern_step3.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Pattern step 4 ---------------------------------------------------------------

pattern_step3 <- image_read(
  here::here("img/ingredients/20220126_pattern_step3.png"))

# Make a row with 3 tiles
tiled_row <- image_append(c(pattern_step3, pattern_step3, pattern_step3))

# Create 2x3 tiling
tiled_2x3 <- image_append(c(tiled_row, tiled_row), stack = TRUE) %>%
  # Add noise
  image_noise(noisetype = "Poisson") %>%
  # Resize
  image_scale("5000")

# Export to file
image_write(
  tiled_2x3,
  path = here::here("img/ingredients/20220126_pattern_step4.png"),
  format = "png")

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Fugaz One", "fugaz")
font_add_google("Unica One", "unica")
showtext_auto()

# Read in image of 2x3 tiling
tiling <- image_read(
  here::here("img/ingredients/20220126_pattern_step4.png"))

# Convert to raster
tiling_raster <- rasterGrob(
  tiling, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot() +
  # 2x3 tiling
  annotation_custom(tiling_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "SMART ARRAY",
    subtitle = "#genuary2022 - Day 26",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "fugaz", colour = "#DBA515"),
    plot.title = element_text(size = rel(13.3)),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#02271C", colour = "#02271C"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220126.png"),
  last_plot(), width = 12, height = 10, units = "cm", dpi = 600)

showtext_auto(FALSE)