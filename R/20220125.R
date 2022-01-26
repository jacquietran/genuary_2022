# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ambient)
library(camcorder)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220125_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 16, # width of saved image
  height = 9, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Define custom function -------------------------------------------------------

mountain_ridge <- function(
  seed_num, intercept, n_points, steepness = c("shallow", "moderate", "steep")){
  
  # Requires {dplyr}
  
  steepness <- match.arg(steepness)
  n_points <- n_points
  
  set.seed(seed_num)
  data <- tibble(
    x = seq(0, 16, length.out = n_points),
    y = intercept,
    steepness = steepness,
    operation = sample(c("add", "sub", "neu"), n_points, replace = TRUE)) %>%
    mutate(
      nudge_amount = case_when(
        steepness == "shallow"  ~ sample(
          seq(-0.04, 0.04, by = 0.001), n(), replace = TRUE),
        steepness == "moderate" ~ sample(
          seq(-0.3, 0.3, by = 0.01), n(), replace = TRUE),
        steepness == "steep"    ~ sample(
          seq(-0.6, 0.6, by = 0.1), n(), replace = TRUE)),
      nudge_with_dir = case_when(
        operation == "add" ~ nudge_amount,
        operation == "sub" ~ -nudge_amount,
        operation == "neu" ~ 0),
      cumulative_nudge = cumsum(nudge_with_dir),
      y_nudged = y + cumulative_nudge)
  
  return(data)
  
}

# Create data ------------------------------------------------------------------

## Create noisy background #####
set.seed(222)
perlin_noise <- long_grid(
  x = seq(0, 9, length.out = 1000),
  y = seq(0, 16, length.out = 1000)) %>%
  mutate(
    noise = fracture(
      gen_perlin, fbm, octaves = 8, frequency = 80, x = x, y = y))

## Create mountain ridge data #####
ridge01 <- mountain_ridge(
  seed_num = 123, intercept = 2, n_points = 1000, steepness = "shallow")

ridge02 <- mountain_ridge(
  seed_num = 321, intercept = 3.3, n_points = 800, steepness = "shallow")
ridge02_depth <- mountain_ridge(
  seed_num = 321, intercept = 2.3, n_points = 800, steepness = "shallow")

ridge03 <- mountain_ridge(
  seed_num = 789, intercept = 5.7, n_points = 200, steepness = "moderate")
ridge03_depth <- mountain_ridge(
  seed_num = 789, intercept = 4.7, n_points = 200, steepness = "moderate")

ridge04 <- mountain_ridge(
  seed_num = 987, intercept = 9.5, n_points = 200, steepness = "moderate")
ridge04_depth <- mountain_ridge(
  seed_num = 987, intercept = 7, n_points = 200, steepness = "moderate")

ridge05 <- mountain_ridge(
  seed_num = 456, intercept = 11.5, n_points = 120, steepness = "steep")
ridge05_depth <- mountain_ridge(
  seed_num = 456, intercept = 10, n_points = 120, steepness = "steep")

# Build plot -------------------------------------------------------------------

# Set custom colour gradient for noisy background
selected_colours <- c(
  "#6C949D", "#9DB8BE", "#B7C4CD", "#DCE4E4", "#F3F6F6")
noise_gradient <- (grDevices::colorRampPalette(selected_colours))(50)

p <- ggplot() +
  # Background fill
  # Noisy background base
  geom_raster(data = perlin_noise, aes(x, y, fill = noise)) +
  scale_fill_gradientn(colours = noise_gradient) +
  ggnewscale::new_scale_fill() +
  # Layer ridges from background to foreground
  # Ridge 5
  ggfx::with_halftone_dither(
    geom_area(
      data = ridge05, aes(x = x, y = y_nudged), fill = "#5DAC97"),
    map_size = 6) +
  ggfx::with_blur(
    geom_area(
      data = ridge05_depth, aes(x = x, y = y_nudged),
      fill = "#D7EAE3", alpha = 0.8),
    sigma = 50) +
  # Ridge 4
  ggfx::with_ordered_dither(
    geom_area(
      data = ridge04, aes(x = x, y = y_nudged), fill = "#FF7733"),
    map_size = 8) +
  ggfx::with_blur(
    geom_area(
      data = ridge04_depth, aes(x = x, y = y_nudged),
      fill = "#FFC9AD", alpha = 0.8),
    sigma = 50) +
  # Ridge 3
  ggfx::with_halftone_dither(
    geom_area(
      data = ridge03, aes(x = x, y = y_nudged), fill = "#B44418"),
    map_size = 4) +
  ggfx::with_blur(
    geom_area(
      data = ridge03_depth, aes(x = x, y = y_nudged),
      fill = "#EEA081", alpha = 0.8),
    sigma = 50) +
  # Ridge 2
  ggfx::with_ordered_dither(
    geom_area(
      data = ridge02, aes(x = x, y = y_nudged), fill = "#BC4E85"),
    map_size = 2) +
  ggfx::with_blur(
    geom_area(
      data = ridge02_depth, aes(x = x, y = y_nudged),
      fill = "#D289AD", alpha = 0.8),
    sigma = 50) +
  # Ridge 1
  ggfx::with_halftone_dither(
    geom_area(
      data = ridge01, aes(x = x, y = y_nudged), fill = "#493657"),
    map_size = 4) +
  coord_cartesian(xlim = c(0,9), ylim = c(0,16), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#DDF1F8", colour = "#DDF1F8"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220125_camcorder", "day25_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 15,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Alfa Slab One", "alfa")
showtext_auto()

p <- ggplot() +
  # Background fill
  # Noisy background base
  geom_raster(data = perlin_noise, aes(x, y, fill = noise)) +
  scale_fill_gradientn(colours = noise_gradient) +
  ggnewscale::new_scale_fill() +
  # Layer ridges from background to foreground
  # Ridge 5
  ggfx::with_halftone_dither(
    geom_area(
      data = ridge05, aes(x = x, y = y_nudged), fill = "#5DAC97"),
    map_size = 6) +
  ggfx::with_blur(
    geom_area(
      data = ridge05_depth, aes(x = x, y = y_nudged),
      fill = "#D7EAE3", alpha = 0.8),
    sigma = 75) +
  # Ridge 4
  ggfx::with_ordered_dither(
    geom_area(
      data = ridge04, aes(x = x, y = y_nudged), fill = "#FF7733"),
    map_size = 8) +
  ggfx::with_blur(
    geom_area(
      data = ridge04_depth, aes(x = x, y = y_nudged),
      fill = "#FFC9AD", alpha = 0.8),
    sigma = 75) +
  # Ridge 3
  ggfx::with_halftone_dither(
    geom_area(
      data = ridge03, aes(x = x, y = y_nudged), fill = "#B44418"),
    map_size = 4) +
  ggfx::with_blur(
    geom_area(
      data = ridge03_depth, aes(x = x, y = y_nudged),
      fill = "#EEA081", alpha = 0.8),
    sigma = 75) +
  # Ridge 2
  ggfx::with_ordered_dither(
    geom_area(
      data = ridge02, aes(x = x, y = y_nudged), fill = "#BC4E85"),
    map_size = 2) +
  ggfx::with_blur(
    geom_area(
      data = ridge02_depth, aes(x = x, y = y_nudged),
      fill = "#D289AD", alpha = 0.8),
    sigma = 75) +
  # Ridge 1
  ggfx::with_halftone_dither(
    geom_area(
      data = ridge01, aes(x = x, y = y_nudged), fill = "#493657"),
    map_size = 4) +
  coord_cartesian(xlim = c(0,9), ylim = c(0,16), expand = FALSE) +
  # Plot titles and theming
  labs(
    title = "Over Yonder",
    subtitle = "#genuary2022 - Day 25",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "alfa", colour = "#655553"),
    plot.title = element_text(
      size = rel(14.5), margin = margin(5,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,15,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(15,0,5,0, unit = "pt")),
    legend.position = "none",
    panel.background = element_rect(fill = "#DDF1F8", colour = "#DDF1F8"),
    plot.background = element_rect(fill = "#DAD3D2", colour = "#DAD3D2"),
    plot.margin = margin(10, 20, 10, 20, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220125.png"),
  last_plot(), width = 16, height = 12, units = "cm", dpi = 600)

showtext_auto(FALSE)