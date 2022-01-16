# Load libraries ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(showtext)
library(ggplot2)
# library(magick)
library(grid)
library(png)

# Define custom functions ------------------------------------------------------

# Source: https://github.com/sharlagelfand/gradients

generate_gradient_colours <- function(colour_1, colour_2, size, probability) {
  sample(
    c(colour_1, colour_2),
    size = size,
    replace = TRUE,
    prob = c(probability, 1 - probability)
  )
}

generate_points_from_grid <- function(xmin, xmax, ymin, ymax, colour_1, colour_2, granularity = 100, horizontal = NULL) {
  x_size <- xmax - xmin
  y_size <- ymax - ymin
  
  grid <- expand.grid(
    x = seq(xmin, xmax, length.out = granularity * x_size),
    y = seq(ymin, ymax, length.out = granularity * y_size)
  ) %>%
    as_tibble() %>%
    filter(!x %in% c(xmin, xmax)) %>%
    filter(!y %in% c(ymin, ymax))
  
  if (colour_1 == colour_2) {
    grid <- grid %>%
      mutate(color = colour_1)
    
    return(grid)
  }
  
  if (is.null(horizontal)) {
    horizontal_gradient <- x_size > y_size
  } else {
    horizontal_gradient <- horizontal
  }
  
  if (horizontal_gradient) {
    grid <- grid %>%
      mutate(prop = (x - xmin) / x_size)
    
    sample_size <- grid %>%
      distinct(y) %>%
      nrow()
  } else {
    grid <- grid %>%
      mutate(prop = (y - ymin) / y_size)
    
    sample_size <- grid %>%
      distinct(x) %>%
      nrow()
  }
  
  grid %>%
    group_nest(prop) %>%
    mutate(color = map(
      prop,
      ~ generate_gradient_colours(
        colour_1 = colour_1,
        colour_2 = colour_2,
        size = sample_size,
        probability = .x
      )
    )) %>%
    unnest(cols = c(data, color))
}

# Create the data --------------------------------------------------------------

size_vec <- c(0.5, 0.35, 0.1) # modify width of segments
colour_1_vec <- c("#D2AB99", "#BDBEA9", "#CA2E55")
colour_2_vec <- c("#04724D", "#56876D", "#8DB38B")
horizontal_vec <- c(TRUE, FALSE)

set.seed(1234)
layout <- tibble(
  size = sample(size_vec, 10, replace = TRUE),
  colour_1 = sample(colour_1_vec, 10, replace = TRUE),
  colour_2 = sample(colour_2_vec, 10, replace = TRUE),
  horizontal = sample(horizontal_vec, 10, replace = TRUE)) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 4)

gradient_grid <- layout %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 500)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build gradients plot ---------------------------------------------------------

p <- ggplot() +
  geom_point(data = gradient_grid, aes(x = x, y = y, color = color), size = 0.01, shape = 15) +
  scale_color_identity() +
  theme_void() +
  coord_equal() +
  theme(
    plot.background = element_blank(),
    plot.margin = margin(-2, -2, -2, -2, unit = "cm"))

ggsave(
  here::here("img/20220116_gradients_only.jpg"),
  last_plot(), width = 8, height = 10, units = "cm", dpi = 600)

# Glitchin' hour ---------------------------------------------------------------

# Define custom functions, by Pete Jones:
# https://www.petejon.es/posts/2020-03-09-glitch-art-in-r/

######################################
# Glitch function - find-and-replace #
######################################
glitch_far2 <- function(input_data, n_changes = 5, tune = 100) {
  glitched <- input_data
  for (i in 1:n_changes) {
    change_rows <- grep(pattern = sample(unique(input_data), 1), x = input_data)
    change_samp <- sample(change_rows, 
                          (length(change_rows) / 100) * min(tune, 100))
    change_to <- sample(unique(input_data), 1)
    for (safe in change_samp) {
      if(safe > (length(input_data) / 100) * 0.5) {
        glitched[safe] <- change_to
      }
    }
  }
  glitched
}

# Read in the gradients image as a data file
grad_data <- readr::read_file_raw(
  here::here("img/20220116_gradient_only.jpg"))

# Might glitch me once
set.seed(543)
glitch_01 <- glitch_far2(grad_data, n_changes = 1, tune = 7)

# Won't let you glitch me twice
set.seed(14207)
glitch_02 <- glitch_far2(grad_data, n_changes = 1, tune = 7)

# Convert to {magick} objects
grad_img <- magick::image_read(grad_data)
glitch_01_img <- magick::image_read(glitch_01)
glitch_02_img <- magick::image_read(glitch_02)

# Append images
img_vec <- c(grad_img, glitch_01_img, glitch_02_img)

img_appended <- magick::image_append(
  magick::image_scale(img_vec, "x1000"))

# Save appended image
magick::image_write(
  img_appended,
  path = here::here("img/20220116_with_glitches.png"),
  format = "png")

# Shift to {ggplot2} workflow --------------------------------------------------

# Import Google Fonts
font_add_google("Bai Jamjuree", "baijam")
showtext_auto()

# Import appended image of glitchy gradients
glitchy_grad <- readPNG(here::here("img/20220116_with_glitches.png"))
glitchy_grad_raster <- rasterGrob(
  glitchy_grad, width = unit(1,"npc"), height = unit(1,"npc"))

# Build plot
p <- ggplot() +
  annotation_custom(glitchy_grad_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "Glitch Me Twice",
    subtitle = "#genuary2022 - Day 16",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "baijam", colour = "#BDBEA9"),
    plot.title = element_text(
      size = rel(9), face = "bold", margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(4), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(4), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#141204", colour = "#141204"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220116.png"),
  last_plot(), width = 10, height = 7, units = "cm", dpi = 600)

showtext_auto(FALSE)