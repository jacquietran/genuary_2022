# Load libraries ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(ggfx)
library(showtext)
library(ggplot2)

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

# Create circles data ----------------------------------------------------------

# Set colours
# Palette from here: https://colorpalettes.net/color-palette-4224/
colour_vector <- c(
  "#09280a",
  "#446b04",
  "#e1c1b7",
  "#b05449",
  "#dbb238")

# Create the data
# Layer 1
set.seed(2005787)
layer_1 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1),
  layer_num = 1)

# Layer 2
set.seed(47698431)
layer_2 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1),
  layer_num = 2)

# Layer 3
set.seed(546861)
layer_3 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1),
  layer_num = 3)

# Layer 4
set.seed(20)
layer_4 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1),
  layer_num = 4)

# Layer 5
set.seed(84751)
layer_5 <- tibble(
  x = runif(1, min=0, max=5),
  y = runif(1, min=0, max=5),
  colour = sample(colour_vector, 1),
  layer_num = 5)

# Merge layers together
layers_merged <- bind_rows(
  layer_1, layer_2, layer_3, layer_4, layer_5) %>%
  mutate(
    point_size = case_when(
      layer_num == 1 ~ 0.7*60,
      layer_num == 2 ~ 0.7*105,
      layer_num == 3 ~ 0.7*45,
      layer_num == 4 ~ 0.7*210,
      layer_num == 5 ~ 0.7*144),
    x_adj = scales::rescale(x, to = c(0, 1)),
    y_adj = scales::rescale(y, to = c(0, 1)))

# Create gradient data ---------------------------------------------------------

# set custom colours
col1 <- "#FFAE03" # honey_yellow
col2 <- "#FE4E00" # intl_orange_aerospace
col3 <- "#FF0F80" # rose

## row 1 #####
set.seed(1921)
layout <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size), xmin = xmax - size,
    ymin = 0, ymax = 1)

gradient_grid <- layout %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Import fonts
font_add_google("Monoton", "monoton")
font_add_google("Unica One", "unica")
showtext_auto()

# Build plot
p <- ggplot() +
  as_reference(
    geom_point(
      data = gradient_grid,
      aes(x = x, y = y, color = color), size = 0.01, shape = 16),
    id = "gradient") +
  with_blend(
    with_halftone_dither(
      geom_point(
        data = layers_merged,
        aes(x = x_adj, y = y_adj, group = layer_num,
            colour = colour, size = point_size),
      shape = 16, stroke = 0),
      map_size = 8),
    bg_layer = "gradient",
    blend_type = "exclusion") +
  scale_colour_identity() +
  scale_size_identity() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  # Plot titles and theming
  labs(
    title = "exclzn",
    subtitle = "#genuary2022 - Day 21",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(colour = "#000000"),
    plot.title = element_text(
      size = rel(18.35), margin = margin(0, 0, 5, 0, unit = "pt"),
      family = "monoton", face = "bold"),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0, 0, 10, 0, unit = "pt"),
      family = "unica"),
    plot.caption = element_text(
      size = rel(8), margin = margin(10, 0, 0, 0, unit = "pt"),
      family = "unica"),
    plot.background = element_rect(fill = "#FDF7ED", colour = "#FDF7ED"),
    panel.background = with_halftone_dither(
      element_rect(fill = "#F8DAA8", colour = "#F8DAA8"),
      map_size = 4),
    plot.margin = margin(10, 20, 10, 20, unit = "pt"))

# Export to PNG ----------------------------------------------------------------

ggsave(
  here::here("img/20220121.png"),
  last_plot(), width = 10, height = 12, units = "cm", dpi = 620)

showtext_auto(FALSE)
