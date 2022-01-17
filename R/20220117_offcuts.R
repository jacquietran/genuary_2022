# Load libraries ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
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

# Offcut 1 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#3A3042" # violet
col2 <- "#FF784F" # orange
col3 <- "#E2FFC2" # tea_green

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, FALSE,
  0.6, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut01.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 2 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#114B5F" # eagle_green
col2 <- "#456990" # queen_blue
col3 <- "#F45B69" # fiery_rose

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, FALSE,
  0.6, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut02.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 3 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#BBDB9B" # granny_smith_apple
col2 <- "#ABC4A1" # laurel_green
col3 <- "#9DB4AB" # opal

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, FALSE,
  0.6, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut03.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 4 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#D8DBE2" # gainsboro
col2 <- "#58A4B0" # cadet_blue
col3 <- "#DAA49A" # pastel_pink

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, FALSE,
  0.6, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut04.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 5 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#FFAE03" # honey_yellow
col2 <- "#FE4E00" # intl_orange_aerospace
col3 <- "#FF0F80" # rose

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, TRUE,
  0.25, col1, col2, TRUE,
  0.5, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, TRUE,
  0.15, col3, col2, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut05.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 6 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#FFAE03" # honey_yellow
col2 <- "#FE4E00" # intl_orange_aerospace
col3 <- "#FF0F80" # rose

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut06.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 7 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#73D2DE" # sky_blue_crayola
col2 <- "#D81159" # ruby
col3 <- "#218380" # celadon_green

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut07.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 8 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#F4743B" # orange_crayola
col2 <- "#BEEE62" # inchworm
col3 <- "#3C6E71" # ming

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut08.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 9 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#1C77C3" # spanish_blue
col2 <- "#F39237" # deep_saffron
col3 <- "#D63230" # geranium_lake

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut09.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 10 --------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#FF99C8" # carnation_pink
col2 <- "#E4C1F9" # mauve
col3 <- "#FCF6BD" # blond

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut10.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 11 --------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#FF99C8" # carnation_pink
col2 <- "#E4C1F9" # mauve
col3 <- "#FCF6BD" # blond

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, TRUE,
  0.3, col1, col2, TRUE,
  0.1, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.15, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.1, col1, col2, TRUE,
  0.1, col1, col3, TRUE,
  0.1, col3, col2, TRUE,
  0.1, col1, col2, TRUE,
  0.1, col1, col3, TRUE,
  0.1, col3, col2, TRUE,
  0.1, col1, col2, TRUE,
  0.1, col1, col3, TRUE,
  0.1, col3, col2, TRUE,
  0.1, col1, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.15)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut11.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 12 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#88CCF1" # light_cornflower_blue
col2 <- "#C1DFF0" # columbia_blue
col3 <- "#2D898B" # dark_cyan

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, TRUE,
  0.25, col1, col2, TRUE,
  0.5, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, TRUE,
  0.15, col3, col2, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut12.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 13 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#104F55" # eagle_green
col2 <- "#223B59" # prussian_blue
col3 <- "#01200F" # dark_green

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, TRUE,
  0.25, col1, col2, TRUE,
  0.5, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, TRUE,
  0.15, col3, col2, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut13.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 14 ---------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#333333" # jet
col2 <- "#2C4251" # charcoal
col3 <- "#9D2F2F" # auburn

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col3, col1, TRUE,
  0.25, col1, col2, TRUE,
  0.5, col2, col1, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

## row 2 #####
set.seed(7810)
row2 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.25, col1, col2, FALSE,
  0.6, col1, col3, TRUE,
  0.15, col3, col2, FALSE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 0.35)

gradient_grid_row2 <- row2 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  geom_point(
    data = gradient_grid_row2,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut14.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)

# Offcut 15 --------------------------------------------------------------------

# Create the data --------------------------------------------------------------

# set custom colours
col1 <- "#FFB997" # mac_and_cheese
col2 <- "#F67E7D" # light_coral
col3 <- "#843B62" # twilight_lavender

## row 1 #####
set.seed(1921)
row1 <- tribble(
  ~size, ~colour_1, ~colour_2, ~horizontal,
  
  0.6, col3, col1, FALSE,
  0.3, col2, col1, TRUE,
  0.1, col3, col2, TRUE) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0.35, ymax = 1.2)

gradient_grid_row1 <- row1 %>%
  mutate(points = pmap(
    list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
    generate_points_from_grid, granularity = 1000)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.margin = margin(-0.5, -0.5, -0.5, -0.5, unit = "cm"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20220117_offcut15.png"),
  last_plot(), width = 6.5, height = 6.5, units = "cm", dpi = 600)