# Load libraries ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
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

# Import Google Fonts
font_add_google("Jost", "jost")
showtext_auto()

# Build plot
p <- ggplot() +
  geom_point(
    data = gradient_grid_row1,
    aes(x = x, y = y, color = color), size = 0.001, shape = 16) +
  scale_color_identity() +
  # Plot titles and theming
  labs(
    title = "Scorcher",
    subtitle = "#genuary2022 - Day 17",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "jost", colour = "#1F1924"),
    plot.title = element_text(
      size = rel(10), hjust = 0.065, face = "bold"),
    plot.subtitle = element_text(
      size = rel(5), hjust = 0.065),
    plot.caption = element_text(
      size = rel(5), hjust = 0.945),
    plot.background = element_rect(fill = "#FFEDC2", colour = "#FFEDC2"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/20220117.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 620)

showtext_auto(FALSE)