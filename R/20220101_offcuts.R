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

tumbleweed <- "#F3B18B"
medium_candy_apple_red <- "#E3002A"
tangerine <- "#F5880B"
light_sea_green <- "#00AA9E"
rufous <- "#B20000"
safety_orange <- "#F5640A"

set.seed(1578)

layout <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1,
  
  0, 1, 0.75, 1, tumbleweed, tumbleweed,
  1, 3, 0.75, 1, light_sea_green, light_sea_green,
  # 0, 1, 3, 3.05, medium_candy_apple_red, medium_candy_apple_red,
  # 1, 3, 3, 3.05, tangerine, tangerine,
  0, 1, 2.75, 6, medium_candy_apple_red, rufous,
  1, 3, 2.75, 6, tangerine, safety_orange,
  0, 1, 1, 3.25, tumbleweed, medium_candy_apple_red,
  1, 3, 1, 3.25, light_sea_green, tangerine)

gradient_grid <- layout %>%
  mutate(
    points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2),
                  generate_points_from_grid,
                  granularity = 600)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Build plot -------------------------------------------------------------------

# Import Google Fonts
font_add_google("Prata", "prata")

showtext_auto()

# Build plot
p <- gradient_grid %>%
  ggplot(aes(x = x, y = y, color = color)) +
  geom_point(size = 0.005, shape = 15) +
  scale_color_identity() +
  labs(
    caption = "Beach Sunset\n@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "prata"),
    plot.caption = element_text(size = 16, lineheight = 0.4, hjust = 0.945),
    plot.background = element_rect(
      fill = "#FFFFFF", colour = "#FFFFFF"),
    plot.margin = margin(10,0,10,0, unit = "pt"))
  
#theme(
 #   text = element_text(colour = "#5c94fc", family = "pressstart", size = 24),
  #  plot.title = element_text(colour = "#FFFFFF"),
   # plot.background = element_rect(fill = "#161c28", colour = "#161c28"),
    #plot.margin = margin(5,0,5,0, unit = "pt"))

# Export to PNG
ggsave(
  here::here("img/20220101_offcut01.png"),
  last_plot(), width = 8, height = 8, units = "cm", dpi = 320)

showtext_auto(FALSE)
