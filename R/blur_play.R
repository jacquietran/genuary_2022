# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggfx)
library(ggplot2)

# Create data ------------------------------------------------------------------

# Define seeds
seed_num <- 7291

# Create data
set.seed(seed_num)
dots <- tibble(
  x = sample(seq(0, 10, by = 0.01), 50, replace = FALSE),
  y = sample(seq(0, 10, by = 0.01), 50, replace = FALSE),
  colour = sample(c("#042A2B",
                    "#5EB1BF",
                    "#CDEDF6"), 50, replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  with_blur(
    geom_point(data = dots, aes(x = x, y = y, colour = colour),
               size = 100, alpha = 0.5),
    sigma = 100) +
  scale_colour_identity() +
  theme_void() +
  coord_equal()

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/offcuts/blurry_background_01.png"),
  last_plot(), width = 8, height = 8, units = "cm", dpi = 300)