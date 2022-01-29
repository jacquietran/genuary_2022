# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Create data ------------------------------------------------------------------

seed_num <- 3

# A straight line
straight_line <- tibble(
  x = 0, xend = 2,
  y = 1, yend = 1)

# Jitter the start and end points
set.seed(seed_num)
straight_line_jittered <- tibble(
  x = 0, xend = 2,
  y = sample(seq(0.9, 1.1, by = 0.02), 10, replace = TRUE),
  yend = sample(seq(0.9, 1.1, by = 0.02), 10, replace = TRUE))

# Jitter on both axes
set.seed(seed_num)
both_axes_jittered <- tibble(
  x = sample(seq(0, 2, by = 0.02), 5000, replace = TRUE),
  xend = sample(seq(0, 2, by = 0.02), 5000, replace = TRUE),
  y = sample(seq(0.9, 1.1, by = 0.02), 5000, replace = TRUE),
  yend = sample(seq(0.9, 1.1, by = 0.02), 5000, replace = TRUE),
  linesize = sample(seq(0.05, 0.25, by = 0.05), 5000, replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_segment(
    data = both_axes_jittered,
    aes(x = x, xend = xend, y = y, yend = yend, size = linesize),
    alpha = 0.1, colour = "#1e1e1e") +
  scale_size_identity() +
  scale_y_continuous(limits = c(0,2)) +
  scale_x_continuous(limits = c(0,2)) +
  coord_fixed()

# Export to file ---------------------------------------------------------------
