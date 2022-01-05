# Load libraries ---------------------------------------------------------------

library(scrawl)
library(dplyr)
library(showtext)
library(ggplot2)
library(camcorder)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220106_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Pass 01 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 100, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 02 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 200, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 03 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 300, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 04 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 400, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 05 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 500, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 06 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 600, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 07 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 700, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 08 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 800, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 09 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 900, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Pass 10 ----------------------------------------------------------------------

# Create data
seed_num <- 7842192

dat <- scrawl_build(
  seed = seed_num, n_paths = 1000, n_steps = 600, sz_step = 2, sz_slip = 3)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.3,
      remainder == 2 ~ 0.27,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # Remove observations outside the "square"
  filter(x >= 0 & x < 2) %>%
  filter(y >= 0 & y < 2)

# For drawing a "square" around the plot
# Data for square - top length
set.seed(seed_num)
dat_square_top <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 2, yend = 2) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - right length
set.seed(seed_num)
dat_square_right <- tibble(
  x = 2, xend = 2,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - bottom length
set.seed(seed_num)
dat_square_bottom <- tibble(
  x = seq(0, 1.99, by = 0.01), xend = seq(0.01, 2, by = 0.01),
  y = 0, yend = 0) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Data for square - left length
set.seed(seed_num)
dat_square_left <- tibble(
  x = 0, xend = 0,
  y = seq(0, 1.99, by = 0.01), yend = seq(0.01, 2, by = 0.01)) %>%
  # Pseudo-random deletion
  mutate(
    to_delete = sample(
      c(rep("yes", 1), rep("no", 6)),
      n(), replace = TRUE)) %>%
  filter(to_delete == "no") %>%
  select(-to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220106_camcorder", "day06_gif.gif"),
  first_image_duration = 1,
  last_image_duration = 15,
  frame_duration = 1)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Rock Salt", "rocksalt")
showtext_auto()

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y))
p <- p + geom_point(aes(size = point_size), alpha = 0.3, shape = 16, stroke = 0)
p <- p + scale_size_identity()
# Draw square - top length
p <- p + geom_segment(
  data = dat_square_top,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - right length
p <- p + geom_segment(
  data = dat_square_right,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - bottom length
p <- p + geom_segment(
  data = dat_square_bottom,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Draw square - left length
p <- p + geom_segment(
  data = dat_square_left,
  aes(x = x, xend = xend, y = y, yend = yend, size = 0.25))
# Plot titles and theming
p <- p + labs(
  title = "Style Guide",
  subtitle = "#genuary2022 - Day 6",
  caption = "@jacquietran")
p <- p + theme_void()
p <- p + coord_equal()
p <- p + theme(
  text = element_text(family = "rocksalt"),
  plot.title = element_text(
    size = rel(4), margin = margin(0,0,5,0, unit = "pt")),
  plot.subtitle = element_text(
    size = rel(2.5), margin = margin(0,0,10,0, unit = "pt")),
  plot.caption = element_text(
    size = rel(2.5), margin = margin(10,0,0,0, unit = "pt")),
  legend.position = "none",
  plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220106.png"),
  last_plot(), width = 10, height = 10, units = "cm",
  device = "png", type = "cairo")

showtext_auto(FALSE)