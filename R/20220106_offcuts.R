# Load libraries ---------------------------------------------------------------

library(scrawl)
library(dplyr)
library(ggplot2)

# Offcut 01 --------------------------------------------------------------------

# Create data
seed_num <- 15

dat <- scrawl_build(
  seed = seed_num, n_paths = 2000, n_steps = 200, sz_step = 2, sz_slip = 4)

set.seed(seed_num)
dat_mod <- dat %>%
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.35,
      remainder == 2 ~ 0.3,
      TRUE           ~ 0.2)) %>%
  select(-remainder)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y, size = point_size)) +
  geom_point(alpha = 0.3, shape = 16, stroke = 0) +
  scale_size_identity() +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/offcuts/20220106_offcut01.png"),
  last_plot(), width = 8, height = 8, units = "cm",
  device = "png", type = "cairo")

# Offcut 02 --------------------------------------------------------------------

# Create data
seed_num <- 83

dat <- scrawl_build(
  seed = seed_num, n_paths = 2000, n_steps = 400, sz_step = 2, sz_slip = 1)

set.seed(seed_num)
dat_mod <- dat %>%
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.1,
      remainder == 1 ~ 0.175,
      remainder == 2 ~ 0.15,
      TRUE           ~ 0.1)) %>%
  select(-remainder)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y, size = point_size)) +
  geom_point(alpha = 0.3, shape = 16, stroke = 0) +
  scale_size_identity() +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/offcuts/20220106_offcut02.png"),
  last_plot(), width = 8, height = 8, units = "cm",
  device = "png", type = "cairo")

# Offcut 03 --------------------------------------------------------------------

# Create data
seed_num <- 83

dat <- scrawl_build(
  seed = seed_num, n_paths = 10, n_steps = 4000, sz_step = 1, sz_slip = 1)

set.seed(seed_num)
dat_mod <- dat %>%
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.1,
      remainder == 1 ~ 0.175,
      remainder == 2 ~ 0.15,
      TRUE           ~ 0.1)) %>%
  select(-remainder)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y, size = point_size)) +
  geom_point(alpha = 0.2, shape = 16, stroke = 0) +
  scale_size_identity() +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/offcuts/20220106_offcut03.png"),
  last_plot(), width = 10, height = 10, units = "cm",
  device = "png", type = "cairo")

# Offcut 04 --------------------------------------------------------------------

# Create data
seed_num <- 157

dat <- scrawl_build(
  seed = seed_num, n_paths = 1000, n_steps = 1000, sz_step = 1, sz_slip = 1)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.35,
      remainder == 2 ~ 0.3,
      TRUE           ~ 0.2)) %>%
  select(-remainder) %>%
  # For selected paths, delete some points along the way to mimic ink spray
  mutate(
    remainder = step_id %% 12,
    to_delete = case_when(
      remainder < 10 ~ TRUE,
      TRUE          ~ FALSE)) %>%
  group_by(path_id) %>%
  filter(to_delete == FALSE) %>%
  ungroup() %>%
  select(-remainder, -to_delete)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y, size = point_size)) +
  geom_point(alpha = 0.2, shape = 16, stroke = 0) +
  scale_size_identity() +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/offcuts/20220106_offcut04.png"),
  last_plot(), width = 20, height = 20, units = "cm",
  device = "png", type = "cairo")

# Offcut 05 --------------------------------------------------------------------

# Create data
seed_num <- 1257

dat <- scrawl_build(
  seed = seed_num, n_paths = 20, n_steps = 2000, sz_step = 1, sz_slip = 1)

set.seed(seed_num)
dat_mod <- dat %>%
  # Different point sizes for different "pen tip widths"
  mutate(
    remainder = path_id %% 4,
    point_size = case_when(
      remainder == 0 ~ 0.2,
      remainder == 1 ~ 0.35,
      remainder == 2 ~ 0.3,
      TRUE           ~ 0.2)) %>%
  select(-remainder)

# Build plot
p <- ggplot(dat_mod, aes(x = x, y = y, size = point_size)) +
  geom_point(alpha = 0.2, shape = 16, stroke = 0) +
  scale_size_identity() +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#e9e3d5", colour = "#e9e3d5"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/offcuts/20220106_offcut05.png"),
  last_plot(), width = 20, height = 20, units = "cm",
  device = "png", type = "cairo")

# Offcut 06 --------------------------------------------------------------------

# Create data
seed_num <- 27418

dat <- scrawl_build(
  seed = seed_num, n_paths = 2000, n_steps = 200, sz_step = 2, sz_slip = 4)

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

# Export to file
ggsave(
  here::here("img/offcuts/20220106_offcut06.png"),
  last_plot(), width = 8, height = 8, units = "cm",
  device = "png", type = "cairo")