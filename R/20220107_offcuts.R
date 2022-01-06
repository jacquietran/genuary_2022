# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Offcut 01 --------------------------------------------------------------------

# Define seeds
seed_num <- 500

set.seed(seed_num)
seed_vec <- sample(1:8000, 50, replace = FALSE)

# Create data
# Base frame
set.seed(seed_num)
dots <- tibble(
  x = sample(seq(0, 8, by = 0.01), 50, replace = FALSE),
  y = sample(seq(0, 10, by = 0.01), 50, replace = FALSE))

# Tight jitter
purrr::map_df(seed_vec, function(i) {
  
  set.seed(i)
  dots_nudged <- dots %>%
    mutate(
      run_id = as.factor(i),
      operator_for_x = sample(c("add", "subtract"), n(), replace = TRUE),
      operator_for_y = sample(c("add", "subtract"), n(), replace = TRUE),
      modifier_for_x = sample(seq(0, 0.1, by = 0.005), n(), replace = TRUE),
      modifier_for_y = sample(seq(0, 0.1, by = 0.003), n(), replace = TRUE),
      x_nudged = case_when(
        operator_for_x == "add"      ~ x + modifier_for_x,
        operator_for_x == "subtract" ~ x - modifier_for_x),
      y_nudged = case_when(
        operator_for_y == "add"      ~ y + modifier_for_y,
        operator_for_y == "subtract" ~ y - modifier_for_y)) %>%
    select(x, y, x_nudged, y_nudged, run_id)

 }) -> dots_jittered_tight

# Loose jitter
purrr::map_df(seed_vec, function(i) {
  
  set.seed(i)
  dots_nudged <- dots %>%
    mutate(
      run_id = as.factor(i),
      operator_for_x = sample(c("add", "subtract"), n(), replace = TRUE),
      operator_for_y = sample(c("add", "subtract"), n(), replace = TRUE),
      modifier_for_x = sample(seq(0, 0.3, by = 0.07), n(), replace = TRUE),
      modifier_for_y = sample(seq(0, 0.3, by = 0.12), n(), replace = TRUE),
      x_nudged = case_when(
        operator_for_x == "add"      ~ x + modifier_for_x,
        operator_for_x == "subtract" ~ x - modifier_for_x),
      y_nudged = case_when(
        operator_for_y == "add"      ~ y + modifier_for_y,
        operator_for_y == "subtract" ~ y - modifier_for_y)) %>%
    select(x, y, x_nudged, y_nudged, run_id)
  
}) -> dots_jittered_loose

# Build plot
p <- ggplot() +
  ggfx::with_blur(
    geom_point(
      data = dots, aes(x = x, y = y),
      size = 12, alpha = 0.2, shape = 16, stroke = 0, colour = "#FFF8F0"),
    sigma = 5) +
  geom_delaunay_segment2(
    data = dots_jittered_loose,
    aes(x = x_nudged, y = y_nudged, group = run_id),
    size = 0.1, alpha = 0.05, colour = "#FFF8F0",
    linetype = "dotted", lineend = "round") +
  geom_delaunay_segment2(
    data = dots_jittered_tight,
    aes(x = x_nudged, y = y_nudged, group = run_id),
    size = 0.1, alpha = 0.05, colour = "#FFF8F0", lineend = "round") +
  theme_void() +
  coord_equal() +
  theme(
    plot.background = element_rect(fill = "#1E1E24", colour = "#1E1E24"))

# Export to file
ggsave(
  here::here("img/offcuts/20220107_offcut01.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)

# Offcut 02 --------------------------------------------------------------------

# Define seeds
seed_num <- 189

set.seed(seed_num)
seed_vec <- sample(1:8000, 3, replace = FALSE)

# Create data
# Base frame
set.seed(seed_num)
dots <- tibble(
  x = sample(seq(0, 8, by = 0.01), 50, replace = FALSE),
  y = sample(seq(0, 10, by = 0.01), 50, replace = FALSE))

# Jittered
purrr::map_df(seed_vec, function(i) {
  
  set.seed(i)
  dots_nudged <- dots %>%
    mutate(
      run_id = as.factor(i),
      operator_for_x = sample(c("add", "subtract"), n(), replace = TRUE),
      operator_for_y = sample(c("add", "subtract"), n(), replace = TRUE),
      modifier_for_x = sample(seq(0, 0.1, by = 0.005), n(), replace = TRUE),
      modifier_for_y = sample(seq(0, 0.1, by = 0.003), n(), replace = TRUE),
      x_nudged = case_when(
        operator_for_x == "add"      ~ x + modifier_for_x,
        operator_for_x == "subtract" ~ x - modifier_for_x),
      y_nudged = case_when(
        operator_for_y == "add"      ~ y + modifier_for_y,
        operator_for_y == "subtract" ~ y - modifier_for_y)) %>%
    select(x, y, x_nudged, y_nudged, run_id)
  
}) -> dots_jittered

# Reduce to 50 data points total (across 3 groups)
set.seed(seed_num)
dots_jittered_reduced <- dots_jittered %>%
  mutate(rando = sample(1:3, n(), replace = TRUE)) %>%
  filter(rando == 1) %>%
  select(-rando)

# Build plot
p <- ggplot() +
  ggfx::with_blur(
    geom_delaunay_segment2(
      data = dots_jittered_reduced,
      aes(x = x_nudged, y = y_nudged, group = run_id, colour = run_id),
      size = 0.1, lineend = "round"),
    sigma = 10) +
  geom_delaunay_segment2(
    data = dots_jittered_reduced,
    aes(x = x_nudged, y = y_nudged, group = run_id, colour = run_id),
    size = 0.1, lineend = "round") +
  scale_colour_manual(
    values = c("#86CD82", "#D6EFFF", "#FE654F")) +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#121215", colour = "#121215"))

# Export to file
ggsave(
  here::here("img/offcuts/20220107_offcut02.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)

# Offcut 03 --------------------------------------------------------------------

# Define seeds
seed_num <- 1809

set.seed(seed_num)
seed_vec <- sample(1:8000, 2, replace = FALSE)

# Create data
# Base frame
set.seed(seed_num)
dots <- tibble(
  x = sample(seq(0, 5, by = 0.01), 10, replace = FALSE),
  y = sample(seq(0, 10, by = 0.01), 10, replace = FALSE))

# Jittered
purrr::map_df(seed_vec, function(i) {
  
  set.seed(i)
  dots_nudged <- dots %>%
    mutate(
      run_id = as.factor(i),
      operator_for_x = sample(c("add", "subtract"), n(), replace = TRUE),
      operator_for_y = sample(c("add", "subtract"), n(), replace = TRUE),
      modifier_for_x = sample(seq(0, 0.5, by = 0.05), n(), replace = TRUE),
      modifier_for_y = sample(seq(0, 0.5, by = 0.045), n(), replace = TRUE),
      x_nudged = case_when(
        operator_for_x == "add"      ~ x + modifier_for_x,
        operator_for_x == "subtract" ~ x - modifier_for_x),
      y_nudged = case_when(
        operator_for_y == "add"      ~ y + modifier_for_y,
        operator_for_y == "subtract" ~ y - modifier_for_y)) %>%
    select(x, y, x_nudged, y_nudged, run_id)
  
}) -> dots_jittered

# Build plot
p <- ggplot() +
  ggfx::with_blur(
    geom_delaunay_segment2(
      data = dots_jittered,
      aes(x = x_nudged, y = y_nudged, group = run_id, colour = run_id),
      size = 2, lineend = "round"),
    sigma = 25) +
  geom_delaunay_segment2(
    data = dots_jittered,
    aes(x = x_nudged, y = y_nudged, group = run_id, colour = run_id),
    size = 1, lineend = "round") +
  scale_colour_manual(
    values = c("#f969d6", "#93bbff")) +
  theme_void() +
  coord_equal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#14346b", colour = "#14346b"))

# Export to file
ggsave(
  here::here("img/offcuts/20220107_offcut03.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)