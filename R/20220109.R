# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(ggforce)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220109_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Create data ------------------------------------------------------------------

# Define seeds
seed_num <- 187370

set.seed(seed_num)
seed_vec <- sample(1:8000, 25, replace = FALSE)

# Left structure - dots
set.seed(seed_num)
dots_left <- tibble(
  x = sample(seq(0, 4, by = 0.01), 80, replace = FALSE),
  y = sample(seq(0, 8, by = 0.01), 80, replace = FALSE)) %>%
  # Remove selected dots to suggest perspective
  mutate(
    to_remove = case_when(
      x < 1 & y > 6.5 ~ "yes",
      x < 1 & y < 1.5 ~ "yes",
      x < 2 & y > 7   ~ "yes",
      x < 2 & y < 1   ~ "yes",
      TRUE            ~ "no")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Left structure - colour tile 1
set.seed(seed_num)
dots_left_tile_01 <- dots_left %>%
  # Remove selected dots to isolate coloured tile
  mutate(
    rando = sample(1:15, n(), replace = TRUE),
    to_remove = case_when(
      rando %in% c(10,11) ~ "no",
      TRUE                ~ "yes")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Left structure - colour tile 2
set.seed(seed_num)
dots_left_tile_02 <- dots_left %>%
  # Remove selected dots to isolate coloured tile
  mutate(
    rando = sample(1:15, n(), replace = TRUE),
    to_remove = case_when(
      rando %in% c(6,7) ~ "no",
      TRUE              ~ "yes")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Right structure - dots
set.seed(seed_num)
dots_right <- tibble(
  x = sample(seq(4.2, 12, by = 0.01), 120, replace = FALSE),
  y = sample(seq(0, 8.5, by = 0.01), 120, replace = FALSE)) %>%
  # Remove selected dots to suggest perspective
  mutate(
    to_remove = case_when(
      x > 11 & y > 6.5 ~ "yes",
      x > 11 & y < 1.5 ~ "yes",
      x > 10 & y > 7   ~ "yes",
      x > 10 & y < 1   ~ "yes",
      x > 9 & y > 6   ~ "yes",
      x > 9 & y < 2   ~ "yes",
      TRUE             ~ "no")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Right structure - colour tile 1
set.seed(seed_num)
dots_right_tile_01 <- dots_right %>%
  # Remove selected dots to isolate coloured tile
  mutate(
    rando = sample(1:15, n(), replace = TRUE),
    to_remove = case_when(
      rando %in% c(12,14) ~ "no",
      TRUE                ~ "yes")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Right structure - colour tile 2
set.seed(seed_num)
dots_right_tile_02 <- dots_right %>%
  # Remove selected dots to isolate coloured tile
  mutate(
    rando = sample(1:15, n(), replace = TRUE),
    to_remove = case_when(
      rando %in% c(2,5) ~ "no",
      TRUE                ~ "yes")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Right structure - colour tile 3
set.seed(seed_num)
dots_right_tile_03 <- dots_right %>%
  # Remove selected dots to isolate coloured tile
  mutate(
    rando = sample(1:15, n(), replace = TRUE),
    to_remove = case_when(
      rando == 1 ~ "no",
      TRUE       ~ "yes")) %>%
  filter(to_remove == "no") %>%
  select(-to_remove)

# Right structure - jitter
purrr::map_df(seed_vec, function(i) {
  
  set.seed(i)
  dots_nudged <- dots_right %>%
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
  
}) -> dots_right_jittered

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  # Left structure - background colour
  geom_delaunay_tile(
    data = dots_left,
    aes(x = x, y = y, group = -1L),
    fill = "#4c4c50") +
  # Left structure - colour tile 1
  geom_delaunay_tile(
    data = dots_left_tile_01,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#a98273") +
  # Left structure - colour tile 2
  geom_delaunay_tile(
    data = dots_left_tile_02,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#939798") +
  # Left structure - dots
  geom_delaunay_segment2(
    data = dots_left,
    aes(x = x, y = y, group = -1L), colour = "#343437", lineend = "round") +
  # Right structure - background colour
  geom_delaunay_tile(
    data = dots_right,
    aes(x = x, y = y, group = -1L),
    fill = "#b1b1b6") +
  # Right structure - colour tile 1
  geom_delaunay_tile(
    data = dots_right_tile_01,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#d3aea0") +
  # Right structure - colour tile 2
  geom_delaunay_tile(
    data = dots_right_tile_02,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#c2cacb") +
  # Right structure - colour tile 3
  geom_delaunay_tile(
    data = dots_right_tile_03,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#d3aea0") +
  # Right structure - jittered
  geom_delaunay_segment2(
    data = dots_right_jittered,
    aes(x = x_nudged, y = y_nudged, group = -1L),
    colour = "#343437", size = 0.1, alpha = 0.25, lineend = "round") +
  scale_y_continuous(limits = c(-2, 12)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#e6ece8", colour = "#e6ece8"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220109_camcorder", "day09_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Cairo", "cairo")
showtext_auto()

p <- ggplot() +
  # Left structure - base colour
  geom_delaunay_tile(
    data = dots_left,
    aes(x = x, y = y, group = -1L),
    fill = "#4c4c50") +
  # Left structure - colour tile 1
  geom_delaunay_tile(
    data = dots_left_tile_01,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#a98273") +
  # Left structure - colour tile 2
  geom_delaunay_tile(
    data = dots_left_tile_02,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#939798") +
  # Left structure - dots
  geom_delaunay_segment2(
    data = dots_left,
    aes(x = x, y = y, group = -1L), colour = "#343437", lineend = "round") +
  # Right structure - base colour
  geom_delaunay_tile(
    data = dots_right,
    aes(x = x, y = y, group = -1L),
    fill = "#b1b1b6") +
  # Right structure - colour tile 1
  geom_delaunay_tile(
    data = dots_right_tile_01,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#d3aea0") +
  # Right structure - colour tile 2
  geom_delaunay_tile(
    data = dots_right_tile_02,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#c2cacb") +
  # Right structure - colour tile 3
  geom_delaunay_tile(
    data = dots_right_tile_03,
    aes(x = x, y = y, group = -1L),
    alpha = 1, fill = "#d3aea0") +
  # Right structure - jittered
  geom_delaunay_segment2(
    data = dots_right_jittered,
    aes(x = x_nudged, y = y_nudged, group = -1L),
    colour = "#343437", size = 0.1, alpha = 0.25, lineend = "round") +
  scale_y_continuous(limits = c(0,8)) +
  # Plot titles and theming
  labs(
    title = "Fed^2",
    subtitle = "#genuary2022 - Day 9",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "", colour = "#4c4c50"),
    plot.title = element_text(
      size = rel(14), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#e6ece8", colour = "#e6ece8"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220109.png"),
  last_plot(), width = 16, height = 9, units = "cm", dpi = 600)

beepr::beep(5)

showtext_auto(FALSE)