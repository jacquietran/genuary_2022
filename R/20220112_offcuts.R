# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ambient)
library(ggfx)

## Set rhombus parameters ------------------------------------------------------

side_length <- 3
angle_A <- 60 * (pi / 180) # acute angle, in radians
# angle_B <- 120 * (pi / 180) # obtuse angle, in radians
rhom_h <- side_length * (sin(angle_A))
diagonal_p <- side_length * sqrt(2 + (2*cos(angle_A)))
diagonal_q <- side_length * sqrt(2 - (2*cos(angle_A)))

## Define custom functions -----------------------------------------------------

## Generate left face data - two tiles #####
left_two_tiles <- function(x_start, y_start) {
  
  data <- tibble(
    coord_id = seq(1, 4, 1), # bottom left, bottom right, top right, top left
    x1 = c(
      x_start,          # bottom left
      x_start + rhom_h, # bottom right
      x_start + rhom_h, # top right
      x_start),         # top left
    y1 = c(
      y_start,                                  # bottom left
      y_start - (diagonal_q / 2),               # bottom right
      y_start - (diagonal_q / 2) + side_length, # top right
      y_start + side_length),                   # top left
    x2 = x1 + (2 * rhom_h),
    y2 = y1)
  
  return(data)
  
}

## Generate left face data - three tiles #####
left_three_tiles <- function(x_start, y_start) {
  
  data <- tibble(
    coord_id = seq(1, 4, 1), # bottom left, bottom right, top right, top left
    x1 = c(
      x_start,          # bottom left
      x_start + rhom_h, # bottom right
      x_start + rhom_h, # top right
      x_start),         # top left
    y1 = c(
      y_start,                                  # bottom left
      y_start - (diagonal_q / 2),               # bottom right
      y_start - (diagonal_q / 2) + side_length, # top right
      y_start + side_length),                   # top left
    x2 = x1 + (2 * rhom_h),
    y2 = y1,
    x3 = x2 + (2 * rhom_h),
    y3 = y1)
  
  return(data)
  
}

## Generate right face data - two tiles #####
right_two_tiles <- function(x_start, y_start) {
  
  data <- tibble(
    coord_id = seq(1, 4, 1), # bottom left, bottom right, top right, top left
    x1 = c(
      x_start,          # bottom left
      x_start + rhom_h, # bottom right
      x_start + rhom_h, # top right
      x_start),         # top left
    y1 = c(
      y_start,                                  # bottom left
      y_start + (diagonal_q / 2),               # bottom right
      y_start + (diagonal_q / 2) + side_length, # top right
      y_start + side_length),                   # top left
    x2 = x1 + (2 * rhom_h),
    y2 = y1)
  
  return(data)
  
}

## Generate right face data - three tiles #####
right_three_tiles <- function(x_start, y_start) {
  
  data <- tibble(
    coord_id = seq(1, 4, 1), # bottom left, bottom right, top right, top left
    x1 = c(
      x_start,          # bottom left
      x_start + rhom_h, # bottom right
      x_start + rhom_h, # top right
      x_start),         # top left
    y1 = c(
      y_start,                                  # bottom left
      y_start + (diagonal_q / 2),               # bottom right
      y_start + (diagonal_q / 2) + side_length, # top right
      y_start + side_length),                   # top left
    x2 = x1 + (2 * rhom_h),
    y2 = y1,
    x3 = x2 + (2 * rhom_h),
    y3 = y1)
  
  return(data)
  
}

## Generate top face data - three tiles
top_three_tiles <- function(x_start, y_start) {
  
  data <- tibble(
    coord_id = seq(1, 4, 1), # west, south, east, north
    x1 = c(
      x_start,                     # west
      x_start + (diagonal_p / 2),  # south
      x_start + diagonal_p,        # east
      x_start + (diagonal_p / 2)), # north
    y1 = c(
      y_start,                     # west
      y_start - (diagonal_q / 2),  # south
      y_start,                     # east
      y_start + (diagonal_q / 2)), # north
    x2 = x1 + diagonal_p,
    y2 = y1,
    x3 = x2 + diagonal_p,
    y3 = y1)
  
    return(data)
    
}

## Generate top face data - four tiles
top_four_tiles <- function(x_start, y_start) {
  
  data <- tibble(
    coord_id = seq(1, 4, 1), # west, south, east, north
    x1 = c(
      x_start,                     # west
      x_start + (diagonal_p / 2),  # south
      x_start + diagonal_p,        # east
      x_start + (diagonal_p / 2)), # north
    y1 = c(
      y_start,                     # west
      y_start - (diagonal_q / 2),  # south
      y_start,                     # east
      y_start + (diagonal_q / 2)), # north
    x2 = x1 + diagonal_p,
    y2 = y1,
    x3 = x2 + diagonal_p,
    y3 = y1,
    x4 = x3 + diagonal_p,
    y4 = y1)
    
    return(data)
  
}

## Rhombus tidying #####
tidy_rhombus <- function(data, face_row_id) {
  
  tidy_data <- data %>%
    pivot_longer(
      cols = c(contains("x"), contains("y")),
      names_to = "coordinate") %>%
    separate("coordinate", into = c("coord", "tile_num"), sep = 1) %>%
    pivot_wider(
      id_cols = c("tile_num", "coord_id"),
      names_from = "coord",
      values_from = "value") %>%
    mutate(tile_id = glue::glue("{face_row_id}_tile{tile_num}")) %>%
    select(tile_id, coord_id, x, y) %>%
    arrange(tile_id, coord_id)
  
  return(tidy_data)
  
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Offcut 01 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_two_tiles(
  x_start = diagonal_p,
  y_start = (3 * side_length) + (2 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

## Row 4 #####
left_face_row4 <- left_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row4_tidy <- tidy_rhombus(left_face_row4, "left_row4")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

## Row 4 #####
right_face_row4 <- right_two_tiles(
  x_start = diagonal_p,
  y_start = 0)
right_face_row4_tidy <- tidy_rhombus(right_face_row4, "right_row4")

# Create data for plotting: Top faces ------------------------------------------

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_four_tiles(
  x_start = 0,
  y_start = (2 * side_length) + diagonal_q)
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

## Row 4 #####
top_face_row4 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
top_face_row4_tidy <- tidy_rhombus(top_face_row4, "top_row4")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy,
  left_face_row4_tidy) %>%
  mutate(fill = "#0E79B2")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy,
  right_face_row4_tidy) %>%
  mutate(fill = "#BF1363")

# Top faces
top_faces <- bind_rows(
  top_face_row2_tidy, top_face_row3_tidy, top_face_row4_tidy) %>%
  mutate(fill = "#F39237")

# Build plot -------------------------------------------------------------------

outline_colour <- "#FBFEF9"
bg_colour <- "#191923"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour) +
  # Right faces
  geom_polygon(
    data = right_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour) +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut01.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Offcut 02 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_two_tiles(
  x_start = diagonal_p,
  y_start = (3 * side_length) + (2 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

## Row 4 #####
left_face_row4 <- left_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row4_tidy <- tidy_rhombus(left_face_row4, "left_row4")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

## Row 4 #####
right_face_row4 <- right_two_tiles(
  x_start = diagonal_p,
  y_start = 0)
right_face_row4_tidy <- tidy_rhombus(right_face_row4, "right_row4")

# Create data for plotting: Top faces ------------------------------------------

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_four_tiles(
  x_start = 0,
  y_start = (2 * side_length) + diagonal_q)
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

## Row 4 #####
top_face_row4 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
top_face_row4_tidy <- tidy_rhombus(top_face_row4, "top_row4")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy,
  left_face_row4_tidy) %>%
  mutate(fill = "#0E79B2")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy,
  right_face_row4_tidy) %>%
  mutate(fill = "#BF1363")

# Top faces
top_faces <- bind_rows(
  top_face_row2_tidy, top_face_row3_tidy, top_face_row4_tidy) %>%
  mutate(fill = "#F39237")

# Build plot -------------------------------------------------------------------

outline_colour <- "#FBFEF9"
bg_colour <- "#191923"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    linetype = "dotted") +
  # Right faces
  geom_polygon(
    data = right_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    linetype = "dotted") +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    linetype = "dotted") +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut02.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Offcut 03 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_two_tiles(
  x_start = diagonal_p,
  y_start = (3 * side_length) + (2 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

## Row 4 #####
left_face_row4 <- left_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row4_tidy <- tidy_rhombus(left_face_row4, "left_row4")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

## Row 4 #####
right_face_row4 <- right_two_tiles(
  x_start = diagonal_p,
  y_start = 0)
right_face_row4_tidy <- tidy_rhombus(right_face_row4, "right_row4")

# Create data for plotting: Top faces ------------------------------------------

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_four_tiles(
  x_start = 0,
  y_start = (2 * side_length) + diagonal_q)
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

## Row 4 #####
top_face_row4 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
top_face_row4_tidy <- tidy_rhombus(top_face_row4, "top_row4")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy,
  left_face_row4_tidy) %>%
  mutate(fill = "#0E79B2")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy,
  right_face_row4_tidy) %>%
  mutate(fill = "#BF1363")

# Top faces
top_faces <- bind_rows(
  top_face_row2_tidy, top_face_row3_tidy, top_face_row4_tidy) %>%
  mutate(fill = "#F39237")

# Build plot -------------------------------------------------------------------

outline_colour <- "#FBFEF9"
bg_colour <- "#191923"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 3) +
  # Right faces
  geom_polygon(
    data = right_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 3) +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 3) +
  scale_fill_identity() +
  coord_cartesian(
    xlim = c(diagonal_p, (3 * diagonal_p)),
    ylim = c((side_length + (diagonal_q / 2)), ((3 * side_length) + (1.5 * diagonal_q)))) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut03.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Offcut 04 --------------------------------------------------------------------

# Create noisy background 1 ----------------------------------------------------

# Define seed
seed_num <- 15713

# Generate noise for plot background
set.seed(seed_num)
grid1 <- long_grid(
  x = seq(0, 20, length.out = 1000),
  y = seq(0, 20, length.out = 1000)) %>%
  mutate(
    noise = gen_value(x = x, y = y, frequency = 0.1, seed = seed_num))

# Create noisy background 2 ----------------------------------------------------

# Define seed
seed_num <- 320

# Generate noise for plot background
set.seed(seed_num)
grid2 <- long_grid(
  x = seq(0, 20, length.out = 1000),
  y = seq(0, 20, length.out = 1000)) %>%
  mutate(
    noise = gen_value(x = x, y = y, frequency = 0.1, seed = seed_num))

# Create noisy background 3 ----------------------------------------------------

# Define seed
seed_num <- 2891

# Generate noise for plot background
set.seed(seed_num)
grid3 <- long_grid(
  x = seq(0, 20, length.out = 1000),
  y = seq(0, 20, length.out = 1000)) %>%
  mutate(
    noise = gen_value(x = x, y = y, frequency = 0.1, seed = seed_num))

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_two_tiles(
  x_start = diagonal_p,
  y_start = (3 * side_length) + (2 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

## Row 4 #####
left_face_row4 <- left_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row4_tidy <- tidy_rhombus(left_face_row4, "left_row4")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

## Row 4 #####
right_face_row4 <- right_two_tiles(
  x_start = diagonal_p,
  y_start = 0)
right_face_row4_tidy <- tidy_rhombus(right_face_row4, "right_row4")

# Create data for plotting: Top faces ------------------------------------------

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_four_tiles(
  x_start = 0,
  y_start = (2 * side_length) + diagonal_q)
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

## Row 4 #####
top_face_row4 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
top_face_row4_tidy <- tidy_rhombus(top_face_row4, "top_row4")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy,
  left_face_row4_tidy) %>%
  mutate(fill = "#0E79B2")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy,
  right_face_row4_tidy) %>%
  mutate(fill = "#BF1363")

# Top faces
top_faces <- bind_rows(
  top_face_row2_tidy, top_face_row3_tidy, top_face_row4_tidy) %>%
  mutate(fill = "#F39237")

# Build plot -------------------------------------------------------------------

left_low <- "#084C61"
left_high <- "#B4E9F9"
right_low <- "#003E1F"
right_high <- "#73BA9B"
top_low <- "#EA890B"
top_high <- "#F9C98A"

p <- ggplot() +
  # Left faces
  as_reference(
    geom_polygon(
      data = left_faces, aes(x = x, y = y, group = tile_id)),
    id = "left") +
  with_mask(
    geom_raster(data = grid1, aes(x, y, fill = noise)),
    mask = ch_alpha("left")) +
  scale_fill_gradient(low = left_low, high = left_high) +
  # Right faces
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_colour() +
  as_reference(
    geom_polygon(
      data = right_faces, aes(x = x, y = y, group = tile_id)),
    id = "right") +
  with_mask(
    geom_raster(data = grid2, aes(x, y, fill = noise)),
    mask = ch_alpha("right")) +
  scale_fill_gradient(low = right_low, high = right_high) +
  # Top faces
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_colour() +
  as_reference(
    geom_polygon(
      data = top_faces, aes(x = x, y = y, group = tile_id)),
    id = "top") +
  with_mask(
    geom_raster(data = grid3, aes(x, y, fill = noise)),
    mask = ch_alpha("top")) +
  scale_fill_gradient(low = top_low, high = top_high) +
  coord_cartesian(
    xlim = c(diagonal_p, (3 * diagonal_p)),
    ylim = c((side_length + (diagonal_q / 2)), ((3 * side_length) + (1.5 * diagonal_q)))) +
  theme_void() +
  theme(legend.position = "none")

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut04.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Offcut 05 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_two_tiles(
  x_start = diagonal_p,
  y_start = (3 * side_length) + (2 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

## Row 4 #####
left_face_row4 <- left_two_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row4_tidy <- tidy_rhombus(left_face_row4, "left_row4")

# Create data for plotting: Top faces ------------------------------------------

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_four_tiles(
  x_start = 0,
  y_start = (2 * side_length) + diagonal_q)
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

## Row 4 #####
top_face_row4 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = side_length + (diagonal_q / 2))
top_face_row4_tidy <- tidy_rhombus(top_face_row4, "top_row4")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy,
  left_face_row4_tidy) %>%
  mutate(fill = "#8FB339")

# Top faces
top_faces <- bind_rows(
  top_face_row2_tidy, top_face_row3_tidy, top_face_row4_tidy) %>%
  mutate(fill = "#B7CE63")

# Build plot -------------------------------------------------------------------

outline_colour <- "#FBFEF9"
bg_colour <- "#4B5842"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut05.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Offcut 06 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = side_length + (diagonal_q / 2))
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (2 * diagonal_p),
  y_start = 0)
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

# Create data for plotting: Top faces ------------------------------------------

## Row 1 #####
top_face_row1 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row1_tidy <- tidy_rhombus(top_face_row1, "top_row1")

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = side_length + (diagonal_q / 2))
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy) %>%
  mutate(fill = "#8CC084")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy) %>%
  mutate(fill = "#FFCAB1")

# Top faces
top_faces <- bind_rows(
  top_face_row1_tidy, top_face_row2_tidy, top_face_row3_tidy) %>%
  mutate(fill = "#ECDCB0")

# Build plot -------------------------------------------------------------------

outline_colour <- "#C1D7AE"
bg_colour <- "#F6F5F4"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Right faces
  geom_polygon(
    data = right_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut06.png"),
  last_plot(), width = 10, height = 8, units = "cm", dpi = 600)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Offcut 07 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = side_length + (diagonal_q / 2))
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (2 * diagonal_p),
  y_start = 0)
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

# Create data for plotting: Top faces ------------------------------------------

## Row 1 #####
top_face_row1 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row1_tidy <- tidy_rhombus(top_face_row1, "top_row1")

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = side_length + (diagonal_q / 2))
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy) %>%
  mutate(fill = "#D78521")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy) %>%
  mutate(fill = "#DE1A1A")

# Top faces
top_faces <- bind_rows(
  top_face_row1_tidy, top_face_row2_tidy, top_face_row3_tidy) %>%
  mutate(fill = "#ACBED8")

# Build plot -------------------------------------------------------------------

outline_colour <- "#F2D398"
bg_colour <- "#E8EBF7"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Right faces
  geom_polygon(
    data = right_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut07.png"),
  last_plot(), width = 10, height = 8, units = "cm", dpi = 600)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Offcut 08 --------------------------------------------------------------------

# Create data for plotting: Left faces -----------------------------------------

## Row 1 #####
left_face_row1 <- left_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (2 * side_length) + (1.5 * diagonal_q))
left_face_row1_tidy <- tidy_rhombus(left_face_row1, "left_row1")

## Row 2 #####
left_face_row2 <- left_three_tiles(
  x_start = diagonal_p,
  y_start = side_length + diagonal_q)
left_face_row2_tidy <- tidy_rhombus(left_face_row2, "left_row2")

## Row 3 #####
left_face_row3 <- left_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = (diagonal_q / 2))
left_face_row3_tidy <- tidy_rhombus(left_face_row3, "left_row3")

# Create data for plotting: Right faces ----------------------------------------

## Row 1 #####
right_face_row1 <- right_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
right_face_row1_tidy <- tidy_rhombus(right_face_row1, "right_row1")

## Row 2 #####
right_face_row2 <- right_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = side_length + (diagonal_q / 2))
right_face_row2_tidy <- tidy_rhombus(right_face_row2, "right_row2")

## Row 3 #####
right_face_row3 <- right_three_tiles(
  x_start = (2 * diagonal_p),
  y_start = 0)
right_face_row3_tidy <- tidy_rhombus(right_face_row3, "right_row3")

# Create data for plotting: Top faces ------------------------------------------

## Row 1 #####
top_face_row1 <- top_three_tiles(
  x_start = (diagonal_p / 2),
  y_start = (3 * side_length) + (1.5 * diagonal_q))
top_face_row1_tidy <- tidy_rhombus(top_face_row1, "top_row1")

## Row 2 #####
top_face_row2 <- top_three_tiles(
  x_start = diagonal_p,
  y_start = (2 * side_length) + diagonal_q)
top_face_row2_tidy <- tidy_rhombus(top_face_row2, "top_row2")

## Row 3 #####
top_face_row3 <- top_three_tiles(
  x_start = (1.5 * diagonal_p),
  y_start = side_length + (diagonal_q / 2))
top_face_row3_tidy <- tidy_rhombus(top_face_row3, "top_row3")

# Merge stuff together ---------------------------------------------------------

# Left faces
left_faces <- bind_rows(
  left_face_row1_tidy, left_face_row2_tidy, left_face_row3_tidy) %>%
  mutate(fill = "#15616D")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy) %>%
  mutate(fill = "#001524")

# Top faces
top_faces <- bind_rows(
  top_face_row1_tidy, top_face_row2_tidy, top_face_row3_tidy) %>%
  mutate(fill = "#FF7D00")

# Build plot -------------------------------------------------------------------

outline_colour <- "#FFF7EB"
bg_colour <- "#FFECD1"

p <- ggplot() +
  # Left faces
  geom_polygon(
    data = left_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Right faces
  geom_polygon(
    data = right_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  # Top faces
  geom_polygon(
    data = top_faces,
    aes(x = x, y = y, group = tile_id, fill = fill), colour = outline_colour,
    size = 2) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220112_offcut08.png"),
  last_plot(), width = 10, height = 8, units = "cm", dpi = 600)