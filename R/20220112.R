# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(camcorder)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220112_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Set rhombus parameters -------------------------------------------------------

side_length <- 3
angle_A <- 60 * (pi / 180) # acute angle, in radians
# angle_B <- 120 * (pi / 180) # obtuse angle, in radians
rhom_h <- side_length * (sin(angle_A))
diagonal_p <- side_length * sqrt(2 + (2*cos(angle_A)))
diagonal_q <- side_length * sqrt(2 - (2*cos(angle_A)))

# Define custom functions ------------------------------------------------------

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
  mutate(fill = "#F52F57")

# Right faces
right_faces <- bind_rows(
  right_face_row1_tidy, right_face_row2_tidy, right_face_row3_tidy,
  right_face_row4_tidy) %>%
  mutate(fill = "#A20021")

# Top faces
top_faces <- bind_rows(
  top_face_row2_tidy, top_face_row3_tidy, top_face_row4_tidy) %>%
  mutate(fill = "#F79D5C")

# Build plot -------------------------------------------------------------------

outline_colour <- "#F3752B"
bg_colour <- "#FDF1E9"

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

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220112_camcorder", "day12_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Space Mono", "space")
showtext_auto()

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
  # Plot titles and theming
  labs(
    title = "Rhombille",
    subtitle = "#genuary2022 - Day 12",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "space", colour = "#4c4c50"),
    plot.title = element_text(
      size = rel(11), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(5.5), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(5.5), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220112.png"),
  last_plot(), width = 8, height = 10, units = "cm", dpi = 600)

showtext_auto(FALSE)