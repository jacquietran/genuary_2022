# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220120_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 600 # dpi to use when saving image
)

# Define custom function -------------------------------------------------------

###########################
# Imitate layers of print #
###########################
woodprint <- function(
  seed_vec, num_of_lines, num_of_points, plot_width, plot_height){
  
  # Requires {purrr}, {dplyr}
  
  dist_between_lines <- plot_height / num_of_lines
  
  purrr::map_df(seed_vec, function(i) {
    
    set.seed(i)
    data <- tibble(
      x = rep(
        seq(0, plot_width, length.out = num_of_points), times = num_of_lines),
      y = rep(
        seq(0 + dist_between_lines, plot_height, by = dist_between_lines),
        each = num_of_points),
      alpha_value = sample(
        c(0.2, 0.4, 0.6), (num_of_points * num_of_lines), replace = TRUE),
      point_size = sample(
        c(0.1, 0.2, 0.3), (num_of_points * num_of_lines), replace = TRUE),
      point_shape = sample(
        c(16, 17, 18), (num_of_points * num_of_lines), replace = TRUE)) %>%
      mutate(seed_num = i)
    
  }) -> data
  
  return(data)
  
}

##################################
# Imitate hand-drawn crest lines #
##################################
crest_lines <- function(y_vec, plot_width){
  
  # Requires {purrr}, {dplyr}
  
  purrr::map_df(y_vec, function(i) {
    
    data <- tibble(
      x = seq(0, plot_width, length.out = 100),
      y = i) %>%
      # Add jitter
      mutate(
        x_jitter_amount = sample(
          c(-0.05, -0.025, 0, 0.025, 0.05), n(), replace = TRUE),
        y_jitter_amount = sample(
          c(-0.05, -0.025, 0, 0.025, 0.05), n(), replace = TRUE),
        x_adj = x + x_jitter_amount,
        y_adj = y + y_jitter_amount) %>%
      # Rescale to fit within image boundaries
      mutate(
        x_adj = scales::rescale(x_adj, to = c(0, plot_width)))
    
  }) -> data
  
  return(data)
  
}

# Create data ------------------------------------------------------------------

## Ocean layers #####
initial_seed <- 1257
num_of_layers <- 22

set.seed(initial_seed)
seed_vec <- sample(1:10000, num_of_layers, replace = FALSE)

layer_colours <- tibble(
  layer_num = 1:num_of_layers,
  point_colour = c(
    "#94a3ba",
    sample(c("#000022", "#040b42", "#192981", "#215096", "#0037aa"),
           num_of_layers - 1, replace = TRUE)))

layers_numbered <- tibble(
  seed_num = seed_vec,
  layer_num = 1:num_of_layers)

set.seed(initial_seed)
ocean_layers <- woodprint(
  seed_vec, num_of_lines = 400, num_of_points = 500,
  plot_width = 12, plot_height = 8) %>%
  left_join(., layers_numbered, by = "seed_num") %>%
  mutate(
    obs_to_keep = case_when(
      layer_num == 1             ~ "yes",
      layer_num == 2 & y < 3.75  ~ "yes",
      layer_num == 3 & y < 3.9   ~ "yes",
      layer_num == 4 & y < 4.05  ~ "yes",
      layer_num == 5 & y < 4.2   ~ "yes",
      layer_num == 6 & y < 4.35  ~ "yes",
      layer_num == 7 & y < 4.5   ~ "yes",
      layer_num == 8 & y < 4.65  ~ "yes",
      layer_num == 9 & y < 4.8   ~ "yes",
      layer_num == 10 & y < 4.95 ~ "yes",
      layer_num == 11 & y < 5.1  ~ "yes",
      layer_num == 12 & y < 5.25 ~ "yes",
      layer_num == 13 & y < 5.4  ~ "yes",
      layer_num == 14 & y < 5.55 ~ "yes",
      layer_num == 15 & y < 5.8  ~ "yes",
      layer_num == 16 & y < 5.95 ~ "yes",
      layer_num == 17 & y < 6.1  ~ "yes",
      layer_num == 18 & y < 6.25 ~ "yes",
      layer_num == 19 & y < 6.4  ~ "yes",
      layer_num == 20 & y < 6.55 ~ "yes",
      layer_num == 21 & y < 6.7  ~ "yes",
      layer_num == 22            ~ "yes",
      TRUE                       ~ "no")) %>%
  filter(obs_to_keep == "yes") %>%
  select(-obs_to_keep) %>%
  # Jitter y values
  mutate(
    y_jitter_amount = sample(
      c(-0.1, -0.05, 0, 0.05, 0.1), n(), replace = TRUE),
    y_adj = y + y_jitter_amount) %>%
  filter(y_adj >= 0 & y_adj <= 8) %>%
  left_join(., layer_colours, by = "layer_num")

## Sand layers #####
initial_seed <- 307
num_of_layers <- 15

set.seed(initial_seed)
seed_vec <- sample(1:10000, num_of_layers, replace = FALSE)

layer_colours <- tibble(
  layer_num = 1:num_of_layers,
  point_colour = c(
    "#ccb89f",
    sample(c("#c4a972", "#c0964c", "#998755", "#827555", "#e5cfaa", "#e9d7c3"),
           num_of_layers - 1, replace = TRUE)))

layers_numbered <- tibble(
  seed_num = seed_vec,
  layer_num = 1:num_of_layers)

set.seed(initial_seed)
sand_layers <- woodprint(
  seed_vec, num_of_lines = 400, num_of_points = 500,
  plot_width = 12, plot_height = 8) %>%
  left_join(., layers_numbered, by = "seed_num") %>%
  filter(y <= 3) %>%
  mutate(
    obs_to_keep = case_when(
      layer_num == 1            ~ "yes",
      layer_num == 2 & y < 0.4  ~ "yes",
      layer_num == 3 & y < 0.6  ~ "yes",
      layer_num == 4 & y < 0.8  ~ "yes",
      layer_num == 5 & y < 1    ~ "yes",
      layer_num == 6 & y < 1.2  ~ "yes",
      layer_num == 7 & y < 1.4  ~ "yes",
      layer_num == 8 & y < 1.6  ~ "yes",
      layer_num == 9 & y < 1.8  ~ "yes",
      layer_num == 10 & y < 2   ~ "yes",
      layer_num == 11 & y < 2.2 ~ "yes",
      layer_num == 12 & y < 2.4 ~ "yes",
      layer_num == 13 & y < 2.6 ~ "yes",
      layer_num == 14 & y < 2.8 ~ "yes",
      layer_num == 15           ~ "yes",
      TRUE                      ~ "no")) %>%
  filter(obs_to_keep == "yes") %>%
  select(-obs_to_keep) %>%
  # Jitter y values
  mutate(
    y_jitter_amount = sample(
      seq(-0.4, 0.4, by = 0.1), n(), replace = TRUE),
    y_adj = y + y_jitter_amount) %>%
  filter(y_adj >= 0 & y_adj <= 8) %>%
  left_join(., layer_colours, by = "layer_num")

## Create crest lines #####
y_values <- sample(seq(3.5, 6, by = 0.3), 7, replace = FALSE)
set.seed(initial_seed)
crests <- crest_lines(y_vec = y_values, plot_width = 12) %>%
  group_by(y) %>%
  mutate(
    x_seg_1 = sample(seq(10, 30), 1, replace = TRUE),
    x_seg_2 = sample(seq(10, 30), 1, replace = TRUE),
    x_seg_3 = sample(seq(10, 30), 1, replace = TRUE),
    x_seg_4 = 100 - x_seg_1 - x_seg_2 - x_seg_3,
    segment_num = c(
      rep(1, x_seg_1[1]),
      rep(2, x_seg_2[1]),
      rep(3, x_seg_3[1]),
      rep(4, x_seg_4[1])),
    keep_segment = c(
      rep(sample(c("yes", "no"), 1, replace = TRUE), x_seg_1[1]),
      rep(sample(c("yes", "no"), 1, replace = TRUE), x_seg_2[1]),
      rep(sample(c("yes", "no"), 1, replace = TRUE), x_seg_3[1]),
      rep(sample(c("yes", "no"), 1, replace = TRUE), x_seg_4[1]))) %>%
  ungroup() %>%
  filter(keep_segment == "yes") %>%
  mutate(grouping_var = glue::glue("{y}_{segment_num}"))

white_caps <- crests %>%
  mutate(y_adj = y_adj - 0.05)

## Create debris #####
debris_pieces <- 300
debris <- tibble(
  x = sample(seq(0, 12, by = 0.2), debris_pieces, replace = TRUE),
  y = sample(seq(0, 3.5, by = 0.2), debris_pieces, replace = TRUE),
  alpha_value = sample(c(0.4, 0.5, 0.6), debris_pieces, replace = TRUE),
  point_size = sample(seq(0.8, 1.1, by = 0.1), debris_pieces, replace = TRUE),
  point_shape = sample(c(15, 17, 18), debris_pieces, replace = TRUE)) %>%
  # Jitter x and y values
  # Jitter y values to imply waves
  mutate(
    x_jitter_amount = sample(
      seq(-0.4, 0.4, by = 0.1), n(), replace = TRUE),
    y_jitter_amount = sample(
      seq(-0.4, 0.4, by = 0.1), n(), replace = TRUE),
    x_adj = x + x_jitter_amount,
    y_adj = y + y_jitter_amount) %>%
  filter(x_adj > 0.1 & x_adj < 11.9) %>%
  filter(y_adj > 0.1 & y_adj < 7.9)

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("Manrope", "manrope")
showtext_auto()

p <- ggplot() +
  geom_point(
    data = ocean_layers,
    aes(x = x, y = y_adj, group = layer_num,
        alpha = alpha_value, colour = point_colour, size = point_size,
        shape = point_shape), stroke = 0) +
  geom_point(
    data = sand_layers,
    aes(x = x, y = y_adj, group = layer_num,
        alpha = alpha_value, colour = point_colour, size = point_size,
        shape = point_shape), stroke = 0) +
  geom_point(
    data = debris,
    aes(x = x_adj, y = y_adj, alpha = alpha_value, size = point_size,
        shape = point_shape), stroke = 0, colour = "#273842") +
  ggfx::with_blur(
    geom_path(
      data = white_caps,
      aes(x = x_adj, y = y_adj, group = grouping_var),
      size = 0.5, colour = "#88a4b9", alpha = 0.5),
    sigma = 20) +
  geom_path(
    data = crests,
    aes(x = x_adj, y = y_adj, group = grouping_var), size = 0.3) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_size_identity() +
  scale_shape_identity() +
  coord_cartesian(xlim = c(0,12), ylim = c(0,8)) +
  # Plot titles and theming
  labs(
    title = "Parkdale",
    subtitle = "#genuary2022 - Day 20",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "manrope", colour = "#21020a"),
    plot.title = element_text(size = rel(11), hjust = 0.065),
    plot.subtitle = element_text(size = rel(6), hjust = 0.065),
    plot.caption = element_text(size = rel(6), hjust = 0.945),
    plot.background = element_rect(fill = "#d4cdc7", colour = "#d4cdc7"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220120_camcorder", "day20_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 10,
  frame_duration = 0.25)

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220120.png"),
  last_plot(), width = 12, height = 10, units = "cm", dpi = 600)

showtext_auto(FALSE)