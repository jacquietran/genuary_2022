# Load libraries ---------------------------------------------------------------

library(dplyr)
library(camcorder)
library(showtext)
library(ggplot2)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220124_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Define custom functions ------------------------------------------------------

prng <- function(seed_vec, group_name){
  
  # Requires {purrr} and {dplyr}
  
  # Generate walk distances
  purrr::map_df(seed_vec, function(i) {
    
    data <- tibble(
      x_walk = sample(seq(-1.1, 1.1, by = 1), 1),
      y_walk = sample(seq(-1, 1, by = 1), 1),
      seed_num = i)
    
  }) -> walkies
  
  # Calculate xy position using walk distances
  walkies_xy <- walkies %>%
    mutate(
      x = cumsum(x_walk),
      y = cumsum(y_walk),
      grouping_var = group_name)
  
  return(walkies_xy)
  
}

go_walkies <- function(initial_seed, group_name){
  
  set.seed(initial_seed)
  seed_vec <- sort(sample(seq(1:10000), 500, replace = FALSE))
  
  walkies <- prng(seed_vec, group_name)
  
  return(walkies)
  
}

# Random walks -----------------------------------------------------------------

walkies1 <- go_walkies(583, "walkies1")
walkies2 <- go_walkies(127, "walkies2")
walkies3 <- go_walkies(334, "walkies3")
walkies4 <- go_walkies(958, "walkies4")
walkies5 <- go_walkies(201, "walkies5")
walkies6 <- go_walkies(469, "walkies6")

## Merge bg walkies into one data frame #####
walkies_merged <- bind_rows(
  walkies1, walkies2, walkies3, walkies4, walkies5) %>%
  mutate(
    path_colour = "#c9c9c9",
    path_size = 0.2,
    path_linetype = case_when(
      grouping_var %in% c(
        "walkies4", "walkies5") ~ "solid",
      TRUE                      ~ "dashed"))

# Isolate walkies start points
walkies_merged_start_points <- walkies_merged %>%
  group_by(grouping_var) %>%
  mutate(
    largest_seed = max(seed_num),
    is_largest_seed = case_when(
      seed_num == largest_seed ~ "yes",
      TRUE                     ~ "no")) %>%
  ungroup() %>%
  filter(is_largest_seed == "yes")

## Style highlighted walkies #####
walkies_highlighted <- walkies6 %>%
  mutate(
    point_colour = "#DF7100",
    path_size = 0.5,
    path_linetype = "solid")

# Isolate highlighted walkies start point
walkies_highlighted_start_point <- walkies_highlighted %>%
  mutate(
    largest_seed = max(seed_num),
    is_largest_seed = case_when(
      seed_num == largest_seed ~ "yes",
      TRUE                     ~ "no")) %>%
  filter(is_largest_seed == "yes")

# Build plot -------------------------------------------------------------------

# Define gradient colours
path_gradient <- c(
  "#DF7100", "#FFA347", "#E71D36", "#9D5C63")

p <- ggplot() +
  # Background: Walk start points
  geom_point(
    data = walkies_merged_start_points,
    aes(x = x, y = y, colour = path_colour), size = 6, alpha = 0.3,
    shape = 16, stroke = 0) +
  scale_colour_identity() +
  # Background: Walk paths
  geom_path(
    data = walkies_merged,
    aes(x, y, group = grouping_var, colour = path_colour, size = path_size,
        linetype = path_linetype), alpha = 0.8) +
  # Foreground: Walk start point
  geom_point(
    data = walkies_highlighted_start_point,
    aes(x = x, y = y, colour = point_colour), size = 12, alpha = 0.3,
    shape = 16, stroke = 0) +
  # Foreground: Walk path
  ggnewscale::new_scale_colour() +
  geom_path(
    data = walkies_highlighted,
    aes(x, y, group = grouping_var, colour = seed_num, size = path_size,
        linetype = path_linetype), lineend = "round") +
  scale_colour_gradientn(colours = rev(path_gradient)) +
  scale_size_identity() +
  scale_linetype_identity() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#212E31", colour = "#212E31"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220124_camcorder", "day24_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 15,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Oxanium", "oxanium")
showtext_auto()

p <- ggplot() +
  # Background: Walk start points
  geom_point(
    data = walkies_merged_start_points,
    aes(x = x, y = y, colour = path_colour), size = 6, alpha = 0.3,
    shape = 16, stroke = 0) +
  scale_colour_identity() +
  # Background: Walk paths
  geom_path(
    data = walkies_merged,
    aes(x, y, group = grouping_var, colour = path_colour, size = path_size,
        linetype = path_linetype), alpha = 0.8) +
  # Foreground: Walk start point
  geom_point(
    data = walkies_highlighted_start_point,
    aes(x = x, y = y, colour = point_colour), size = 12, alpha = 0.3,
    shape = 16, stroke = 0) +
  # Foreground: Walk path
  ggnewscale::new_scale_colour() +
  geom_path(
    data = walkies_highlighted,
    aes(x, y, group = grouping_var, colour = seed_num, size = path_size,
        linetype = path_linetype), lineend = "round") +
  scale_colour_gradientn(colours = rev(path_gradient)) +
  scale_size_identity() +
  scale_linetype_identity() +
  # Plot titles and theming
  labs(
    title = "Mindlessness",
    subtitle = "#genuary2022 - Day 24",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "oxanium", colour = "#c9c9c9"),
    plot.title = element_text(
      size = rel(14), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    legend.position = "none",
    panel.background= element_rect(fill = "#212e31", colour = "#212e31"),
    plot.background = element_rect(fill = "#636C6E", colour = "#636C6E"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220124.png"),
  last_plot(), width = 12, height = 10, units = "cm", dpi = 600)

showtext_auto(FALSE)