# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ambient)
library(ggfx)
library(showtext)
library(ggplot2)

# Define custom functions ------------------------------------------------------

################################
# Grow tree trunk and branches #
################################
grow_minimal_tree <- function(seed_num, x_value){
  
  set.seed(seed_num)
  trunk <- tibble(
    x = x_value, y = 0,
    xend = x_value, yend = sample(seq(7, 12, by = 0.1), 1),
    tree_part = "trunk")
  
  set.seed(seed_num)
  branch_left <- tibble(
    x = x_value,
    y = trunk$yend - sample(seq(0.1, 0.3, by = 0.025), 1)*trunk$yend) %>%
    mutate(
      xend = x - sample(seq(0.25, 1, by = 0.025), 1),
      yend = y + sample(seq(0.25, 1, by = 0.015), 1),
      tree_part = "branch_left")
  
  set.seed(seed_num)
  branch_right <- tibble(
    x = x_value,
    y = trunk$yend - sample(seq(0.2, 0.4, by = 0.025), 1)*trunk$yend) %>%
    mutate(
      xend = x + sample(seq(0.25, 1, by = 0.02), 1),
      yend = y + sample(seq(0.25, 1, by = 0.01), 1),
      tree_part = "branch_right")
  
  set.seed(seed_num)
  tree <- bind_rows(
    trunk, branch_left, branch_right) %>%
    mutate(
      segment_size = sample(seq(0.1, 0.6, by = 0.05), 1),
      segment_linetype = sample(
        c("dashed", "dotted", "dotdash", "longdash"), 1))
  
  return(tree)
  
}

##################
# Make the crown #
##################
make_crown <- function(seed_num, tree_data){
  
  crown_colours <- c(
    "#dd977d", "#afd371", "#d2bd6a", "#0b4e9a", "#d00088")
  
  crown_outlines <- c(
    "#3A5077", "#7DC3DD", "#7f3cf2", "#6A7FD2", "#f4a000", "#88D000")
  
  set.seed(seed_num)
  crown <- tree_data %>%
    filter(tree_part == "trunk") %>%
    mutate(y = yend - 0.2*yend) %>%
    select(x = xend, y) %>%
    mutate(
      point_size = sample(seq(60, 90, by = 5), 1),
      point_shape = sample(c(21:24), 1),
      point_colour = sample(crown_colours, 1),
      point_outline_size = point_size +
        sample(seq(0.05, 0.15, by = 0.05), 1)*point_size,
      point_outline_colour = sample(crown_outlines, 1))
  
  return(crown)
  
}

# Create data ------------------------------------------------------------------

# Tree 1
seed_num <- 283
tree1 <- grow_minimal_tree(seed_num, x_value = 4)
crown1 <- make_crown(seed_num, tree1)

# Tree 2
seed_num <- 23944
tree2 <- grow_minimal_tree(seed_num, x_value = 7.5)
crown2 <- make_crown(seed_num, tree2)

# Tree 3
seed_num <- 4561
tree3 <- grow_minimal_tree(seed_num, x_value = 11)
crown3 <- make_crown(seed_num, tree3)

# Tree 4
seed_num <- 78493
tree4 <- grow_minimal_tree(seed_num, x_value = 16)
crown4 <- make_crown(seed_num, tree4)

# Merge trees
trees_merged <- bind_rows(
  tree1, tree2, tree3, tree4, .id = "id") %>%
  mutate(grouping_var = glue::glue("{id}_{tree_part}"))

# Merge crowns
crowns_merged <- bind_rows(
  crown1, crown2, crown3, crown4, .id = "id")

## Generate noisy background #####
set.seed(1068)
perlin_noise <- long_grid(
  x = seq(0, 20, length.out = 1000),
  y = seq(0, 15, length.out = 1000)) %>%
  mutate(
    noise = fracture(
      gen_perlin, fbm, octaves = 8, frequency = 60, x = x, y = y))

# Build plot -------------------------------------------------------------------

# Set custom colour gradient for noisy background
selected_colours <- c("#a2a9af", "#99abc3", "#d6e4f7")
noise_gradient <- (grDevices::colorRampPalette(selected_colours))(50)

p <- ggplot() +
  # Noisy background base
  geom_raster(data = perlin_noise, aes(x, y, fill = noise)) +
  scale_fill_gradientn(colours = noise_gradient) +
  # Crowns
  ggnewscale::new_scale_fill() +
  geom_point(
    data = crowns_merged,
    aes(x = x, y = y, group = id,
        size = point_size, shape = point_shape, fill = point_colour),
    stroke = 0, alpha = 0.6) +
  # Crown outlines
  geom_point(
    data = crowns_merged,
    aes(x = x, y = y, group = id,
        size = point_outline_size, shape = point_shape,
        colour = point_outline_colour)) +
  # Trees
  geom_segment(
    data = trees_merged,
    aes(x = x, y = y, xend = xend, yend = yend, group = grouping_var,
        size = segment_size, linetype = segment_linetype)) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  scale_shape_identity() +
  scale_linetype_identity() +
  coord_cartesian(xlim = c(0,20), ylim = c(0,15), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")
  
ggsave(
    here::here("img/offcuts/20220123_offcuts03.png"),
    last_plot(), width = 20, height = 15, units = "cm", dpi = 600)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("", "")
showtext_auto()

p <- ggplot() +
  # Plot titles and theming
  labs(
    title = "",
    subtitle = "#genuary2022 - Day XX",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "", colour = "#"),
    plot.title = element_text(
      size = rel(14), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#", colour = "#"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220123.png"),
  last_plot(), width = 12, height = 8, units = "cm", dpi = 600)

showtext_auto(FALSE)