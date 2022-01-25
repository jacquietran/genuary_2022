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
    xend = x_value, yend = sample(seq(5, 8, by = 0.1), 1),
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
      segment_size = sample(seq(0.4, 0.8, by = 0.05), 1),
      segment_linetype = sample(
        c("dashed", "dotted", "dotdash", "longdash"), 1))
  
  return(tree)
  
}

##################
# Make the crown #
##################
make_crown <- function(seed_num, tree_data){
  
  crown_colours_set1 <- c(
    "#D1704D", "#96C544", "#0b4e9a", "#d00088", "#267D5F")
  
  crown_colours_set2 <- crown_colours_set1
  
  crown_outlines <- c(
    "#ff7b00", "#ff9500", "#ffaa00", "#ffc300")
  
  set.seed(seed_num)
  crown <- tree_data %>%
    filter(tree_part == "trunk") %>%
    mutate(y = yend - 0.2*yend) %>%
    select(x = xend, y) %>%
    mutate(
      point_size = sample(seq(60, 90, by = 5), 1),
      point_shape = sample(c(21:24), 1),
      point_colour1 = sample(crown_colours_set1, 1),
      point_colour2 = sample(crown_colours_set2, 1),
      point_outline_size = point_size +
        sample(seq(0.05, 0.15, by = 0.05), 1)*point_size,
      point_outline_colour = sample(crown_outlines, 1))
  
  return(crown)
  
}

# Create data ------------------------------------------------------------------

# Grow tree and crown
seed_num <- 283
tree1 <- grow_minimal_tree(seed_num, x_value = 5)
crown1 <- make_crown(seed_num, tree1)

# Grow tree and crown
seed_num <- 145
tree2 <- grow_minimal_tree(seed_num, x_value = 10)
crown2 <- make_crown(seed_num, tree2)

## Generate noisy background #####
set.seed(1068)
perlin_noise <- long_grid(
  x = seq(0, 15, length.out = 1000),
  y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    noise = fracture(
      gen_perlin, fbm, octaves = 8, frequency = 60, x = x, y = y))

# Build plot -------------------------------------------------------------------

# Set custom colour gradient for noisy background
selected_colours <- c("#a2a9af", "#99abc3", "#d6e4f7")
noise_gradient <- (grDevices::colorRampPalette(selected_colours))(50)

# Import fonts from Google
font_add_google("Fredoka One", "fredoka")
showtext_auto()

p <- ggplot() +
  # Noisy background base
  geom_raster(data = perlin_noise, aes(x, y, fill = noise)) +
  scale_fill_gradientn(colours = noise_gradient) +
  # Crown 1
  ggnewscale::new_scale_fill() +
  geom_point(
    data = crown1,
    aes(x = x, y = y, size = point_size, shape = point_shape,
        fill = point_colour1),
    stroke = 0, alpha = 0.4) +
  geom_jitter(
    data = crown1,
    aes(x = x, y = y, size = point_size, shape = point_shape,
        fill = point_colour2),
    stroke = 0, alpha = 0.4) +
  # Crown outline 1
  geom_point(
    data = crown1,
    aes(x = x, y = y, size = point_outline_size, shape = point_shape,
        colour = point_outline_colour), alpha = 0.6) +
  # Tree 1
  geom_segment(
    data = tree1,
    aes(x = x, y = y, xend = xend, yend = yend, size = segment_size,
        linetype = segment_linetype)) +
  # Crown 2
  geom_point(
    data = crown2,
    aes(x = x, y = y, size = point_size, shape = point_shape,
        fill = point_colour1),
    stroke = 0, alpha = 0.4) +
  geom_jitter(
    data = crown2,
    aes(x = x, y = y, size = point_size, shape = point_shape,
        fill = point_colour2),
    stroke = 0, alpha = 0.4) +
  # Crown outline 2
  geom_point(
    data = crown2,
    aes(x = x, y = y, size = point_outline_size, shape = point_shape,
        colour = point_outline_colour), alpha = 0.6) +
  # Tree 2
  geom_segment(
    data = tree2,
    aes(x = x, y = y, xend = xend, yend = yend, size = segment_size,
        linetype = segment_linetype)) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  scale_shape_identity() +
  scale_linetype_identity() +
  coord_cartesian(xlim = c(0,15), ylim = c(0,10), expand = FALSE) +
  # Plot titles and theming
  labs(
    title = "Regrowth",
    subtitle = "#genuary2022 - Day 23",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "fredoka", colour = "#437763"),
    plot.title = element_text(
      size = rel(18), margin = margin(5,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(7.2), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(7.2), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#f9dcc4", colour = "#f9dcc4"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/20220123.png"),
  last_plot(), width = 15, height = 12, units = "cm", dpi = 600)

showtext_auto(FALSE)