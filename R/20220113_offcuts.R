# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ambient)
library(ggplot2)
library(gganimate)

# Define custom function -------------------------------------------------------

# Generate noise for plot background
make_noise <- function(seed_vec){
  
  # Requires {purrr}, {dplyr}
  
  purrr::map_df(seed_vec, function(i) {
    
    set.seed(i)
    long_grid(
      x = seq(0, 80, length.out = 100),
      y = seq(0, 8,  length.out = 100)) %>%
      mutate(
        noise = gen_value(x = x, y = y, frequency = 0.1, seed = i),
        seed_num = i)
    
  }) -> noise
  
  return(noise)
  
}

# Create data ------------------------------------------------------------------

# Define seeds
initial_seed = 981607
seed_vec <- sample(1:10000, 100, replace = FALSE)

# Generate noise for plot background
noise_complaint <- make_noise(seed_vec)

# Offcut 1 ---------------------------------------------------------------------

# Build plot
colour_vec <- c("#494368", "#7A28CB", "#FC60A8", "#F4B393", "#CEEC97")

p <- ggplot() +
  geom_raster(
    data = subset(
      noise_complaint,
      seed_num == unique(noise_complaint$seed_num)[13]),
    aes(x, y, fill = noise, group = seed_num)) +
  scale_fill_gradientn(
    colours = colour_vec) +
  theme_void() +
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank(),
    plot.margin      = margin(-1, -2, -1, -2, "cm"),
    panel.spacing    = margin(-1, -1, -1, -1, "cm"),
    legend.position = "none")

# Export to file
ggsave(
  here::here("img/offcuts/20220113_offcut01.png"),
  last_plot(),
  width = 6, height = 6, units = "cm")

# Offcut 2 ---------------------------------------------------------------------

# Build plot
colour_vec <- c("#494368", "#7A28CB", "#FC60A8", "#F4B393", "#CEEC97")

p <- ggplot() +
  geom_raster(
    data = subset(
      noise_complaint,
      seed_num == unique(noise_complaint$seed_num)[14]),
    aes(x, y, fill = noise, group = seed_num)) +
  scale_fill_gradientn(
    colours = colour_vec) +
  theme_void() +
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank(),
    plot.margin      = margin(-1, -2, -1, -2, "cm"),
    panel.spacing    = margin(-1, -1, -1, -1, "cm"),
    legend.position = "none")

# Export to file
ggsave(
  here::here("img/offcuts/20220113_offcut02.png"),
  last_plot(),
  width = 6, height = 6, units = "cm")

# Offcut 3 ---------------------------------------------------------------------

# Build plot
colour_vec <- c("#B8336A", "#C490D1", "#ACACDE", "#ABDAFC", "#E5FCFF")

p <- ggplot() +
  geom_raster(
    data = subset(
      noise_complaint,
      seed_num == unique(noise_complaint$seed_num)[16]),
    aes(x, y, fill = noise, group = seed_num)) +
  scale_fill_gradientn(
    colours = colour_vec) +
  theme_void() +
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank(),
    plot.margin      = margin(-1, -2, -1, -2, "cm"),
    panel.spacing    = margin(-1, -1, -1, -1, "cm"),
    legend.position = "none")

# Export to file
ggsave(
  here::here("img/offcuts/20220113_offcut03.png"),
  last_plot(),
  width = 6, height = 6, units = "cm")

# Offcut 4 ---------------------------------------------------------------------

# Build plot
colour_vec <- c("#9381FF", "#B8B8FF", "#FFD8BE", "#FFEEDD", "#F8F7FF")

p <- ggplot() +
  geom_raster(
    data = subset(
      noise_complaint,
      seed_num == unique(noise_complaint$seed_num)[17]),
    aes(x, y, fill = noise, group = seed_num)) +
  scale_fill_gradientn(
    colours = colour_vec) +
  theme_void() +
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank(),
    plot.margin      = margin(-1, -2, -1, -2, "cm"),
    panel.spacing    = margin(-1, -1, -1, -1, "cm"),
    legend.position = "none")

# Export to file
ggsave(
  here::here("img/offcuts/20220113_offcut04.png"),
  last_plot(),
  width = 6, height = 6, units = "cm")

# Offcut 5 ---------------------------------------------------------------------

# Build plot
colour_vec <- c("#392F5A", "#9DD9D2", "#FF8811", "#F4D06F", "#FFF8F0")

p <- ggplot() +
  geom_raster(
    data = subset(
      noise_complaint,
      seed_num == unique(noise_complaint$seed_num)[18]),
    aes(x, y, fill = noise, group = seed_num)) +
  scale_fill_gradientn(
    colours = colour_vec) +
  theme_void() +
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank(),
    plot.margin      = margin(-1, -2, -1, -2, "cm"),
    panel.spacing    = margin(-1, -1, -1, -1, "cm"),
    legend.position = "none")

# Export to file
ggsave(
  here::here("img/offcuts/20220113_offcut05.png"),
  last_plot(),
  width = 6, height = 6, units = "cm")