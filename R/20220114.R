# Load libraries ---------------------------------------------------------------

library(dplyr)
library(rayshader)
library(raster)
library(showtext)
library(ggplot2)
library(grid)
library(png)

# Import data ------------------------------------------------------------------

# Data downloaded from Elvis: https://elevation.fsdf.org.au/
melb <- raster(here::here("misc/vmelev_dem10m.tif"))

melb_mat <- raster_to_matrix(melb)
melb_water <- melb_mat
melb_water[melb_water < 0] = 0

# For prototyping
melb_small <- resize_matrix(melb_mat, 0.05)
melb_small_water <- melb_small
melb_small_water[melb_small_water < 0] = 0

# Resized
melb_resized <- resize_matrix(melb_mat, 0.15)
melb_resized_water <- melb_resized
melb_resized_water[melb_resized_water < 0] = 0

# Make map ---------------------------------------------------------------------

# Prototype
melb_small %>% 
  sphere_shade(texture = "bw") %>%
  add_shadow(ambient_shade(melb_small), 0) %>%
  add_shadow(
    texture_shade(
      melb_small_water, detail = 8/10, contrast = 9, brightness = 15),
    0.1) %>%
  add_water(detect_water(melb_small_water), color = "#232323") %>%
  plot_map()

# The real deal
melb_resized %>% 
  sphere_shade(texture = "bw") %>%
  add_shadow(ambient_shade(melb_resized), 0) %>%
  add_shadow(
    texture_shade(melb_resized, detail = 8/10, contrast = 9, brightness = 15),
    0.1) %>%
  add_water(detect_water(melb_resized_water), color = "#232323") %>%
  save_png(filename = here::here("img/20220114_map_only.png"))

# Shift to {ggplot2} workflow --------------------------------------------------

# Import fonts from Google
font_add_google("Overpass", "overpass")
showtext_auto()

# Import paper texture image
map <- readPNG(here::here("img/20220114_map_only.png"))
map_raster <- rasterGrob(map, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot() +
  # Paper texture background
  annotation_custom(map_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "The Melbourne Uniform",
    subtitle = "#genuary2022 - Day 14",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "overpass", colour = "#D3D3D3"),
    plot.title = element_text(
      size = rel(9), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(4.5), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(4.5), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#232323", colour = "#232323"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220114.png"),
  last_plot(), width = 10, height = 9, units = "cm", dpi = 600)

showtext_auto(FALSE)