# Load libraries ---------------------------------------------------------------

library(imager)
library(showtext)
library(tidyverse)
library(ggpattern)
library(magick)
library(grid)

# Apply clustering approach to selfie ------------------------------------------

# Method and code courtesy of Antonio Sánchez Chinchón
# Blog post: https://fronkonstin.com/2019/08/05/clustering-frankenstein/
# Code: https://github.com/aschinchon/clustering-frankenstein/blob/master/clust_frank.R

# File path of image
source_image <- here::here("misc/20220128_193805.jpg")
# Note: A smaller version of the source image lives in this repo

# Load, convert to grayscale, filter image (to convert it to bw) and sample
load.image(source_image) %>%
  grayscale() %>%
  as.cimg() %>%
  as.data.frame() -> selfie

# This function cuts the dendrogram in x groups and removes clusters formed by just one point
clustResultx <- function(x) {
  clustCut <- tibble(cluster_id = cutree(clusters, x)) %>% bind_cols(data)
  clustCut %>% group_by(cluster_id) %>% 
    summarize(size = n()) %>% 
    filter(size > 1) %>% 
    select(cluster_id) %>% 
    inner_join(clustCut, by = "cluster_id") -> clustCut
  return(clustCut)
}

# This function adds the resulting segments of comparing two consecutive clustering results
add_segments <- function(x){
  
  df1 <- clustEvol[[x]]
  df0 <- clustEvol[[x-1]]
  
  new_points <- anti_join(df1, df0, by = "id")
  
  # If a new point is added to an existing cluster
  new_points %>% 
    inner_join(df1, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 != id.2) %>% 
    mutate(d = sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2)) %>% 
    group_by(id.1) %>% 
    arrange(d) %>% 
    slice(1) %>% 
    select(p1 = id.1, p2 = id.2) %>% 
    ungroup -> new_segments1
  
  # If a new 2-points cluster is generated
  new_points %>% anti_join(bind_rows(select(new_segments1, id = p1), 
                                     select(new_segments1, id = p2)), by = "id") %>% 
    group_by(cluster_id) %>% 
    ungroup -> unpaired_points
  
  unpaired_points %>% inner_join(unpaired_points, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 < id.2) %>% 
    select(p1 = id.1, p2 = id.2) -> new_segments2
  
  # If two existing clusters are joined
  new_points <- anti_join(df1, df0, by = c("id", "cluster_id"))
  
  new_points %>% 
    inner_join(df1, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 != id.2) %>% 
    anti_join(new_points, by = c("id.2" = "id")) %>% 
    mutate(d = sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2)) %>% 
    arrange(d) %>% 
    slice(1) %>% 
    select(p1 = id.1, p2 = id.2) %>% 
    ungroup -> new_segments3
  
  bind_rows(new_segments1, new_segments2, new_segments3)
}

# Sample size
n <- 5000

# Random sample of points from protograph
selfie %>% 
  sample_n(n, weight=(1-value)) %>% 
  select(x,y) %>% mutate(id = row_number()) -> data

# Matriz of distance between points
dist_data <- dist(data %>% select(-id), method = "euclidean")

# Hierarchical clustering
clusters <- hclust(dist_data, method = 'single')

# List with all possible clusters from maximum to minimum number of clusters
nrow(data):1 %>% 
  map(function(x) clustResultx(x)) -> clustEvol

# Segments of clusters
2:length(clustEvol) %>% 
  map(function(x) add_segments(x)) %>% 
  bind_rows() -> segments_id

# Segments in (x, y) and (xend, yend) format
segments_id %>% 
  inner_join(data, by = c("p1" = "id"), suffix = c(".1", ".2")) %>% 
  inner_join(data, by = c("p2" = "id"), suffix = c(".1", ".2")) %>% 
  select(x = x.1, y = y.1, xend = x.2, yend = y.2) -> segments

# Plot clustered selfie --------------------------------------------------------

p <- ggplot() + 
  geom_curve(
    data = segments,
    aes(x = x, y = y, xend = xend, yend = yend),
    ncp = 10, colour = "#feefdd") +
  scale_x_continuous(expand=c(.1, .1)) +
  scale_y_continuous(expand=c(.1, .1), trans = scales::reverse_trans()) +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#313e50", colour = "#313e50"))

# Export to file
ggsave(
  here::here("img/ingredients/20220128_selfie_base.png"),
  last_plot(), height = 12.5, width = 10, units = "cm", dpi = 600)

# Make blobs -------------------------------------------------------------------

# Define custom function
drop_blobs <- function(initial_seed, n_blobs){
  
  # Requires {dplyr} and {purrr}
  
  set.seed(initial_seed)
  seed_vec <- sample(1:10000, n_blobs, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    operator_vec <- c("add", "sub", rep("neu", 6))
    
    set.seed(i)
    circle <- tibble(
      seed_num = i,
      degrees = seq(0, 355, by = 5),
      radians = degrees * pi / 180,
      radius = sample(seq(1, 3, by = 0.2), 1),
      x = (radius + sample(seq(-4, 8, by = 0.5), 1)) + (radius * cos(radians)),
      y = (radius + sample(seq(-4, 8, by = 0.5), 1)) + (radius * sin(radians))) %>%
      mutate(
        operator = sample(operator_vec, n(), replace = TRUE),
        x_nudge_amount = sample(seq(0.2, 0.4, by = 0.1), n(), replace = TRUE),
        y_nudge_amount = sample(seq(0.225, 0.4, by = 0.08), n(), replace = TRUE),
        x_nudged = case_when(
          operator == "add" ~ x + x_nudge_amount,
          operator == "sub" ~ x - x_nudge_amount,
          operator == "neu" ~ x),
        y_nudged = case_when(
          operator == "add" ~ y + y_nudge_amount,
          operator == "sub" ~ y - y_nudge_amount,
          operator == "neu" ~ y))
    
  }) -> circle
  
  return(circle)
  
}

# Drop blobs
blob1 <- drop_blobs(initial_seed = 16, n_blobs = 1)
blob2 <- drop_blobs(initial_seed = 52, n_blobs = 1)
blob3 <- drop_blobs(initial_seed = 47, n_blobs = 1)
blob4 <- drop_blobs(initial_seed = 30, n_blobs = 1)

# Plot blobs -------------------------------------------------------------------

p <- ggplot() +
  geom_polygon_pattern(
    data = blob1, aes(x_nudged, y_nudged, group = seed_num), 
    fill = "#0e4a4d", colour = "#0e4a4d",
    pattern_density = 0.28, pattern_spacing = 0.01,
    pattern_fill = "#0e4a4d", pattern_colour  = "#0e4a4d") +
  geom_polygon_pattern(
    data = blob2, aes(x_nudged, y_nudged, group = seed_num), 
    fill = "#0d1f2d", colour = "#0d1f2d",
    pattern_density = 0.28, pattern_spacing = 0.01,
    pattern_fill = "#0d1f2d", pattern_colour  = "#0d1f2d") +
  geom_polygon_pattern(
    data = blob3, aes(x_nudged, y_nudged, group = seed_num), 
    fill = "#7fd14d", colour = "#7fd14d",
    pattern_density = 0.28, pattern_spacing = 0.01,
    pattern_fill = "#7fd14d", pattern_colour  = "#7fd14d") +
  geom_polygon_pattern(
    data = blob4, aes(x_nudged, y_nudged, group = seed_num), 
    fill = "#ff7b00", colour = "#ff7b00",
    pattern_density = 0.28, pattern_spacing = 0.01,
    pattern_fill = "#ff7b00", pattern_colour  = "#ff7b00") +
  coord_cartesian(xlim = c(0,10), ylim = c(0,12.5), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#202020", colour = "#202020"))

# Export to file
ggsave(
  here::here("img/ingredients/20220128_blobbies.png"),
  last_plot(), height = 12.5, width = 10, units = "cm", dpi = 600)

# Image manipulation with {magick} ---------------------------------------------

## Read in blobbies image #####
blobbies <- image_read(
  here::here("img/ingredients/20220128_blobbies.png"))

# Apply filters
blobbies_filtered <- blobbies %>%
  image_noise(noisetype = "Poisson") %>%
  image_ordered_dither(threshold_map = "h16x16o")

## Read in selfie image #####
selfie_base <- image_read(
  here::here("img/ingredients/20220128_selfie_base.png"))

# Apply filters
selfie_filtered <- selfie_base %>%
  image_morphology(method = "Dilate", kernel = "Disk", iter = 2) %>%
  image_charcoal() %>%
  image_negate() %>%
  image_morphology(method = "DilateIntensity", kernel = "Octagon", iter = 3)

# Blend images together
blobbies_dark <- image_flatten(
  c(blobbies_filtered, blobbies_filtered), "Darken")

blended <- image_flatten(
  c(blobbies_dark, selfie_filtered), "Screen")

# Export
image_write(
  blended,
  path = here::here("img/ingredients/20220128_blended_img.png"),
  format = "png")

# Shift back to {ggplot2} workflow ---------------------------------------------

# Import fonts from Google
font_add_google("Reenie Beanie", "reenie")
showtext_auto()

# Read in blended image
selfie <- image_read(
  here::here("img/ingredients/20220128_blended_img.png"))

# Convert to raster
selfie_raster <- rasterGrob(
  selfie, width = unit(1,"npc"), height = unit(1,"npc"))

# Build plot
p <- ggplot() +
  annotation_custom(selfie_raster, -Inf, Inf, -Inf, Inf) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 12.5)) +
  # Plot titles and theming
  labs(
    title = "Approximation",
    subtitle = "#genuary2022 - Day 28",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "reenie", colour = "#ECF1F6"),
    plot.title = element_text(
      size = rel(18), face = "bold", margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(10), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(10), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#202020", colour = "#202020"),
    plot.margin = margin(10, 20, 10, 20, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220128.png"),
  last_plot(), width = 10, height = 14, units = "cm", dpi = 600)

showtext_auto(FALSE)