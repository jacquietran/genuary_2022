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

# Build plot -------------------------------------------------------------------

colour_vec <- c("#212738", "#F97068", "#D1D646", "#57C4E5", "#48A9A6",
                "#136F63", "#EDF2EF")

p <- ggplot() +
  geom_raster(
    data = noise_complaint, aes(x, y, fill = noise, group = seed_num)) +
  scale_fill_gradientn(
    colours = colour_vec) +
  theme_void() +
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank(),
    plot.margin      = margin(-1, -2, -1, -2, "cm"),
    panel.spacing    = margin(-1, -1, -1, -1, "cm"),
    legend.position = "none") +
  transition_states(seed_num) +
  ease_aes("linear")

animate(p, duration = 5, width = 800, height = 80, units = "px", res = 300)

# Export to GIF ----------------------------------------------------------------

anim_save(here::here("img/20220113.gif"), last_animation())

# Export static PNG ------------------------------------------------------------

p <- ggplot() +
  geom_raster(
    data = subset(
      noise_complaint,
      seed_num == unique(noise_complaint$seed_num)[6]),
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

ggsave(
  here::here("img/ingredients/20220113.png"),
  last_plot(),
  width = 6, height = 6, units = "cm")