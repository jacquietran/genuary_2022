# Load libraries ---------------------------------------------------------------

library(showtext)
library(tidyverse)
library(EnvStats)
library(zoo)
library(grid)
library(jpeg)

# Define custom functions ------------------------------------------------------

# By Will Chase:
# https://github.com/will-r-chase/aRt/blob/master/October_disintegration/dust_functions.R

#########################
### general functions ###
#########################
rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
  bounds <- pnorm(c(min, max), mean, sd)
  u <- runif(n, bounds[1], bounds[2])
  qnorm(u, mean, sd)
}

#split the edges and add inertia + diff values
splitter2 <- function(data, seed, wind_angle = 0, inertia_mean = 1, inertia_sd = 5, inertia_min = 1, inertia_max = 100) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    data %>%
      mutate(split = seed$split, inertia2 = seed$inertia2) %>%
      select(id, x, y, split, inertia2) %>%
      group_by(split) %>%
      arrange(desc(x)) %>%
      group_map( ~ mutate(., id = 1:nrow(.))) %>%
      map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
      bind_rows(.id = "group_id") %>%
      mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
      ungroup() 
  } else {
    data %>%
      mutate(split = seed$split, inertia2 = seed$inertia2) %>%
      select(id, x, y, split, inertia2) %>%
      group_by(split) %>%
      arrange(y) %>%
      group_map( ~ mutate(., id = 1:nrow(.))) %>%
      map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
      bind_rows(.id = "group_id") %>%
      mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
      ungroup() 
  }
}

#fray the edges of the shape
fray <- function(data, min_fray = 20, max_fray = 100, num_fray = 3) {
  frayer <- function(dat, start, end) { 
    dat %>%
      filter(inertia != 0) %>%
      mutate(group_id2 = group_id) %>%
      group_by(group_id2) %>%
      group_map( ~ filter(., id %in% start:end | id %in% (max(id)-start):(max(id)-end))) %>%
      bind_rows() %>%
      ungroup() %>%
      mutate(inertia2 = sample(c(0, 1), nrow(.), replace = TRUE, prob = c(1, 1)),
             inertia = ifelse(inertia2 == 0, 0, inertia))
  }
  
  fray_params <- list()
  for(i in 1:num_fray) {
    fray_params$start[[i]] <- ifelse(length(fray_params) == 0, 1, fray_params$end[[i - 1]])
    fray_params$end[[i]] <- sample(min_fray:max_fray, 1)
  }
  
  frayed <- list()
  for(i in 1:num_fray) {
    frayed[[i]] <- frayer(data, start = fray_params$start[[i]], end = fray_params$end[[i]])
  }
  
  frayed <- bind_rows(frayed)
  
  full_join(data, frayed, by = c("group_id", "id")) %>%
    mutate(inertia = ifelse(is.na(inertia.y), inertia.x, inertia.y)) %>%
    select(group_id, id, x = x.x, y = y.x, inertia, diff = diff.x)
  
}

#move points according to force, inertia, jitter, etc
gust <- function(data, angle = 0, force = 100, diff_mod = 100, inertia_mod = 50, jitter_min = 0.1, jitter_max = 1, jitter_mod = 1) {
  a <- angle * pi / 180
  perp <- ifelse((((angle >= 315 & angle <= 360) | (angle >= 0 & angle <= 45)) | (angle >= 45 & angle <= 135)), a + ((3 * pi) / 2), a + (pi / 2))
  
  data_jittered <- 
    data %>%
    mutate(jitter = rtnorm(nrow(.), mean = 0, sd = 2, min = -10, max = 15) * jitter_mod) 
  
  
  pts_new <- 
    data_jittered %>%
    mutate(x = ifelse(inertia == 0, x + cos(a)*force, (x + (cos(a) * (force * inertia))) + 
                        (cos(perp) * ((diff * diff_mod)  * (inertia * inertia_mod) * sample(seq(jitter_min, jitter_max, 0.01), 1))) +
                        (cos(perp) * jitter)),
           y = ifelse(inertia == 0, y + sin(a)*force, (y + (sin(a) * (force * inertia))) + 
                        (sin(perp) * ((diff * diff_mod) * (inertia * inertia_mod) * sample(seq(jitter_min, jitter_max, 0.01), 1))) +
                        (sin(perp) * jitter)))
  
  return(pts_new)
}

#################
### for lines ###
#################

#generate random points between endpoints for a set of edges
sand_paint <- function(edges, n_grains = 100) {
  # Function for sand painting a single edge
  sand_paint_edge <- function(edge_num) {
    a <- as.numeric(edges[edge_num, c("x", "y")])
    b <- as.numeric(edges[edge_num, c("xend", "yend")])
    data.frame(alpha = runif(n_grains)) %>%
      dplyr::mutate(x = a[1] * (1 - alpha) + b[1] * alpha,
                    y = a[2] * (1 - alpha) + b[2] * alpha) %>%
      dplyr::select(-alpha)
  }
  # Sand paint all the edges
  1:nrow(edges) %>%
    purrr::map_df(~sand_paint_edge(.x), id = "id")
}

#paint a line from endpoints w/ sandpaint
paint <- function(line, grains, wind_angle = 0) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand_paint(line, n_grains = grains) %>%
      arrange(desc(x)) %>%
      mutate(id = 1:nrow(.))
  } else {
    sand_paint(line, n_grains = grains) %>%
      arrange(y) %>%
      mutate(id = 1:nrow(.))
  }
}

#generate a seed line with splits
gen_seed <- function(seed, n_grains = 10000, wind_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand <- sand_paint(seed, n_grains = n_grains) %>%
      arrange(desc(x)) %>%
      mutate(id = 1:nrow(.))
  } else {
    sand <- sand_paint(seed, n_grains = n_grains) %>%
      arrange(y) %>%
      mutate(id = 1:nrow(.))
  }
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  sand_split %>%
    mutate(split = split2, group_split = split2) %>%
    select(id, x = x.x, y = y.x, split, group_split) %>%
    group_by(group_split) %>%
    group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
    map( ~ mutate(., id = 1:nrow(.))) %>%
    map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
    bind_rows(.id = "group_id")
}

###################
### for circles ###
###################

#draw a circle
circle <- function(points = 10000, r = 30) {
  tibble(angle = seq(0, 2*pi, length.out = points), x = r*cos(angle), y = r*sin(angle)) %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, x, y)
}

#generate a seed for circles with splits
gen_seed_cir <- function(n_grains = 10000, r = 30, wind_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand <- circle(n_grains, r) %>%
      arrange(desc(x)) %>%
      mutate(id = 1:nrow(.))
  } else {
    sand <- circle(n_grains, r) %>%
      arrange(y) %>%
      mutate(id = 1:nrow(.))
  }
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  sand_split %>%
    mutate(split = split2, group_split = split2) %>%
    select(id, x = x.x, y = y.x, split, group_split) %>%
    group_by(group_split) %>%
    group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
    map( ~ mutate(., id = 1:nrow(.))) %>%
    map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
    bind_rows(.id = "group_id")
}


##################
### for regons ###
##################

#draw a regon
regon_pts <- function(points = 10000, edges = 3, start_angle = 0, r = 10, wind_angle = 0, cx = 0, cy = 0) {
  a = start_angle * (pi / 180)
  n = floor(points / edges)
  
  corners <- tibble(angle = seq(0 + a, 2*pi + a, length.out = edges + 1), x = r*cos(angle) + cx, y = r*sin(angle) + cy)
  
  edges_list <- list()
  
  for(i in 1:edges) {
    edges_list[[i]] <- tibble(id = 1:n) %>%
      mutate(d = id / n, 
             x = (1 - d) * corners$x[i] + d * corners$x[i + 1],
             y = (1 - d) * corners$y[i] + d * corners$y[i + 1])
  }
  
  bind_rows(edges_list)
  
}

#generate a seed for regon with splits
gen_seed_regon <- function(n_grains = 10000, r = 30, edges = 3, start_angle = 0, cx = 0, cy = 0, wind_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1) {
  sand <- regon_pts(points = n_grains, r = r, edges = edges, start_angle = start_angle, cx = cx, cy = cy) %>%
    mutate(id = 1:nrow(.))
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand_split %>%
      mutate(split = split2, group_split = split2) %>%
      select(id, x = x.x, y = y.x, split, group_split) %>%
      group_by(group_split) %>%
      arrange(desc(x)) %>%
      group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
      map( ~ mutate(., id = 1:nrow(.))) %>%
      map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
      bind_rows(.id = "group_id")
  } else {
    sand_split %>%
      mutate(split = split2, group_split = split2) %>%
      select(id, x = x.x, y = y.x, split, group_split) %>%
      group_by(group_split) %>%
      arrange(y) %>%
      group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
      map( ~ mutate(., id = 1:nrow(.))) %>%
      map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
      bind_rows(.id = "group_id")
  }
}

# Create data ------------------------------------------------------------------

# Riffing on Will Chase's source code:
# https://github.com/will-r-chase/aRt/blob/master/October_disintegration/two_squares.R

regon_seed1 <- gen_seed_regon(
  n_grains = 100000, edges = 4, start_angle = 100,
  wind_angle = 0, r = 50, split_mod = 2000)

# Data for "topside" square!
regon_seed1_nudged <- regon_seed1 %>%
  mutate(y_nudged = y + 45)

regons1 <- list(a = regon_pts(100000, start_angle = 100, edges = 4, r = 50),
                b = regon_pts(100000, start_angle = 100, edges = 4, r = 50.2),
                c = regon_pts(100000, start_angle = 100, edges = 4, r = 50.4),
                d = regon_pts(100000, start_angle = 100, edges = 4, r = 50.6),
                e = regon_pts(100000, start_angle = 100, edges = 4, r = 50.8))

regons_1 <- 
  regons1 %>%
  map( ~ splitter2(., seed = regon_seed1, wind_angle = 0)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 2))

# Data for disintegrated square
square1 <- gust(
  regons_1, angle = 270, force = 3, diff_mod = 0.020, inertia_mod = 0.0008,
  jitter_min = 2, jitter_max = 5, jitter_mod = 0.1)

# Use regon_seed1_nudged to define objects for use when drawing segments
max_y <- max(regon_seed1_nudged$y_nudged)
min_y <- min(regon_seed1_nudged$y_nudged)
x_at_max_y <- regon_seed1_nudged %>%
  filter(y_nudged == max_y) %>%
  select(x) %>%
  pull()
x_at_min_y <- regon_seed1_nudged %>%
  filter(y_nudged == min_y) %>%
  select(x) %>%
  pull()

# Data for north line segment
seg_north <- tibble(
  x = x_at_max_y, xend = x_at_max_y,
  y = max_y, yend = max_y - 46)

# Data for south line segment
seg_south <- tibble(
  x = x_at_min_y, xend = x_at_min_y,
  y = min_y, yend = min_y - 46)

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("Alegreya Sans", "alegreya")
showtext_auto()

# Import paper texture image
cyano <- readJPEG(here::here("misc/cyanotype.jpg"))
cyano_raster <- rasterGrob(cyano, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot()
# Paper texture background
p <- p + annotation_custom(cyano_raster, -Inf, Inf, -Inf, Inf)
# Disintegrating square
p <- p + geom_point(
  data = square1, aes(x = x, y = y),
  alpha = 0.03, size = 0.1, color = "#f4f4f4", shape = 46)
# North line
p <- p + geom_segment(
  data = seg_north,
  aes(x = x, xend = xend, y = y, yend = yend),
  size = 0.6, color = "#f4f4f4", linetype = 3333)
# South line
p <- p + geom_segment(
  data = seg_south,
  aes(x = x, xend = xend, y = y, yend = yend),
  size = 0.6, color = "#f4f4f4", linetype = 3333)
# Topside
p <- p + geom_point(
  data = regon_seed1_nudged, aes(x = x, y = y_nudged),
  alpha = 0.03, size = 0.2, color = "#f4f4f4")
# Plot titles and theming
p <- p + labs(
  title = "Sift",
  subtitle = "#genuary2022 - Day 5",
  caption = "@jacquietran")
p <- p + theme_void()
# p <- p + coord_equal()
p <- p + theme(
  text = element_text(family = "alegreya", colour = "#183778"),
  plot.title = element_text(
    size = rel(12), margin = margin(0,0,5,0, unit = "pt")),
  plot.subtitle = element_text(
    size = rel(5), margin = margin(0,0,10,0, unit = "pt")),
  plot.caption = element_text(
    size = rel(5), margin = margin(10,0,0,0, unit = "pt")),
  plot.background = element_rect(fill = "#EEEEEE", colour = "#EEEEEE"),
  plot.margin = margin(20,20,20,20, unit = "pt"))

# Export to PNG
ggsave(
  here::here("img/20220105.png"),
  last_plot(), height = 7, width = 4, dpi = 300,
  device = "png", type = "cairo")

showtext_auto(FALSE)