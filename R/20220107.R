# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(showtext)
library(ggplot2)
library(patchwork)

# Define custom functions ------------------------------------------------------

################################
# Generate 50 coordinate pairs #
################################

sol_create_dots <- function(seed_num) {
  
  # Requires {dplyr}
  
  # Base frame  
  set.seed(seed_num)
  dots <- tibble(
    x = sample(seq(0, 10, by = 0.01), 50, replace = FALSE),
    y = sample(seq(0, 10, by = 0.01), 50, replace = FALSE))
  
  return(dots)
  
}

#####################################
# Generate dots jittered base frame #
#####################################

sol_jitter_dots <- function(seed_vec){
  
  # Requires {purrr}, {dplyr}
  
  purrr::map_df(seed_vec, function(i) {
    
    set.seed(i)
    dots_nudged <- dots %>%
      mutate(
        run_id = as.factor(i),
        operator_for_x = sample(c("add", "subtract"), n(), replace = TRUE),
        operator_for_y = sample(c("add", "subtract"), n(), replace = TRUE),
        modifier_for_x = sample(seq(0, 0.3, by = 0.07), n(), replace = TRUE),
        modifier_for_y = sample(seq(0, 0.3, by = 0.12), n(), replace = TRUE),
        x_nudged = case_when(
          operator_for_x == "add"      ~ x + modifier_for_x,
          operator_for_x == "subtract" ~ x - modifier_for_x),
        y_nudged = case_when(
          operator_for_y == "add"      ~ y + modifier_for_y,
          operator_for_y == "subtract" ~ y - modifier_for_y)) %>%
      select(x, y, x_nudged, y_nudged, run_id)
    
  }) -> dots_jittered
  
  return(dots_jittered)
  
}

####################################
# Plot dots and delaunay triangles #
####################################

build_delaunay_plot <- function(dots, dots_jittered, bg_colour, line_colour){
  
  # Requires {ggplot2}, {ggfx}, {ggforce}
  
  ggplot() +
    ggfx::with_blur(
      geom_point(
        data = dots, aes(x = x, y = y),
        size = 12, alpha = 0.3, shape = 16, stroke = 0, colour = line_colour),
      sigma = 5) +
    geom_delaunay_segment2(
      data = dots_jittered,
      aes(x = x_nudged, y = y_nudged, group = run_id),
      size = 0.2, alpha = 0.1, colour = line_colour,
      linetype = "dotted", lineend = "round") +
    geom_delaunay_segment2(
      data = dots, aes(x = x, y = y),
      size = 2, alpha = 0.8, colour = line_colour, lineend = "round") +
    scale_x_continuous(limits = c(-2,12)) +
    scale_y_continuous(limits = c(-2,12)) +
    theme_void() +
    coord_fixed() +
    theme(
      panel.background = element_rect(fill = bg_colour, colour = bg_colour))
  
}

# Plot 1 -----------------------------------------------------------------------

# Create dots
seed_num <- 5050 # Define new seeds for different outcomes
dots <- sol_create_dots(seed_num)

# Create jittered dots
set.seed(seed_num)
seed_vec <- sample(1:8000, 50, replace = FALSE)
dots_jittered <- sol_jitter_dots(seed_vec)

# Set plot colours
# grey panel
bg_colour <- "#b7b8b3"
line_colour <- "#e1e2dd"

# Build plot
p1 <- build_delaunay_plot(dots, dots_jittered, bg_colour, line_colour)

# Plot 2 -----------------------------------------------------------------------

# Define seeds
seed_num <- 2525 # Define new seeds for different outcomes
dots <- sol_create_dots(seed_num)

# Create jittered dots
set.seed(seed_num)
seed_vec <- sample(1:8000, 50, replace = FALSE)
dots_jittered <- sol_jitter_dots(seed_vec)

# Set plot colours
# yellow panel
bg_colour <- "#fff856"
line_colour <- "#fffba4"

# Build plot
p2 <- build_delaunay_plot(dots, dots_jittered, bg_colour, line_colour)

# Plot 3 -----------------------------------------------------------------------

# Define seeds
seed_num <- 7894 # Define new seeds for different outcomes
dots <- sol_create_dots(seed_num)

# Create jittered dots
set.seed(seed_num)
seed_vec <- sample(1:8000, 50, replace = FALSE)
dots_jittered <- sol_jitter_dots(seed_vec)

# Set plot colours
# pink panel
bg_colour <- "#fd2eb8"
line_colour <- "#fc86d2"

# Build plot
p3 <- build_delaunay_plot(dots, dots_jittered, bg_colour, line_colour)

# Plot 4 -----------------------------------------------------------------------

# Define seeds
seed_num <- 38124 # Define new seeds for different outcomes
dots <- sol_create_dots(seed_num)

# Create jittered dots
set.seed(seed_num)
seed_vec <- sample(1:8000, 50, replace = FALSE)
dots_jittered <- sol_jitter_dots(seed_vec)

# Set plot colours
# cyan panel
bg_colour <- "#34edee"
line_colour <- "#aeedee"

# Build plot
p4 <- build_delaunay_plot(dots, dots_jittered, bg_colour, line_colour)

# Patch plots together ---------------------------------------------------------

# Import fonts from Google
font_add_google("Asap Condensed", "asap")
showtext_auto()

quilt <- (p1 + p2 + p3 + p4) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Sol Delaunay",
    subtitle = "#genuary2022 - Day 7",
    caption = "@jacquietran",
    theme = theme(
      text = element_text(family = "asap"),
      plot.title = element_text(
        size = rel(12), margin = margin(0,0,10,0, unit = "pt")),
      plot.subtitle = element_text(
        size = rel(8), margin = margin(0,0,15,0, unit = "pt")),
      plot.caption = element_text(
        size = rel(8), margin = margin(15,0,0,0, unit = "pt")),
      plot.margin = margin(30, 30, 30, 30, unit = "pt"),
      panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF")
      )
    )

# Export to file ---------------------------------------------------------------

ggsave(
  here::here("img/20220107.png"),
  quilt, width = 20, height = 24, units = "cm", dpi = 300)

showtext_auto(FALSE)