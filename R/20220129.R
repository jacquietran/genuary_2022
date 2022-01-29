# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(camcorder)
library(showtext)
library(ggplot2)
library(ggforce)

# Begin {camcorder} recording --------------------------------------------------

gg_record(
  dir = file.path(
    here::here("img"), "20220129_camcorder"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 8, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Define custom function -------------------------------------------------------

##############################
# Isometric perspective grid #
##############################
draw_iso_grid <- function(
  x_lower, x_upper, n_lines_angled, n_lines_vert){
  
  # Requires {dplyr} and {glue}
  
  gridlines_obtuse <- tibble(
  x = rep(c(x_lower,x_upper), n_lines_angled),
  intercept = rep(-(n_lines_angled / 2):(n_lines_angled / 2 - 1), each = 2),
  point_label = rep(c("start", "end"), n_lines_angled)) %>%
  pivot_wider(
    id_cols = intercept,
    names_from = point_label,
    names_glue = "x_{point_label}",
    values_from = x) %>%
  mutate(
    y_start = tan(150 * pi / 180)*x_start + intercept,
    y_end = tan(150 * pi / 180)*x_end + intercept) %>%
  select(x_start, y_start, x_end, y_end)

  gridlines_acute <- tibble(
    x = rep(c(x_lower,x_upper), n_lines_angled),
    intercept = rep((n_lines_angled / 2 - 1):-(n_lines_angled / 2), each = 2),
    point_label = rep(c("start", "end"), n_lines_angled)) %>%
    pivot_wider(
      id_cols = intercept,
      names_from = point_label,
      names_glue = "x_{point_label}",
      values_from = x) %>%
    mutate(
      y_start = tan(30 * pi / 180)*x_start + intercept,
      y_end = tan(30 * pi / 180)*x_end + intercept) %>%
    select(x_start, y_start, x_end, y_end)
  
  gridlines_vert <- tibble(
    line_num = rep(seq(1, n_lines_vert, 1), each = 2)) %>%
    mutate(
   x = line_num * (sqrt(3) / 2),
   line_label = rep(c("start", "end"), times = n_lines_vert))%>%
    pivot_wider(
   id_cols = line_num,
   names_from = line_label,
   names_glue = "x_{line_label}",
   values_from = x) %>%
    mutate(
   y_start = 0,
   y_end = 10) %>%
    select(x_start, y_start, x_end, y_end)
  
  # Merge into one data frame
  iso_grid <- bind_rows(
    gridlines_obtuse, gridlines_acute, gridlines_vert)
  
  return(iso_grid)
  
}

# Create data ------------------------------------------------------------------

# Grid base
iso_grid <- draw_iso_grid(
  x_lower = 0, x_upper = 10, n_lines = 40, n_lines_vert = 11)

# Polygon
n_squares <- 7
rounded_squares <- tibble(
  polygon_num = rep(seq(0, n_squares - 1, by = 1), each = 4),
  x = rep(c(
    (sqrt(3) / 2) * 6,
    (sqrt(3) / 2) * 9,
    (sqrt(3) / 2) * 6,
    (sqrt(3) / 2) * 3), times = n_squares),
  y = rep(c(5, 3.5, 2, 3.5), times = n_squares),
  y_nudge_amt = 0.5) %>%
  mutate(
    y_nudged = case_when(
      polygon_num == 0 ~ y,
      TRUE             ~ y + (polygon_num*y_nudge_amt)),
    fill_hex = case_when(
      polygon_num == 3 ~ "#C69C72",
      TRUE             ~ "#a2a392"))

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_segment(
    data = iso_grid,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
    colour = "#4E4941") +
  geom_shape(
    data = rounded_squares,
    aes(x = x, y = y_nudged, group = polygon_num, fill = fill_hex),
    radius = unit(10, "points"), alpha = 0.35) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0,10), ylim = c(0,10), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#433e3f", colour = "#433e3f"))

# Generate GIF of plot building process ----------------------------------------

gg_playback(
  name = file.path(
    here::here("img"), "20220129_camcorder", "day29_gif.gif"),
  first_image_duration = 0.25,
  last_image_duration = 15,
  frame_duration = 0.25)

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Libre Baskerville", "libre")
showtext_auto()

p <- ggplot() +
  geom_segment(
    data = iso_grid,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
    colour = "#4E4941") +
  geom_shape(
    data = rounded_squares,
    aes(x = x, y = y_nudged, group = polygon_num, fill = fill_hex),
    radius = unit(10, "points"), alpha = 0.35) +
  scale_fill_identity() +
  coord_fixed(xlim = c(0,10), ylim = c(0,10), expand = FALSE) +
  # Plot titles and theming
  labs(
    title = "concertina",
    subtitle = "#genuary2022 - Day 29",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "libre", colour = "#a2a392"),
    plot.title = element_text(
      size = rel(8.5), face = "bold", margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(4), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(4), margin = margin(10,0,0,0, unit = "pt")),
    panel.background = element_rect(fill = "#433e3f", colour = "#433e3f"),
    plot.background = element_rect(fill = "#353132", colour = "#353132"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220129.png"),
  last_plot(), width = 7.5, height = 9, units = "cm", dpi = 600)

showtext_auto(FALSE)