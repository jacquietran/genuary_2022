# Load libraries ---------------------------------------------------------------

library(Rcpp)
library(dplyr)
library(purrr)
library(showtext)
library(ggplot2)
library(magick)
library(grid)

# Define custom function -------------------------------------------------------

# Define C++ function
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

# Attractor 01 -----------------------------------------------------------------

# Prepare data
a <- 1.9
b <- 1.8
c <- -0.7
d <- 1.3

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#2E294E", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#F1E9DA", colour = "#F1E9DA"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_01.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 02 -----------------------------------------------------------------

# Prepare data
a <- 2.3
b <- 1.8
c <- -0.7
d <- 1.3

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#541388", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#F1E9DA", colour = "#F1E9DA"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_02.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 03 -----------------------------------------------------------------

# Prepare data
a <- 2.3
b <- 2.1
c <- -0.7
d <- 1.3

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#FFD400", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#F1E9DA", colour = "#F1E9DA"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_03.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 04 -----------------------------------------------------------------

# Prepare data
a <- 2.3
b <- 2.1
c <- -0.5
d <- 1.3

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#D90368", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#F1E9DA", colour = "#F1E9DA"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_04.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 05 -----------------------------------------------------------------

# Prepare data
a <- 2.3
b <- 2.1
c <- -0.5
d <- 1.7

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#541388", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#2E294E", colour = "#2E294E"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_05.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 06 -----------------------------------------------------------------

# Prepare data
a <- 2.3
b <- 2.1
c <- -0.2
d <- 1.8

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#F1E9DA", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#2E294E", colour = "#2E294E"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_06.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 07 -----------------------------------------------------------------

# Prepare data
a <- 1.7
b <- 2.2
c <- -0.2
d <- 1.8

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#FFD400", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#2E294E", colour = "#2E294E"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_07.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Attractor 08 -----------------------------------------------------------------

# Prepare data
a <- 1.7
b <- 2.4
c <- -0.6
d <- 1.3

df <- createTrajectory(4000000, 0, 0, a, b, c, d)

# Build plot
p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    color = "#D90368", shape = 46, alpha = 0.01) +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    plot.background  = element_rect(fill = "#2E294E", colour = "#2E294E"),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank())

# Export to file
ggsave(
  here::here("img/ingredients/20220127_08.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

att01 <- image_read(here::here("img/ingredients/20220127_01.png"))
att02 <- image_read(here::here("img/ingredients/20220127_02.png"))
att03 <- image_read(here::here("img/ingredients/20220127_03.png"))
att04 <- image_read(here::here("img/ingredients/20220127_04.png"))
att05 <- image_read(here::here("img/ingredients/20220127_05.png"))
att06 <- image_read(here::here("img/ingredients/20220127_06.png"))
att07 <- image_read(here::here("img/ingredients/20220127_07.png"))
att08 <- image_read(here::here("img/ingredients/20220127_08.png"))

row1 <- image_append(c(att01, att06, att04, att07))
row2 <- image_append(c(att08, att02, att05, att03))

rows_stacked <- image_append(c(row1, row2), stack = TRUE) %>%
  # Resize
  image_scale("5000")

# Export to file
image_write(
  rows_stacked,
  path = here::here("img/ingredients/20220127_checkerboard.png"),
  format = "png")

# Shift to {ggplot2} workflow --------------------------------------------------

# Import fonts from Google
font_add_google("DM Serif Display", "dmserif")
showtext_auto()

# Read in image of attractor checkerboard
checkerboard <- image_read(
  here::here("img/ingredients/20220127_checkerboard.png"))

# Convert to raster
checkerboard_raster <- rasterGrob(
  checkerboard, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot() +
  # Checkerboard of Clifford attractors
  annotation_custom(checkerboard_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "Permutation",
    subtitle = "#genuary2022 - Day 27",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "dmserif", colour = "#2E294E"),
    plot.title = element_text(
      size = rel(11), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(6), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(6), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#F9F6F0", colour = "#F9F6F0"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220127.png"),
  last_plot(), width = 12, height = 8.5, units = "cm", dpi = 600)

showtext_auto(FALSE)