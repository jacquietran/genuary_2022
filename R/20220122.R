# Deal with the Java heap space issue early...before loading any packages.
# The default is ~500MB but set to use X% of system RAM
options(
  java.parameters = paste("- Xmx", round(memory.limit() * 0.8), "m", sep = ""))

# Load libraries ---------------------------------------------------------------

library(Rcpp)
library(dplyr)
library(purrr)
library(showtext)
library(ggplot2)
library(EBImage)
library(grid)
library(png)

# 2022 version -----------------------------------------------------------------

# Create data ------------------------------------------------------------------

# Code from Will Chase's R implementation of "ratchet" attractors:
# https://www.williamrchase.com/post/strange-attractors-12-months-of-art-february/
# ("Ratchet" approach inspired by Masaru Fujii: https://twitter.com/ozachou_g)

# Prepare data frame
createTrajectory <- function(
  n, x0, y0, t0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6) {
  
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] = a1*sin(f1*y[i-1])^p+a2*cos(f2*y[i-1])^q+a3*sin(f3*t[i-1])^p
    y[i] = a4*cos(f4*x[i-1])^q+a5*sin(f5*y[i-1])^p+a6*cos(f6*t[i-1])^q
    t[i] = t[i-1]+v
    
  }
  
  data.frame(x = x, y = y)
  
}

# Seed value based on date as per ISO 8601 standard
yyyymmdd <- as.numeric(stringr::str_remove_all(Sys.Date(), "-"))

set.seed(yyyymmdd)
a1 <- sample(seq(0.5, 2.5, by = 0.01), 1)
set.seed(yyyymmdd)
a2 <- sample(seq(0.5, 2.3, by = 0.01), 1)
set.seed(yyyymmdd)
a3 <- sample(seq(1, 3, by = 0.01), 1)
set.seed(yyyymmdd)
a4 <- sample(seq(0.5, 1, by = 0.01), 1)
set.seed(yyyymmdd)
a5 <- sample(seq(2.5, 3.5, by = 0.01), 1)
set.seed(yyyymmdd)
a6 <- sample(seq(0.5, 1.5, by = 0.01), 1)
set.seed(yyyymmdd)
f1 <- sample(seq(1, 2.5, by = 0.01), 1)
set.seed(yyyymmdd)
f2 <- sample(seq(1.5, 3, by = 0.01), 1)
set.seed(yyyymmdd)
f3 <- sample(seq(2, 3, by = 0.01), 1)
set.seed(yyyymmdd)
f4 <- sample(seq(0.8, 1.8, by = 0.01), 1)
set.seed(yyyymmdd)
f5 <- sample(seq(1.3, 2.3, by = 0.01), 1)
set.seed(yyyymmdd)
f6 <- sample(seq(0.75, 1.95, by = 0.01), 1)
p <- 6 # changing this from 6 seems to be tricky / unwise!
q <- 15 # changing this from 15 seems to be tricky / unwise!
set.seed(yyyymmdd)
v <- sample(seq(6.5, 10.5, by = 0.01), 1)

df <- createTrajectory(
  5000000, 0, 0, 0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6)

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    colour = "#FFFFFF", shape = 46, alpha = 0.03) +
  theme_void() +
  theme(
    legend.position  = "none",
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    plot.background  = element_rect(fill = "#000000", colour = "#000000"))

# Export bw plot to file
ggsave(
  here::here("img/ingredients/20220122_v2022_bw.png"),
  last_plot(), width = 20, height = 20, units = "cm", dpi = 600)

# Apply colour mapping ---------------------------------------------------------

# Create a colour gradient
possible_colours <- c(
  "#FABC3C", "#FFB238", "#F19143", "#FF773D", "#F55536", "#987284", "#A0CED9",
  "#75B9BE", "#A6D49F", "#254441")

set.seed(yyyymmdd)
selected_colours <- sample(possible_colours, 4, replace = FALSE)

custom_gradient <- (grDevices::colorRampPalette(selected_colours))(256)

# read in the image and convert to greyscale
img <- readImage(here::here("img/ingredients/20220122_v2022_bw.png"))
gray <- channel(img, "gray")

img_col <- colormap(gray, rev(custom_gradient))
# display(img_col, method = "raster")

# Save to PNG
writeImage(
  img_col, here::here("img/ingredients/20220122_v2022_colour_mapped.png"))

# Build final plot -------------------------------------------------------------

# Import fonts from Google
font_add_google("Recursive", "recursive")
showtext_auto()

# Import colour-mapped image
colour_mapped <- readPNG(
  here::here("img/ingredients/20220122_v2022_colour_mapped.png"))
colour_mapped_raster <- rasterGrob(
  colour_mapped, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot() +
  annotation_custom(colour_mapped_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "ISO 8601 (2022 version)",
    subtitle = "#genuary2022 - Day 22",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "recursive", colour = "#fbf0d2"),
    plot.title = element_text(
      size = rel(12), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(6.5), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(6.5), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#1E1E1E", colour = "#1E1E1E"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220122_v2022.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

showtext_auto(FALSE)

# 2023 version -----------------------------------------------------------------

# Create data ------------------------------------------------------------------

# Seed value based on 2023 date as per ISO 8601 standard
v2023 <- 20230122

set.seed(v2023)
a1 <- sample(seq(0.5, 2.5, by = 0.01), 1)

set.seed(v2023)
a2 <- sample(seq(0.5, 2.3, by = 0.01), 1)

set.seed(v2023)
a3 <- sample(seq(1, 3, by = 0.01), 1)

set.seed(v2023)
a4 <- sample(seq(0.5, 1, by = 0.01), 1)

set.seed(v2023)
a5 <- sample(seq(2.5, 3.5, by = 0.01), 1)

set.seed(v2023)
a6 <- sample(seq(0.5, 1.5, by = 0.01), 1)

set.seed(v2023)
f1 <- sample(seq(1, 2.5, by = 0.01), 1)

set.seed(v2023)
f2 <- sample(seq(1.5, 3, by = 0.01), 1)

set.seed(v2023)
f3 <- sample(seq(2, 3, by = 0.01), 1)

set.seed(v2023)
f4 <- sample(seq(0.8, 1.8, by = 0.01), 1)

set.seed(v2023)
f5 <- sample(seq(1.3, 2.3, by = 0.01), 1)

set.seed(v2023)
f6 <- sample(seq(0.75, 1.95, by = 0.01), 1)
p <- 6 # changing this from 6 seems to be tricky / unwise!
q <- 15 # changing this from 15 seems to be tricky / unwise!

set.seed(v2023)
v <- sample(seq(6.5, 10.5, by = 0.01), 1)

df <- createTrajectory(
  5000000, 0, 0, 0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6)

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_point(
    data = df, aes(x, y),
    colour = "#FFFFFF", shape = 46, alpha = 0.03) +
  theme_void() +
  theme(
    legend.position  = "none",
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    plot.background  = element_rect(fill = "#000000", colour = "#000000"))

# Export bw plot to file
ggsave(
  here::here("img/ingredients/20220122_v2023_bw.png"),
  last_plot(), width = 20, height = 20, units = "cm", dpi = 600)

# Apply colour mapping ---------------------------------------------------------

# Create a colour gradient
possible_colours <- c(
  "#FABC3C", "#FFB238", "#F19143", "#FF773D", "#F55536", "#987284", "#A0CED9",
  "#75B9BE", "#A6D49F", "#254441")

set.seed(v2023)
selected_colours <- sample(possible_colours, 4, replace = FALSE)

custom_gradient <- (grDevices::colorRampPalette(selected_colours))(256)

# read in the image and convert to greyscale
img <- readImage(here::here("img/ingredients/20220122_v2023_bw.png"))
gray <- channel(img, "gray")

img_col <- colormap(gray, rev(custom_gradient))
# display(img_col, method = "raster")

# Save to PNG
writeImage(
  img_col, here::here("img/ingredients/20220122_v2023_colour_mapped.png"))

# Build final plot -------------------------------------------------------------

showtext_auto()

# Import colour-mapped image
colour_mapped <- readPNG(
  here::here("img/ingredients/20220122_v2023_colour_mapped.png"))
colour_mapped_raster <- rasterGrob(
  colour_mapped, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot() +
  annotation_custom(colour_mapped_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "ISO 8601 (2023 version)",
    subtitle = "#genuary2022 - Day 22",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "recursive", colour = "#1E1E1E"),
    plot.title = element_text(
      size = rel(12), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(6.5), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(6.5), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#fbf0d2", colour = "#fbf0d2"),
    plot.margin = margin(10, 15, 10, 15, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220122_v2023.png"),
  last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

showtext_auto(FALSE)