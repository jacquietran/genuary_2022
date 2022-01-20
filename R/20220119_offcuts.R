# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ambient)
library(showtext)
library(ggplot2)
library(geomtextpath)

# Create data ------------------------------------------------------------------

# Background noise
set.seed(4582)
noisy_bg <- long_grid(
  x = seq(-2.5, 5.5, length.out = 2000),
  y = seq(0, 6,  length.out = 2000)) %>%
  mutate(
    noise = gen_value(x = x, y = y, frequency = 0.1, seed = seed_num))

# Lyrics
df1 <- tibble(
  x = seq(0, 5, length.out = 200),
  y = (10 + (-1.2*x) + (-0.15*(x^2)) + (0.1*(x^3))),
  z = paste(
    "WHEN", "I", "CALL", "YOUR", "NAME,", "I", "CAN", "BE", "SURE",
    sep = "    "))

df2 <- tibble(
  x = seq(0, 5, length.out = 200),
  y = (5 + (1.1*x) + (-0.25*(x^2))),
  z = paste(
    "THAT", "IT", "HURTS", "JUST", "THE", "SAME", "AS", "I", "HURT", "YOU",
    "BEFORE.", sep = "    "))

df3 <- tibble(
  x = seq(0, 5, length.out = 200),
  y = (2.5 + (-1.5*x) + (0.15*(x^2)) + (0.2*(x^3)) + (-0.03*(x^4))),
  z = paste(
    "BEEN", "TRYING", "TO", "FIND", "SOME", "WAY", "TO", "FORGET",
    sep = "    "))

df4 <- tibble(
  x = seq(0, 5, length.out = 200),
  y = (1 + (-1.5*x) + (0.15*(x^2)) + (0.1*(x^3)) + (-0.02*(x^4))),
  z = paste(
    "BUT", "WHEN", "I", "CALL", "YOUR", "NAME,", "I'M", "FILLED", "WITH",
    "REGRET.", sep = "    "))

# Band name
band_name <- tibble(x = 0, y = 12.5, z = "saskwatch")

# Noise overlay
num_of_lines <- 400
dist_between_lines <- 6 / num_of_lines
num_of_points <- 500
noisy_lines <- tibble(
  x = rep(
    seq(-2.5, 5.5, length.out = num_of_points), times = num_of_lines),
  y = rep(
    seq(0 + dist_between_lines, 6, by = dist_between_lines),
    each = num_of_points),
  alpha_value = sample(
    c(0.01, 0.02, 0.04), (num_of_points * num_of_lines), replace = TRUE))

# Build plot -------------------------------------------------------------------

font_add("archivo", here::here("misc/ArchivoNarrow-VariableFont_wght.ttf"))
font_add("prata", here::here("misc/Prata-Regular.ttf"))
showtext_auto()

p <- ggplot()
# Lyrics line 1 - offset shadow
p <- p + geom_textpath(
  data = df1, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.203, spacing = -200, alpha = 0.2)
# Lyrics line 1
p <- p + geom_textpath(
  data = df1, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.2, spacing = -200)
# Lyrics line 2 - offset shadow
p <- p + geom_textpath(
  data = df2, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.404, spacing = -200, alpha = 0.2)
# Lyrics line 2
p <- p + geom_textpath(
  data = df2, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.4, spacing = -200)
# Lyrics line 3 - offset shadow
p <- p + geom_textpath(
  data = df3, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.303, spacing = -200, alpha = 0.2)
# Lyrics line 3
p <- p + geom_textpath(
  data = df3, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.3, spacing = -200)
# Lyrics line 4 - offset shadow
p <- p + geom_textpath(
  data = df4, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.703, spacing = -200, alpha = 0.2)
# Lyrics line 4
p <- p + geom_textpath(
  data = df4, aes(x, y, label = z),
  size = 7, text_only = FALSE, family = "archivo",
  hjust = 0.7, spacing = -200)
# Band name
p <- p + geom_text(
  data = band_name, aes(x, y, label = z), size = 35, family = "prata",
  fontface = "bold", hjust = 0)
p <- p + theme_void()
p <- p + theme(
  plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))





# Background noise
p <- p + geom_raster(data = noisy_bg, aes(x, y, fill = noise))
p <- p + scale_fill_gradient(low = "#131313", high = "#3F3F3F")
# Lyrics line 1
p <- p + geom_textpath(
  data = df1, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.4,
  colour = "#fc608f")
# Lyrics line 2
p <- p + geom_textpath(
  data = df2, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.75,
  colour = "#fdd827")
# Band name SASK-

# Band name -WATCH
p <- p + geom_text(
  data = band_name_watch, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 0, lineheight = 0.8, colour = "#F9F9F9", angle = 90)
# Noisy lines overlay
p <- p + geom_point(
  data = noisy_lines, aes(x = x, y = y, alpha = alpha_value),
  colour = "#2A2A2A", shape = 16, stroke = 0, size = 0.2)
# Lyrics line 1 - slightly transparent overlay
p <- p + geom_textpath(
  data = df1, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.4,
  colour = "#fc608f", alpha = 0.6)
# Lyrics line 2 - slightly transparent overlay
p <- p + geom_textpath(
  data = df2, aes(x, y, label = z),
  size = 3.5, text_only = FALSE, fontface = "bold", hjust = 0.75,
  colour = "#fdd827", alpha = 0.6)
# Band name SASK- - slightly transparent overlay
p <- p + geom_text(
  data = band_name_sask, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 1, lineheight = 0.8, colour = "#F9F9F9", alpha = 0.6)
# Band name -WATCH - slightly transparent overlay
p <- p + geom_text(
  data = band_name_watch, aes(x, y, label = z), size = 13, fontface = "bold",
  hjust = 0, lineheight = 0.8, colour = "#F9F9F9", angle = 90, alpha = 0.6)
p <- p + theme_void()
p <- p + theme(
  legend.position = "none",
  plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/offcuts/20220119_offcut01.png"),
  last_plot(), width = 6, height = 4, units = "cm", dpi = 600)
