# Load libraries ---------------------------------------------------------------

library(dplyr)
library(showtext)
library(ggplot2)

# Create data ------------------------------------------------------------------

initial_seed <- 503

## Trunk and branches #####
trunk_main <- tibble(
  x = 3, y = -2,
  xend = 5, yend = 14)

set.seed(initial_seed)
trunk_branch_left <- tibble(
  x = seq(trunk_main$x, trunk_main$xend, length.out = 100),
  y = seq(trunk_main$y, trunk_main$yend, length.out = 100)) %>%
  filter(y >= 8 & y <= 10) %>%
  mutate(
    selection_var = sample(1:n(), n(), replace = FALSE)) %>%
  filter(selection_var == 10) %>%
  mutate(
    xend = x - 2,
    yend = y + 5) %>%
  select(x, y, xend, yend)

set.seed(initial_seed)
trunk_branch_right <- tibble(
  x = seq(trunk_main$x, trunk_main$xend, length.out = 100),
  y = seq(trunk_main$y, trunk_main$yend, length.out = 100)) %>%
  filter(y >= 6 & y <= 10) %>%
  mutate(
    selection_var = sample(1:n(), n(), replace = FALSE)) %>%
  filter(selection_var == 20) %>%
  mutate(
    xend = x + 4,
    yend = y + 7) %>%
  select(x, y, xend, yend)

## Light through the canopy #####
light_source <- tibble(
  x = c(-1, -1, -1, 1, 1.5), y = c(11, 11.5, 12.5, 13, 13),
  xend = rep(9, times = 5), yend = c(0, 1.5, 2.5, 3, 3),
  segment_num = seq(1, 5, by = 1))

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("", "")
showtext_auto()

p <- ggplot() +
  # Light through the canopy
  ggfx::with_blur(
    geom_segment(
      data = light_source,
      aes(x = x, xend = xend, y = y, yend = yend, group = segment_num),
      colour = "#fffbd1", size = 30, alpha = 0.2),
    sigma = 10) +
  ggfx::with_blur(
    geom_segment(
      data = light_source,
      aes(x = x, xend = xend, y = y, yend = yend, group = segment_num),
      colour = "#fffbd1", size = 3, alpha = 0.3),
    sigma = 5) +
  geom_curve(
    data = trunk_main, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.08, size = 20, colour = "#1C1111") +
  geom_curve(
    data = trunk_main, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.08, size = 20, colour = "#1C1111") +
  geom_curve(
    data = trunk_branch_left, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.1, size = 5, colour = "#1C1111") +
  geom_curve(
    data = trunk_branch_right, aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.2, size = 5, colour = "#1C1111") +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 12), expand = FALSE)


  # Plot titles and theming
  labs(
    title = "",
    subtitle = "#genuary2022 - Day 23",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "", colour = "#"),
    plot.title = element_text(
      size = rel(14), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(8), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(8), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#", colour = "#"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220123.png"),
  last_plot(), width = 8, height = 12, units = "cm", dpi = 200)

showtext_auto(FALSE)