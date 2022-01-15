# Load libraries ---------------------------------------------------------------

library(dplyr)
library(rayshader)
library(raster)
library(showtext)
library(ggplot2)

# Import data ------------------------------------------------------------------

eildon <- raster(here::here("misc/vmelev_dem10m.tif"))

# Crop data
lat_range <- c(-37.217420, -36.996101)
long_range <- c(145.770706, 146.138610)

convert_coords <- function(lat, long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long = long, lat = lat)
  sp::coordinates(data) <- ~ long + lat
  sp::proj4string(data) = from
  # Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

# raster::crs(eildon)

utm_bbox <- convert_coords(
  lat = lat_range,
  long = long_range,
  to = crs(eildon))

# raster::extent(eildon)

extent_zoomed <- raster::extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])

eildon_zoom <- raster::crop(eildon, extent_zoomed)

eildon_zoom_mat <- rayshader::raster_to_matrix(eildon_zoom)

eildon_zoom_small <- resize_matrix(eildon_zoom_mat, 0.25)

#eildon_mat = raster_to_matrix(eildon)
# Resize for prototyping
#eildon_small = resize_matrix(eildon_mat, 0.25)

eildon_zoom_small_water <- eildon_zoom_small
eildon_zoom_small_water[eildon_zoom_small_water < 0] <- 0

# Build plot -------------------------------------------------------------------

eildon_zoom_small %>% 
  sphere_shade() %>%
  add_water(
    detect_water(eildon_zoom_small_water, min_area = 0, zscale = 10),
    color = "dodgerblue") %>%
  plot_map()



  add_overlay(
    sphere_shade(
      eildon_small, texture = "desert", zscale = 4, colorintensity = 5),
    alphalayer = 0.5) %>%
  add_shadow(lamb_shade(eildon_small, zscale = 6), 0) %>%
  add_shadow(ambient_shade(eildon_small), 0) %>%
  add_shadow(
    texture_shade(eildon_small, detail = 8/10, contrast = 9, brightness = 11),
    0.1) %>%
  plot_map()


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------





# Import fonts from Google
font_add_google("", "")
showtext_auto()

p <- ggplot() +
  # Plot titles and theming
  labs(
    title = "",
    subtitle = "#genuary2022 - Day 14",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "", colour = "#4c4c50"),
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
  here::here("img/20220114.png"),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 600)

showtext_auto(FALSE)