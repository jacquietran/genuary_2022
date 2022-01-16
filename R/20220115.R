# Deal with the Java heap space issue early...before loading any packages.
# The default is ~500MB but set to use X% of system RAM
options(java.parameters = paste("- Xmx", round(memory.limit() * 0.8), "m", sep = ""))

# Load libraries ---------------------------------------------------------------

library(rayshader)
library(raster)
library(sp)
library(scales)

# Import data ------------------------------------------------------------------

# Elevation data downloaded from Derek Watkins' 30-m SRTM tile downloader:
# https://dwtkns.com/srtm30m/
elevation1 <- raster(here::here("misc/20220115/S25E136.hgt"))
elevation2 <- raster(here::here("misc/20220115/S25E137.hgt"))
elevation3 <- raster(here::here("misc/20220115/S26E136.hgt"))
elevation4 <- raster(here::here("misc/20220115/S26E137.hgt"))

# Satellite imagery downloaded from USGS Earth Explorer
munga_thirri_r = raster::raster(here::here("misc/20220115/LC08_L1TP_100078_20201208_20210313_01_T1_B4.tif"))
munga_thirri_g = raster::raster(here::here("misc/20220115/LC08_L1TP_100078_20201208_20210313_01_T1_B3.tif"))
munga_thirri_b = raster::raster(here::here("misc/20220115/LC08_L1TP_100078_20201208_20210313_01_T1_B2.tif"))

# Prepare elevation data -------------------------------------------------------

# Merge elevation data sets
munga_thirri_elevation <- raster::merge(
  elevation1, elevation2, elevation3, elevation4)
munga_thirri_mat <- raster_to_matrix(munga_thirri_elevation)

# Prepare satellite imagery ----------------------------------------------------

# Stack RGB .tif data
munga_thirri_rgb <- sqrt(raster::stack(
  munga_thirri_r, munga_thirri_g, munga_thirri_b))

# Prepare both data sets for layering ------------------------------------------

# Transform elevation data from long/lat to UTM
munga_thirri_elevation_utm <- raster::projectRaster(
  munga_thirri_elevation, crs = crs(munga_thirri_r), method = "bilinear")
#crs(munga_thirri_elevation_utm)

# Cropping
bottom_left = c(y = 136.6699, x = -25.8246)
top_right   = c(y = 137.351253, x = -25.529991)

extent_latlong <- sp::SpatialPoints(
  rbind(bottom_left, top_right),
  proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm <- sp::spTransform(
  extent_latlong, raster::crs(munga_thirri_elevation_utm))

e <- raster::extent(extent_utm)

munga_thirri_rgb_cropped <- raster::crop(munga_thirri_rgb, e)
elevation_cropped <- raster::crop(munga_thirri_elevation_utm, e)

names(munga_thirri_rgb_cropped) <- c("r","g","b")

munga_thirri_r_cropped <- rayshader::raster_to_matrix(
  munga_thirri_rgb_cropped$r)
munga_thirri_g_cropped <- rayshader::raster_to_matrix(
  munga_thirri_rgb_cropped$g)
munga_thirri_b_cropped <- rayshader::raster_to_matrix(
  munga_thirri_rgb_cropped$b)

munga_thirri_el_matrix <- rayshader::raster_to_matrix(elevation_cropped)

munga_thirri_rgb_array <- array(
  0, dim = c(nrow(munga_thirri_r_cropped), ncol(munga_thirri_r_cropped), 3))

munga_thirri_rgb_array[,,1] <- munga_thirri_r_cropped/255 #Red layer
munga_thirri_rgb_array[,,2] <- munga_thirri_g_cropped/255 #Blue layer
munga_thirri_rgb_array[,,3] <- munga_thirri_b_cropped/255 #Green layer

munga_thirri_rgb_array = aperm(munga_thirri_rgb_array, c(2,1,3))
# plot_map(munga_thirri_rgb_array)

munga_thirri_rgb_contrast <- scales::rescale(
  munga_thirri_rgb_array, to = c(0,1))
# plot_map(munga_thirri_rgb_contrast)

# Make map ---------------------------------------------------------------------

munga_thirri_rgb_contrast %>%
  add_overlay(
    sphere_shade(munga_thirri_el_matrix, texture = "bw", zscale = 15),
    alphalayer = 0.25) %>%
  add_shadow(
    texture_shade(
      munga_thirri_el_matrix, detail = 8/10, contrast = 9, brightness = 11),
    0.1) %>%
  save_png(filename = here::here("img/20220115_map_only.png"))

# Shift to {ggplot2} workflow --------------------------------------------------

# Load libraries
library(showtext)
library(ggplot2)
library(grid)
library(png)

# Import fonts from Google
font_add_google("Yeseva One", "yeseva")
showtext_auto()

# Import map image
map <- readPNG(here::here("img/20220115_map_only.png"))
map_raster <- rasterGrob(map, width = unit(1,"npc"), height = unit(1,"npc"))

p <- ggplot() +
  # Map image
  annotation_custom(map_raster, -Inf, Inf, -Inf, Inf) +
  # Plot titles and theming
  labs(
    title = "Munga-Thirri",
    subtitle = "#genuary2022 - Day 15",
    caption = "@jacquietran") +
  theme_void() +
  theme(
    text = element_text(family = "yeseva", colour = "#A2B2BB"),
    plot.title = element_text(
      size = rel(9), margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(4), margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = element_text(
      size = rel(4), margin = margin(10,0,0,0, unit = "pt")),
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export final plot to file ----------------------------------------------------

ggsave(
  here::here("img/20220115.png"),
  last_plot(), width = 10, height = 7, units = "cm", dpi = 600)

showtext_auto(FALSE)