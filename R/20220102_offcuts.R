# Load libraries ---------------------------------------------------------------

library(magick)

# Oddish -----------------------------------------------------------------------

# Image sourced from here: https://pokemondb.net/pokedex/oddish

# Read image into R
oddish <- image_read(here::here("misc/oddish.jpg"))

oddish_quantized <- image_quantize(oddish, 6)

# Write dithered image out to file
image_write(
  oddish_quantized,
  path = here::here("img/20220102_offcut01.jpg"),
  format = "jpg")