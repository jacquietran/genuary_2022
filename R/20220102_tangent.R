# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggfx)
library(ggplot2)

# Set colours
colour_vector <- c(
  "#09280a",
  "#446b04",
  "#e1c1b7",
  "#b05449",
  "#dbb238")

# Create the data
set.seed(2005787)
data_points <- tribble(
  ~x, ~y, ~colour,
  
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1),
  runif(1, min=0, max=10), runif(1, min=0, max=10), sample(colour_vector, 1)
)

# Build the plot
p <- ggplot(
  data_points,
  aes(x = x, y = y, colour = colour))
p <- p + geom_point(size = 50, stroke = 0)
p <- p + scale_colour_identity()