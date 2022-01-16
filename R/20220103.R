# Load libraries ---------------------------------------------------------------

library(Rcpp)
library(dplyr)
library(purrr)
library(showtext)
library(ggplot2)
library(EBImage)
library(paletteer)

# Generate the data ------------------------------------------------------------

# Define C++ function
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(b*y[i-1])+b*cos(a*x[i-1])*cos(b*x[i-1]);
            y[i] = sin(c*x[i-1])*sin(b*x[i-1])-d*cos(b*y[i-1])*cos(a*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-1.7 #making a smaller makes it more "spread out"? but don't make it smaller than -3. making it bigger is bad, just makes lines
b=-.2 #making b smaller seems good... not sure, making it from -3 to 3 doesn't change, but 1 to -1 does...?
c=-2.03
d=9.6

# Create data frame
df=createTrajectory(5000000, 0, 0, a, b, c, d)

# Build plot -------------------------------------------------------------------

# Import fonts from Google
font_add_google("Major Mono Display", "major")

showtext_auto()

# Build plot
p <- ggplot(df, aes(x, y))
p <- p + geom_point(color="#1E1E1E", shape=46, alpha=.01)
p <- p + labs(
  title = "Cassini-Huygens",
  subtitle = "#genuary2022 - Day 3",
  caption = "@jacquietran")
p <- p + theme(
  legend.position  = "none",
  panel.background = element_rect(fill="white"),
  axis.ticks       = element_blank(),
  panel.grid       = element_blank(),
  axis.title       = element_blank(),
  axis.text        = element_blank(),
  text = element_text(family = "major"),
  plot.title = element_text(
    size = rel(16), margin = margin(0,0,5,0, unit = "pt")),
  plot.subtitle = element_text(
    size = rel(9), margin = margin(0,0,10,0, unit = "pt")),
  plot.caption = element_text(
    size = rel(7), margin = margin(10,0,0,0, unit = "pt")),
  plot.margin = margin(10,10,10,10, unit = "pt"))

# Export plot to PNG
ggsave(
  here::here("img/ingredients/20220103_plot_only.png"),
  last_plot(), device = "png",
  width = 6, height = 6, units = "in", dpi = 600)

showtext_auto(FALSE)

# Map colours ------------------------------------------------------------------

# read in the image and convert to greyscale
img <- readImage(
  here::here("img/ingredients/20220103_plot_only.png"))
gray <- channel(img, "gray")

img_col <- colormap(
  gray, paletteer_c("pals::ocean.thermal", n = 256, direction = -1))
# display(img_col, method = "raster")

# Save to PNG
writeImage(img_col, here::here("img/20220103.png"))
